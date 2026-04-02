#lang racket
(require web-server/servlet-env
         web-server/http
         net/url-structs
         net/uri-codec
         json)

(require "metafunctions.rkt"
         "transpiler.rkt"
         "syntax-checking.rkt"
         "zipper.rkt"
         "model-registry.rkt"
         "legacy-variant-adapter.rkt")

(provide step! back! reset! init! init-session! 
         make-stepper step step-name 
         session session-zipper session-stepper session-nqv
         switch-model! list-models!)

(define-struct step (name prog) #:transparent)
(define-struct session 
  ([zipper #:mutable] 
   [stepper #:mutable]
   [nqv #:mutable])
  #:transparent)
(define session-table (make-hash))


;; init-session!: session program -> void
;; Purpose: Initializes all of the state variables
(define (init-session! s p)
  (let ([z (session-zipper s)])
    (zipper-init! z)
    (zipper-add! z (step "Initialize Program" p))
    (set-session-nqv! s (num-query-vars (canonical-config->legacy-program p)))))


;; program->display-prog: program -> legacy-program
;; Purpose: Keep existing JSON/transpiler view logic while stepping L4 configs.
(define (program->display-prog prog)
  (canonical-config->legacy-program prog))


;; step->response: step nat nat-> response
;; Purpose: Creates a response with the given step and index
(define (step->response a-step a-idx nqv)
  (match-let ([(step name prog) a-step])
    (let ([response (hasheq 'stepName name
                            'step a-idx
                            'program (to-json (program->display-prog prog) nqv))])
      (response/jsexpr response #:mime-type #"application/json; charset=utf-8"))))


;; step->response/initial: step nqv -> response
;; Purpose: Sends the first tree in the history (init-state) with a header indicating it is the initial one
(define (step->response/initial a-step nqv)
  (match-let ([(step name prog) a-step])
    (let ([response (hasheq 'stepName name
                            'step 0
                            'program (to-json (program->display-prog prog) nqv))])
      (response/jsexpr response
                       #:mime-type #"application/json; charset=utf-8"
                       #:headers (list (make-header #"X-Is-Last" #"true"))))))


;; step/html->response: step string string nat -> response
;; Purpose: Send the initial tree and the html embedded program. Sets the cookies for the new session.
(define (step/html/cookie->response a-step tagged-prog session-id nqv)
  (match-let ([(step name prog) a-step])
    (let ([response (hasheq 'stepName name
                            'step 0
                            'program (to-json (program->display-prog prog) nqv)
                            'htmlGuids tagged-prog)])
      (response/jsexpr response
                       #:mime-type #"application/json; charset=utf-8"
                       #:headers (list (make-header #"Set-Cookie"
                                                    (string->bytes/utf-8 (format "session-id=~a; Path=/; SameSite=Lax" session-id))))))))


;; send-end-step: -> response
;; Purpose: Send a response with a header indicating the program can no longer step
(define (send-end-step)
  (response/jsexpr (json-null)
                   #:mime-type #"application/json; charset=utf-8"
                   #:headers (list (make-header #"X-Done" #"true"))))


;; [Term Nat -> [Listof [List String Term]]] -> [zipper -> response]
(define (make-stepper step-term)
  (lambda (z nqv)
    (let ([maybe-next (zipper-next! z)]
          [idx (zipper-idx z)])
      (if (false? maybe-next) ;; Nothing in forward cache
          (match (step-term (step-prog (zipper-curr z)))
            ['() (send-end-step)]
            [(cons (list name new-prog) _)
             (define new-step (step name new-prog))
             (zipper-add! z new-step)
             (step->response new-step (add1 idx) nqv)])
          (step->response maybe-next idx nqv)))))


;; step!: session -> response
;; Purpose: Applies one reduction step and sends the new JSON data of that tree
(define (step! ses) 
  (match-let ([(session zip step nqv) ses])
    (step zip nqv)))


;; read-all: port -> ListOf sexpression
;; Purpose: To read the string program into sexpressions
(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()  ;; Stop when EOF is reached
        (cons expr (read-all port)))))


;; init!: session request string -> response
;; Purpose: To initialize the given session
(define (init! ses req ses-id)
  (define json-data (request-post-data/raw req))                      ;; Get the JSON data from the request
  (define raw-prog (hash-ref (bytes->jsexpr json-data) 'text))        ;; Get the program from that JSON
  (check-syntax-capture-error raw-prog)                               ;; Check for syntax errors
  (define sexpr-prog (read-all (open-input-string raw-prog)))         ;; Read the program into sexpressions
  (define-values (legacy-prog html-prog) (parse-prog sexpr-prog))      ;; Parse the sexpressions
  (check-well-formed legacy-prog)                                      ;; Legacy parser/wf gate
  (define model-prog (legacy-program->canonical-config legacy-prog))   ;; Target syntax migration
  (unless (canonical-config? model-prog)
    (error 'init! (format "transpiler produced a program outside canonical target ~a"
                          canonical-target-id)))
  (init-session! ses model-prog)                                       ;; Initialize all state variables
  (match-define (session zip _ nqv) ses)                              ;; Get zipper and number query vars
  (define init-step (zipper-curr zip))                                ;; Get the initial program
  (step/html/cookie->response init-step html-prog ses-id nqv))        ;; Send the initial program and HTML


;; reset!: session hash string -> response
;; Purpose: Resets the given session to the initial state of its program
(define (reset! ses ses-table ses-id)
  (match-let ([(session zip _ nqv) ses])
    (define response (match zip
                       [(zipper prev _ _ _) 
                        #:when (cons? prev)
                        (let ([init-prog (last prev)])
                          (zipper-init! zip)
                          (zipper-add! zip init-prog)
                          (step->response/initial (zipper-curr zip) nqv))]
                       [(zipper _ curr _ _) 
                        #:when (step? curr)
                        (step->response/initial curr nqv)]))
    (hash-remove! ses-table ses-id)
    response))


;; back!: session -> response
;; Purpose: Sends the previous step if it exists and updates the state.
;;          If there is no prevous step, sends the current step w/ header.
(define (back! ses)
  (let* ([zipper (session-zipper ses)]
         [nqv (session-nqv ses)]
         [maybe-back (zipper-back! zipper)]
         [idx (zipper-idx zipper)])
    (match maybe-back
      [(initial s) (step->response/initial s nqv)]
      [s #:when (step? s) (step->response s idx nqv)]
      [_ (step->response maybe-back idx nqv)])))


;; switch-model!: session request -> response
;; Purpose: Switches the model that is being used to step with
(define (switch-model! ses req)
  (define json-data (request-post-data/raw req))
  (define new-model (hash-ref (bytes->jsexpr json-data) 'model #f))
  (define maybe-step-once (lookup-model-step-once new-model))
  (if maybe-step-once
      (begin
        (set-session-stepper! ses (make-stepper maybe-step-once))
        (response/jsexpr (hasheq 'model new-model) #:code 200))
      (response/jsexpr (hasheq 'error (format "Unknown model: ~a" new-model))
                       #:code 400)))


;; list-models!: -> response
;; Purpose: Returns known backend model ids and metadata for UI dispatch.
(define (list-models!)
  (response/jsexpr
   (for/list ([spec (in-list all-model-specs)])
     (model-spec->jsexpr spec))
   #:mime-type #"application/json; charset=utf-8"
   #:code 200))


;; get-or-create-session-id: req -> string
;; Purpose: Gets the session id from cookies or creates a new one
(define (cookie-field->string v)
  (cond
    [(string? v) v]
    [(bytes? v) (bytes->string/utf-8 v)]
    [(symbol? v) (symbol->string v)]
    [else (format "~a" v)]))

(define (get-or-create-session-id req)
  (or
   (for/first ([c (in-list (request-cookies req))]
               #:when (string=? (cookie-field->string (client-cookie-name c))
                                "session-id"))
     (cookie-field->string (client-cookie-value c)))
   (symbol->string (gensym 'sess-))))


;; get-session: string -> session
(define (get-session session-id)
  (hash-ref session-table session-id
            (lambda ()
              (define default-step-once (lookup-model-step-once default-model-id))
              (when (not default-step-once)
                (error 'get-session
                       (format "No default model found for id: ~a" default-model-id)))
              (define new-session (session (zipper '() #f '() 0) 
                                           (make-stepper default-step-once)
                                           1))
              (hash-set! session-table session-id new-session)
              new-session)))


;; get-path: request -> string
;; Purpose: Gets the path that was pinged as it was on the javascript side
(define (get-path req)
  (string-join (map path/param-path (url-path (request-uri req))) "/"))

;; dispatcher: request -> response
;; Purpose: Maps the input request to an output response
(define (dispatcher req)
  (let* ([session-id (get-or-create-session-id req)]
         [session (get-session session-id)])
    (match (get-path req)
      ["get/models" (list-models!)]
      ["get/next"   (step! session)]
      ["post/init"  (init! session req session-id)]
      ["post/reset" (reset! session session-table session-id)]
      ["post/back"  (back! session)]
      ["post/model" (switch-model! session req)])))

(define (handled-dispatcher req)
  (with-handlers
      [(exn:fail?
        (λ (e)
          (response/jsexpr (hasheq 'error (exn-message e)) #:code 400)))]
    (dispatcher req)))



(module+ main
  ;; Start the server on port 5000
  (serve/servlet handled-dispatcher
                 #:port 5000
                 #:servlet-regexp #rx""
                 #:listen-ip "0.0.0.0" ; any
                 #:launch-browser? #f)
  )
