#lang racket
(require web-server/servlet-env
         web-server/http
         net/url-structs
         net/uri-codec
         json)

(require (prefix-in mmk:    "reduction-relations/reduction-relations.rkt")
         (prefix-in dmitry: "reduction-relations/dmitry-and-dmitry.rkt")
         (prefix-in dfs:    "reduction-relations/dfs.rkt")
         "metafunctions.rkt"
         "transpiler.rkt"
         "syntax-checking.rkt"
         "zipper.rkt")

(provide step! back! reset! init! init-session! 
         make-stepper step step-name 
         session session-zipper session-stepper session-nqv)

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
    (set-session-nqv! s (num-query-vars p))))


;; step->response: step nat nat-> response
;; Purpose: Creates a response with the given step and index
(define (step->response a-step a-idx nqv)
  (match-let ([(step name prog) a-step])
    (let ([response (hasheq 'stepName name
                            'step a-idx
                            'program (to-json prog nqv))])
      (response/jsexpr response #:mime-type #"application/json; charset=utf-8"))))


;; step->response/initial: step nqv -> response
;; Purpose: Sends the first tree in the history (init-state) with a header indicating it is the initial one
(define (step->response/initial a-step nqv)
  (match-let ([(step name prog) a-step])
    (let ([response (hasheq 'stepName name
                            'step 0
                            'program (to-json prog nqv))])
      (response/jsexpr response
                       #:mime-type #"application/json; charset=utf-8"
                       #:headers (list (make-header #"X-Is-Last" #"true"))))))


;; step/html->response: step string string nat -> response
;; Purpose: Send the initial tree and the html embedded program. Sets the cookies for the new session.
(define (step/html/cookie->response a-step tagged-prog session-id nqv)
  (match-let ([(step name prog) a-step])
    (let ([response (hasheq 'stepName name
                            'step 0
                            'program (to-json prog nqv)
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
  (define-values (model-prog html-prog) (parse-prog sexpr-prog))      ;; Parse the sexpressions
  (displayln model-prog) (flush-output)
  ;;(check-well-formed model-prog)                                      ;; Check if the program is well-formed
  (init-session! ses model-prog)                                      ;; Initialize all state variables
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
  (define new-model (hash-ref (bytes->jsexpr json-data) 'model))
  (match new-model
    ["microKanren" (set-session-stepper! ses (make-stepper mmk:step-once))]
    ["dmitry"      (set-session-stepper! ses (make-stepper dmitry:step-once))]
    ["dfs"         (set-session-stepper! ses (make-stepper dfs:step-once))])
  (response/jsexpr (json-null) #:code 200))


;; get-or-create-session-id: req -> string
;; Purpose: Gets the session id from cookies or creates a new one
(define (get-or-create-session-id req)
  (let* ([cookies (map (λ (c) (cons (client-cookie-name c)
                                    (client-cookie-value c)))
                       (request-cookies req))]
         [maybe-session (assoc "session-id" cookies)])
    (if maybe-session
        (cdr maybe-session)
        (symbol->string (gensym 'sess-)))))


;; get-session: string -> session
(define (get-session session-id)
  (hash-ref session-table session-id
            (lambda ()
              (define new-session (session (zipper '() #f '() 0) 
                                           (make-stepper mmk:step-once)
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
