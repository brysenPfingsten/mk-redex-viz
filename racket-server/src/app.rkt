#lang racket
(require web-server/servlet-env
         web-server/http
         net/url-structs
         json)

(require (prefix-in mmk:    "reduction-relations/reduction-relations.rkt")
         (prefix-in dmitry: "reduction-relations/dmitry-and-dmitry.rkt")
         (prefix-in dfs:    "reduction-relations/dfs.rkt")
         (prefix-in no-rr:  "reduction-relations/no-railway.rkt")
         "metafunctions.rkt"
         "transpiler.rkt"
         "syntax-checking.rkt"
         "zipper.rkt")

(provide step! back! reset! init! initialize-all! 
         get-index make-stepper step step-name)

(define-struct step (name prog) #:transparent)
;; zipper of steps
(define state (zipper '() #f '() 0))
(define state-init! zipper-init!)
(define state-add!  zipper-add!)
(define state-next! zipper-next!)
(define state-back! zipper-back!)
(define state-curr  zipper-curr)
(define get-index   zipper-idx)


;; initialize-all!: state program -> void
;; Purpose: Initializes all of the state variables
(define (initialize-all! s prog)
  (state-init! s)
  (state-add! s (step "Initialize Program" prog))
  (set-num-query-vars! (num-query-vars prog)))


;; state->response: state -> response
;; Purpose: Creates a response with the current state of the program
(define (state->response state)
  (let ([a-step (state-curr state)]
        [idx (get-index state)])
    (match-let ([(step name prog) a-step])
      (let ([response (hasheq 'stepName name
                              'step idx
                              'program (to-json prog))])
        (response/jsexpr response #:mime-type #"application/json; charset=utf-8")))))


;; state->response/initial: state -> response
;; Purpose: Sends the last tree in the history (init-state) with a header indicating it is the initial one
(define (state->response/initial state)
  (let ([a-step (state-curr state)]
        [idx (get-index state)])
    (match-let ([(step name prog) a-step])
      (let ([response (hasheq 'stepName name
                              'step idx
                              'program (to-json prog))])
        (response/jsexpr response
                         #:mime-type #"application/json; charset=utf-8"
                         #:headers (list (make-header #"X-Is-Last" #"true")))))))


;; state/html->response: state html -> response
;; Purpose: Send the initial tree and the html embedded program
(define (state/html->response state html)
  (let ([a-step (state-curr state)]
        [idx (get-index state)])
    (match-let ([(step name prog) a-step])
      (let ([response (hasheq 'stepName name
                              'step idx
                              'program (to-json prog)
                              'htmlGuids html)])
        (response/jsexpr response
                         #:mime-type #"application/json; charset=utf-8")))))


;; send-end-state: -> response
;; Purpose: Send a response with a header indicating the program can no longer step
(define (send-end-state)
  (response/jsexpr (json-null)
                   #:mime-type #"application/json; charset=utf-8"
                   #:headers (list (make-header #"X-Done" #"true"))))


;; [Term -> [Listof [List String Term]]] -> [state -> Response]
(define (make-stepper step-term)
  (lambda (state)
    (let ([maybe-next (state-next! state)])
      (if (false? maybe-next) ;; Nothing in forward cache
          (match (step-term (step-prog (state-curr state)))
            ['() (send-end-state)]
            [(cons (list name new-prog) _)
             (define new-step (step name new-prog))
             (state-add! state new-step)
             (state->response state)])
          (state->response state)))))


(define stepper (make-stepper mmk:step-once))

;; step!: state [state -> response] -> response
;; Purpose: Applies one reduction step and sends the new JSON data of that tree
(define (step! a-state a-stepper) (a-stepper a-state))


;; read-all: port -> ListOf sexpression
;; Purpose: To read the string program into sexpressions
(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()  ;; Stop when EOF is reached
        (cons expr (read-all port)))))


;; init!: state request -> response
;; Purpose: To initialize the tree
(define (init! state req)
  (define json-data (request-post-data/raw req))                      ;; Get the JSON data from the request
  (define raw-prog (hash-ref (bytes->jsexpr json-data) 'text))        ;; Get the program from that JSON
  (check-syntax-capture-error raw-prog)                               ;; Check for syntax errors
  (define sexpr-prog (read-all (open-input-string raw-prog)))         ;; Read the program into sexpressions
  (define-values (model-prog html-prog) (parse-prog sexpr-prog))      ;; Parse the sexpressions
  (check-well-formed model-prog)                                      ;; Check if the program is well-formed
  (initialize-all! state model-prog)                                  ;; Initialize all state variables
  (state/html->response state html-prog))                             ;; Send the initial program and HTML


;; reset!: state -> response
;; Purpose: Resets the given state to the initial state
(define (reset! state)
  (match state
    [(zipper prev _ _ _) #:when (cons? prev)
     (let ([init-prog (last prev)])
       (state-init! state)
       (state-add! state init-prog)
       (state->response state))]
    [_
     (state->response state)]))


;; back!: state -> response
;; Purpose: Sends the previous step if it exists and updates the state.
;;          If there is no prevous step, sends the current step w/ header.
(define (back! state)
  (match (state-back! state)
    [(initial _) (state->response/initial state)]
    [(step _ _) (state->response state)]
    [#f (state->response state)]))


;; switch-model!: request -> response
;; Purpose: Switches the model that is being used to step with
(define (switch-model! req)
  (define json-data (request-post-data/raw req))
  (define new-model (hash-ref (bytes->jsexpr json-data) 'model))
  (match new-model
    ["microKanren" (set! stepper (make-stepper mmk:step-once))]
    ["dmitry"      (set! stepper (make-stepper dmitry:step-once))]
    ["dfs"         (set! stepper (make-stepper dfs:step-once))]
    ["no-rr"       (set! stepper (make-stepper no-rr:step-once))])
  (response/jsexpr (json-null) #:code 200))


;; get-path: request -> string
;; Purpose: Gets the path that was pinged as it was on the javascript side
(define (get-path req)
  (string-join (map path/param-path (url-path (request-uri req))) "/"))

;; dispatcher: request -> response
;; Purpose: Maps the input request to an output response
(define (dispatcher req)
  (match (get-path req)
    ["get/next"   (step! state stepper)]
    ["post/init"  (init! state req)]
    ["post/reset" (reset! state)]
    ["post/back"  (back! state)]
    ["post/model" (switch-model! req)]))

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
