#lang racket
(require web-server/servlet-env
         web-server/http
         net/url-structs
         json
         web-server/http/bindings)

(require "definitions.rkt"
         "reduction-relations/reduction-relations.rkt"
         ;"./dmitry-and-dmitry.rkt"
         "metafunctions.rkt"
         "transpiler.rkt"
         "syntax-checking.rkt")

(define-struct state (red-step prog) #:transparent)

(define future-cache 'uninitialized)
(define trace 'uninitialized)

(define (get-index trace)
  (length trace))

;; initialize-all!: program -> void
;; Purpose: Initializes all of the state variables
(define (initialize-all! prog)
  (define init-state (state "Initialize Program" prog))
  (set! trace (list init-state))
  (set! future-cache '())
  (set-num-query-vars! (num-query-vars prog)))


;; trace->response: [Listof State] -> response
;; Purpose: Creates a response with the current state of the program
(define (trace->response trace)
  (let ([a-state (first trace)]
        [idx (get-index trace)])
    (match-let ([(state red-step prog) a-state])
      (let ([response (hasheq 'stepName red-step
                              'step idx
                              'program (to-json prog))])
        (response/jsexpr response #:mime-type #"application/json; charset=utf-8")))))


;; trace->response/initial: -> response
;; Purpose: Sends the last tree in the history (init-state) with a header indicating it is the initial one
(define (trace->response/initial trace)
  (let ([a-state (first trace)]
        [idx (get-index trace)])
    (match-let ([(state red-step prog) a-state])
      (let ([response (hasheq 'stepName red-step
                              'step idx
                              'program (to-json prog))])
        (response/jsexpr response
                         #:mime-type #"application/json; charset=utf-8"
                         #:headers (list (make-header #"X-Is-Last" #"true")))))))


;; trace/html->response: [Listof State] html -> response
;; Purpose: Send the initial tree and the html embedded program
(define (trace/html->response trace html)
  (let ([a-state (first trace)]
        [idx (get-index trace)])
    (match-let ([(state red-step prog) a-state])
      (let ([response (hasheq 'stepName red-step
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


;; [Term -> [Listof [List String Term]]] ->	Response
(define (make-stepper step-term)
  (lambda ()
    (match future-cache
      [`(,a-state . ,future-cache^)
       (set! future-cache future-cache^)
       (set! trace (cons a-state trace))
       (trace->response trace)]
      ['()
       (match (step-term (state-prog (first trace)))
         ['() (send-end-state)]
         [(cons (list red-step new-program) _)
          (define new-state (state red-step new-program)) ;; form the new state
          (set! trace (cons new-state trace))             ;; add to the trace
          (trace->response trace)])])))   ;; send response


;; step!: -> response
;; Purpose: Applies one reduction step and sends the new JSON data of that tree
(define step! (make-stepper step-once))


;; read-all: port -> ListOf sexpression
;; Purpose: To read the string program into sexpressions
(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()  ;; Stop when EOF is reached
        (cons expr (read-all port)))))


;; init-tree!: request -> response
;; Purpose: To initialize the tree
(define (init-tree! req)
  (define json-data (request-post-data/raw req))                      ;; Get the JSON data from the request
  (define raw-prog (hash-ref (bytes->jsexpr json-data) 'text))        ;; Get the program from that JSON
  (define maybe-syntax-error (check-syntax-capture-error raw-prog))   ;; Check for syntax error w/ syntax-spec
  (cond
    [(non-empty-string? maybe-syntax-error)                           ;; If there was a syntax error
     (response/jsexpr (hasheq 'error maybe-syntax-error) #:code 400)] ;; Return error response
    [else                                                             ;; Else there was no syntax error
     (define sexpr-prog (read-all (open-input-string raw-prog)))      ;; Read the program into sexpressions
     (define-values (model-prog html-prog) (parse-prog sexpr-prog))   ;; Parse the sexpressions
     (define maybe-wf-error (check-well-formed model-prog))           ;; Check if the program is well-formed
     (cond
       [(non-empty-string? maybe-wf-error)                            ;; If the program not well-formed
        (response/jsexpr (hasheq 'error maybe-wf-error) #:code 400)]  ;; Return error respose
       [else                                                          ;; Else the program is well formed. Continue w/ parsing.
        (initialize-all! model-prog)                                  ;; Initialize all state variables
        (trace/html->response trace html-prog)])]))                   ;; Send the initial program and HTML


;; reset!: -> response
;; Purpose: Resets the state of the program to the initial state
(define (reset!)
  (define-values (current-downto-second listof-initial-state) (split-at-right trace 1))
  (set! future-cache (append (reverse current-downto-second) future-cache))
  (set! trace listof-initial-state)
  (trace->response trace))


;; back!: -> response
;; Purpose: Step the programs backwards one step and send that state
(define (back!)
  (match trace
    [`(,current-state ,initial-state)
     (set! future-cache (cons current-state future-cache))
     (set! trace (list initial-state))
     (trace->response/initial trace)]
    [`(,current-state ,prior-state . ,trace^)
     (set! future-cache (cons current-state future-cache))
     (set! trace (cons prior-state trace^))
     (trace->response trace)]))


;; get-path: request -> string
;; Purpose: Gets the path that was pinged as it was on the javascript side
(define (get-path req)
  (string-join (map path/param-path (url-path (request-uri req))) "/"))

;; dispatcher: request -> response
;; Purpose: Maps the input request to an output response
(define (dispatcher req)
  (match (get-path req)
    ["get/next"   (step!)]
    ["post/init"  (init-tree! req)]
    ["post/reset" (reset!)]
    ["post/back"  (back!)]))



(module+ main
  ;; Start the server on port 5000
  (serve/servlet dispatcher
                 #:port 5000
                 #:servlet-regexp #rx""
                 #:listen-ip "0.0.0.0" ; any
                 #:launch-browser? #f)
  )

(module+ test
  (require rackunit)

  (define sample-tree
    '(prog
      ()
      ((∃
        (x:q)
        ((sym "tree1") =? (sym "horse") "u5")
        "f0")
       (state () 0 ()))))

  (define step!/const-tree-output
    (make-stepper (lambda (_) (list (list "foo" sample-tree)))))

  (define test-program
    '(prog ()
           ((∃ (x:q)
               (∃ ()
                  (((((sym "dog1") =? (sym "cat") "u5")
                     ∧ ((sym "bear1") =? x:lion "u6") "c4")
                    ∧ ((sym "dog") =? (sym "cat") "u7") "c3")
                   ∧ ((sym "bear") =? (sym "lion") "u8") "c2") "f1") "f0")
            (state () 0 ()))))

  (test-case "step! works"
             (initialize-all! test-program)
             (check-equal? (state-prog (first trace)) test-program)
             (check-equal? (get-index trace) 1)
             (step!/const-tree-output)
             (check-equal? (state-prog (first trace)) sample-tree)
             (check-equal? (state-prog (second trace)) test-program)
             (check-equal? (get-index trace) 2))

  (test-case "step!,back!,step! works"
             (initialize-all! test-program)
             (step!/const-tree-output)
             (check-equal? (state-prog (first trace)) sample-tree)
             (check-equal? (get-index trace) 2)
             (back!)
             (check-equal? (state-prog (first future-cache)) sample-tree)
             (check-equal? (get-index trace) 1)
             (step!/const-tree-output)
             (check-equal? (state-prog (first trace)) sample-tree)
             (check-equal? (get-index trace) 2))

  (test-case "reset! produces same index and trace as init"
             (initialize-all! test-program)
             (define init-idx (get-index trace))
             (step!/const-tree-output)
             (check-equal? (state-prog (first trace)) sample-tree)
             (check-equal? (get-index trace) 2)
             (reset!)
             (check-equal? (state-prog (first trace)) test-program)
             (check-equal? (length trace) 1)
             (check-equal? (get-index trace) init-idx))

  ;; Assumes input is empty ans stream and search tree is just (goal state)
  (define step!/only-inc-state
    (make-stepper
     (lambda (tr)
       (match-let* ([(list 'prog rels (list g st)) tr]
                    [(list 'state σ count fvs) st])
         (let* ([new-st (list 'state σ (add1 count) fvs)]
                [new-tr (list 'prog rels (list g new-st))])
           (list (list "incr-state" new-tr)))))))

  ;; Assumes input is empty ans stream and search tree is just (goal state)
  (define (query-program->state-ct prog)
    (match prog
      [`(prog ,_ (,_ (state ,_ ,n ,_))) n]))


  (test-case "back! from init is no-op"
             (initialize-all! sample-tree)
             (step!/only-inc-state)
             (define will-be-future-cache (list (first trace)))
             (back!)
             (check-equal? (state-prog (first trace)) sample-tree)
             (check-equal? future-cache will-be-future-cache)
             (check-equal? (get-index trace) 1))


  (test-case "step! 2x, back 2x reasonable"
             (initialize-all! sample-tree)
             (step!/only-inc-state)
             (check-equal? (query-program->state-ct (state-prog (first trace))) 1)
             (check-equal? (get-index trace) 2)
             (step!/only-inc-state)
             (check-equal? (query-program->state-ct (state-prog (first trace))) 2)
             (check-equal? (get-index trace) 3)
             (back!)
             (check-equal? (query-program->state-ct (state-prog (first trace))) 1)
             (check-equal? (get-index trace) 2)
             (back!)
             (check-equal? (query-program->state-ct (state-prog (first trace))) 0)
             (check-equal? (get-index trace) 1))


  (define sample-req
    (make-request
     #"POST"
     (make-url #f #f #f #f #t
               (list (make-path/param "post" empty)
                     (make-path/param "init" empty))
               empty
               #f)
     (list (make-header #"content-type" #"application/json"))
     (delay '())
     (string->bytes/utf-8 "{\"text\":\"(run* (q) (== 'a 'a))\"}")
     "127.0.0.1"
     5000
     "127.0.0.1"))

  (define second-req
    (make-request
     #"GET"
     (make-url #f #f #f #f #t
               (list (make-path/param "get"  empty)
                     (make-path/param "next"  empty))
               empty
               #f)
     (list (make-header #"content-type" #"application/json"))
     (delay '())
     #f
     "127.0.0.1"
     5000
     "127.0.0.1"))



  )
