#lang racket
(require web-server/servlet-env
         web-server/http
         net/url-structs
         json
         redex/reduction-semantics
         web-server/http/bindings)

(require "definitions.rkt"
         "reduction-relations.rkt"
         "metafunctions.rkt"
         "transpiler.rkt")

(define-struct state (red-step json prog) #:transparent)

(define current-prog 'uninitialized)
(define init-prog 'uninitialized)
(define init-state 'uninitialized)
(define current 'uninitialized)
(define history 'uninitialized)
(define index 'uninitialized)

;; initialize-all!: program -> void
;; Purpose: Initializes all of the state variables
(define (initialize-all! prog)
  (set! current-prog prog)
  (set! init-prog prog)
  (set! init-state (state "Initialize Program"
                          (to-json prog)
                          prog))
  (set! current init-state)
  (set! history (list init-state))
  (set! index 0))

;; create-response: string nat json -> response
;; Purpose: Create a reponse structure from the given reduction step and JSON data
(define (create-response red-step step-num json-data)
  (let [(response (hasheq 'stepName red-step
                          'step step-num
                          'program json-data))]
    (response/jsexpr response
                     #:mime-type #"application/json; charset=utf-8")))


;; send-current-state: -> response
;; Purpose: Creates a response with the current state of the program
(define (send-current-state)
  (let* ([red-step (state-red-step current)]
         [json-data (state-json current)])
    (create-response red-step index json-data)))

;; send-end-state: -> response
;; Purpose: Send a response with a header indicating the program can no longer step
(define (send-end-state)
  (response/jsexpr (json-null)
                   #:mime-type #"application/json; charset=utf-8"
                   #:headers (list (make-header #"X-Done" #"true"))))


;; step: -> response
;; Purpose: Applies one reduction step and sends the new JSON data of that tree
(define (step!)
  (set! index (add1 index))
  (if (< index (length history))
      (begin
        (set! current (list-ref history index))
        (send-current-state))
      (let ([stepped (apply-reduction-relation/tag-with-names red (term ,current-prog))])
        (if (empty? stepped)
            (begin (set! index (sub1 index)) (send-end-state))
            (let* ([next-step   (car stepped)]         ; Get the program and reduction step
                   [red-step    (car next-step)]       ; Get the name of the reduction step
                   [new-program (cadr next-step)]       ; Get the new program
                   [json-data   (to-json new-program)]  ; Convert tree to JSON
                   [response    (create-response red-step index json-data)]) ; Prepare response
              (set! current-prog new-program)         ; Update the program
              (set! current (state red-step json-data current-prog)) ; Update the current variable
              (set! history (append history (list current))) ; Update the history
              response)))))


  ;; read-all: port -> ListOf sexpression
  ;; Purpose: To read the string program into sexpressions
  (define (read-all port)
    (let ([expr (read port)])
      (if (eof-object? expr)
          '()  ;; Stop when EOF is reached
          (cons expr (read-all port)))))

  ;; send-tree-and-html: json string
  ;; Purpose: Send the initial tree and the html embedded program
  (define (send-tree-and-html tree html)
    (let ([response (hasheq 'stepName "Initialize program"
                            'step 0
                            'program tree
                            'htmlGuids html)])
      (response/jsexpr response
                       #:mime-type #"application/json; charset=utf-8")))

  ;; init-tree!: request -> response
  ;; Purpose: To initialize the tree
  (define (init-tree! req)
    (define json-data (request-post-data/raw req))                    ;; Get the JSON data from the request
    (define raw-prog (hash-ref (bytes->jsexpr json-data) 'text))      ;; Get the program from that JSON
    (define sexpr-prog (read-all (open-input-string raw-prog)))       ;; Read the program into sexpressions
    (define parsed (parse-prog sexpr-prog))                           ;; Parse the sexpressions
    (initialize-all! (car parsed))                                    ;; Initialize all state variables with the model program
    (define html-prog (cdr parsed))                                   ;; Get the HTML embedded program
    (send-tree-and-html (state-json current) html-prog))              ;; Send the initial program and HTML embedded program back to the JS side


  ;; reset!: -> response
  ;; Purpose: Resets the state of the program to the initial state
  (define (reset!)
    (initialize-all! init-prog)
    (send-current-state))

  ;; send-last-tree!: -> response
  ;; Purpose: Sends the last tree in the history (init-state) with a header indicating it is the last
  (define (send-last-tree!)
    (set! current (first history))
    (let* ([red-step (state-red-step current)]
           [json-data (state-json current)]
           [response (hasheq 'stepName red-step
                             'step index
                             'program json-data)])
      (response/jsexpr response
                       #:mime-type #"application/json; charset=utf-8"
                       #:headers (list (make-header #"X-Is-Last" #"true")))))

  ;; back!: -> response
  ;; Purpose: Step the programs backwards one step and send that state
  (define (back!)
    (set! index (sub1 index))
    (if (= index 0)
        (send-last-tree!)
        (begin
          (set! current (list-ref history index))
          (send-current-state))))


  ;; get-path: request -> string
  ;; Purpose: Gets the path that was pinged as it was on the javascript side
  (define (get-path req)
    (string-join (map path/param-path (url-path (request-uri req))) "/"))


  ;; dispatcher: request -> response
  ;; Purpose: Maps the input request to an output response
  (define (dispatcher req)
    (case (get-path req)
      [("get/next") (step!)]
      [("post/init") (init-tree! req)]
      [("post/reset") (reset!)]
      [("post/back") (back!)]))


  ;; Start the server on port 5000
  (serve/servlet dispatcher
                 #:port 5000
                 #:servlet-regexp #rx""
                 #:listen-ip "0.0.0.0" ; any
                 #:launch-browser? #f)
  