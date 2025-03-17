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

(define-struct state (red-step json prog))

(define current-prog (term (prog 
                            ((r:appendo (x:l x:s x:out)
                                        (((x:l =? empty "g1100545") ∧ (x:s =? x:out "g1100546"))
                                         ∨
                                         (∃ (x:a x:d x:res)
                                            (((x:a : x:d) =? x:l "g1100547")
                                             ∧ (((x:a : x:res) =? x:out "g1100548")
                                                ∧ (r:appendo x:d x:s x:res)))))))
                            ((∃ (x:q x:s x:l) (r:appendo
                                               x:l
                                               x:s
                                               x:q))
                             (state () 0 ())))))

(define init-prog current-prog)
(define init-state (state "Initialize program"
                          (term (to-json (prog->tree ,current-prog)))
                          init-prog))
(define history '())
(define current init-state)
(define index 0)

;; create-response: string json -> response
;; Purpose: Create a reponse structure from the given reduction step and JSON data
(define (create-response red-step json-data)
  (let [(response (string-append "{\"stepName\": \"" red-step "\", "
                                 "\"step\": \"" (number->string index) "\", "
                                 "\"program\": " json-data "}"))]
    (response/jsexpr response
                     #:mime-type #"application/json; charset=utf-8")))


;; send-current-state: _ -> response
;; Purpose: Creates a response with the current state of the program
(define (send-current-state)
  (let* ([red-step (state-red-step current)]
         [json-data (state-json current)])
    (create-response red-step json-data)))


;; step: _ -> response
;; Purpose: Applies one reduction step and sends the new JSON data of that tree
(define (step)
  (set! index (add1 index))
  (if (> index (length history))
      (begin
        (set! history (append history (list current)))                                              ; Update the history
        (let* [(next-step (car (apply-reduction-relation/tag-with-names red (term ,current-prog)))) ; Step once
               (red-step (car next-step))                                                           ; Get the name of the reduction step
               (new-program (cadr next-step))                                                       ; Get the new program
               (tree (term (prog->tree ,new-program)))                                              ; Get the search tree
               (json-data (term (to-json ,tree)))                                                   ; Convert tree to JSON
               (response (create-response red-step json-data))]                                     ; Prepare response
          (set! current-prog new-program)                                                           ; Update the program
          (set! current (state red-step json-data current-prog))                                    ; Update the current variable
          response))                                                                                ; Return the response
      (begin
        (set! current (list-ref history index))
        (send-current-state))))

(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()  ;; Stop when EOF is reached
        (cons expr (read-all port)))))

;; request -> response
;; Purpose: To initialize the tree
(define (init-tree req)
  (define json-data (request-post-data/raw req))
  (define raw-prog (hash-ref (bytes->jsexpr json-data) 'text))
  (define sexpr-prog (read-all (open-input-string raw-prog)))

  (set! current-prog (parse-prog sexpr-prog))

  (set! init-prog current-prog)
  (set! init-state (state "Initialize program"
                            (term (to-json (prog->tree ,current-prog)))
                            init-prog))

  (set! history '())
  (set! current init-state)
  (set! index 0)
  
  (send-current-state))


;; reset: _ -> response
;; Purpose: Resets the state of the program to the initial state
(define (reset)
  (set! index 0)
  (set! current-prog init-prog)
  (set! current init-state)
  (set! history '())
  (send-current-state))

;; send-last-tree: _ -> response
;; Purpose: Sends the last tree in the history (init-state) with a header indicating it is the last
(define (send-last-tree)
  (set! current (first history))
  (let* ([red-step (state-red-step current)]
         [json-data (state-json current)]
         [response (string-append "{\"stepName\": \"" red-step "\", "
                                  "\"step\": \"" (number->string index) "\", "
                                  "\"program\": " json-data "}")])
    (response/jsexpr response
                     #:mime-type #"application/json; charset=utf-8"
                     #:headers (list (make-header #"X-Is-Last" #"true")))))

;; back: _ -> response
;; Purpose: Step the programs backwards one step and send that state
(define (back)
  (set! index (sub1 index))
  (if (= index 0)
      (send-last-tree)
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
    [("get/next") (step)]
    [("post/init") (init-tree req)]
    [("post/reset") (reset)]
    [("post/back") (back)]))


;; Start the server on port 5000
(serve/servlet dispatcher
               #:port 5000
               #:servlet-regexp #rx""
               #:listen-ip "0.0.0.0" ; any
               #:launch-browser? #f)
