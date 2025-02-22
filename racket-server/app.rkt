#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/http/json
         web-server/http
         net/url-structs
         json
         redex
         redex/reduction-semantics)

(require "definitions.rkt"
         "reduction-relations.rkt"
         "metafunctions.rkt")

(define program (term (prog
                       ((r:appendo
                         (x:l x:s x:out)
                         (((x:l =? empty "g1100545") ∧ (x:s«2» =? x:out "g1100546"))
                          ∨
                          (∃ (x:a x:d x:res) (((x:a : x:d) =? x:l "g1100547") ∧ (((x:a : x:res) =? x:out "g1100548") ∧ (r:appendo x:d x:s x:res)))))))
                       ((∃ (x:q) (r:appendo ("cat" : ("dog" : empty)) ("bear" : ("lion" : empty)) x:q)) (state () 0 ())))))

(define history (list program))

#; (define program (term (add-tags (prog () ((∃ (x:q) (x:q =? "hello")) (state () 0 ()))))))

(define init-program program)

;; create-response: json -> response
;; Purpose: Create a reponse structure from the given JSON data
(define (create-response response)
  (response/jsexpr response
                   #:mime-type #"application/json; charset=utf-8"))


;; send-current-state: _ -> response
;; Purpose: Creates a response with the current state of the program
(define (send-current-state)
  (let* ([tree (term (prog->tree ,program))]
         [tree-json (term (to-json ,tree))])
    (create-response tree-json)))


;; step: _ -> response
;; Purpose: Applies one reduction step and sends the new JSON data of that tree
(define (step)
  (set! history (cons program history)) ; Update the history
  (let* [(new-program (car (apply-reduction-relation red (term ,program)))) ; Step once
         (tree (term (prog->tree ,new-program)))  ; Get the search tree
         (tree-json (term (to-json ,tree)))   ; Convert tree to JSON
         (response (create-response tree-json))]     ; Prepare response
    (display tree-json)

    (set! program new-program)  ; Update the program
    
    ; (display program) (newline) (newline) (newline) ; Display the program
    
    response))  ; Return the response

;; _ -> response
;; Purpose: To initialize the tree
;; Note: this will change once the interpreter is functional
(define (init-tree)
  (reset))


;; reset: _ -> response
;; Purpose: Resets the state of the program to the initial state
(define (reset)
  (set! program init-program)
  (set! history (list program))
  (send-current-state))

;; send-last-tree: _ -> response
;; Purpose: Sends the last tree in the history (init-program) with a header indicating it is the last
(define (send-last-tree)
  (set! program (first history))
  (let* ([tree (term (prog->tree ,program))]
         [tree-json (term (to-json ,tree))])
    (response/jsexpr tree-json
                     #:mime-type #"application/json; charset=utf-8"
                     #:headers (list (make-header #"X-Is-Last" #"true")))))

;; back: _ -> response
;; Purpose: Step the programs backwards one step and send that state
(define (back)
  (if (= (length history) 1)
      (send-last-tree)
      (begin
        (set! program (car history))
        (set! history (cdr history))
        (send-current-state))))


;; get-path: request -> string
;; Purpose: Gets the path that was pinged as it was on the javascript side
(define (get-path req)
  (string-join (map path/param-path (url-path (request-uri req))) "/"))


;; dispatcher: request -> response
;; Purpose: Maps the input request to an output response
(define (dispatcher req)
  (newline) (newline) (newline)
  ; (display req)
  (case (get-path req)
    [("get/next") (step)]
    [("get/init") (init-tree)]
    [("post/reset") (reset)]
    [("post/back") (back)]))


;; Start the server on port 5000
(serve/servlet dispatcher
               #:port 5000
               #:servlet-regexp #rx""
               #:listen-ip "0.0.0.0" ; any
               #:launch-browser? #f)
