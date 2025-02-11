#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/http/json
         web-server/http
         json
         redex
         redex/reduction-semantics)

(require "definitions.rkt" "reduction-relations.rkt" "metafunctions.rkt")

(define program (term (add-tags (prog ((r:appendo (x:l x:s x:out)
                                        (((x:l =? empty) ∧ (x:s =? x:out))
                                         ∨
                                         (∃ (x:a x:d x:res)
                                            (((x:a : x:d) =? x:l)
                                             ∧
                                             (((x:a : x:res) =? x:out)
                                              ∧
                                              (r:appendo x:d x:s x:res)))))))
                            ((∃ (x:l x:s) (r:appendo x:l x:s ("dog" : ("cat" : ("bear" : empty)))))
                             (state () 0 ()))))))

(define init-program program)


;; Define CORS headers
(define cors-headers
  (list
   (header #"Access-Control-Allow-Origin" #"*")
   (header #"Access-Control-Allow-Methods" #"GET, POST, OPTIONS")
   (header #"Access-Control-Allow-Headers" #"Content-Type, Authorization")))

;; API handler that responds with JSON and includes CORS headers
(define (get-handler req)
  (begin
    (define search-tree (term (prog->tree ,program))) ; Get the search tree
    (define result (term (to-json ,search-tree))) ; Convert it to JSON
    (set! program (car (apply-reduction-relation red (term ,program)))) ; Step once
    (display program) (newline) (newline) ; Display the program
    (response/jsexpr result ; Send response
                     #:mime-type #"application/json; charset=utf-8"
                     #:headers cors-headers)))

;; OPTIONS request handler for CORS preflight
(define (options-handler req)
  (response/output
   (lambda (out) (display "" out)) 
   #:code 204 ;; No Content response
   #:headers cors-headers))

;; POST requester handler for resetting the state
(define (post-handler req)
  (set! program init-program)
  (get-handler req))

;; Dispatcher: Routes requests based on URL and method
(define (dispatcher req)
  (display req)
  (cond
    [(equal? (request-method req) #"OPTIONS") (options-handler req)] ;; Handle preflight
    [(equal? (request-method req) #"GET") (get-handler req)]
    [(equal? (request-method req) #"POST") (post-handler req)]
    [else #f]))

;; Start the server on port 5000
(serve/servlet dispatcher #:port 5000 #:servlet-regexp #rx"" #:listen-ip "0.0.0.0" #:launch-browser? #f)
