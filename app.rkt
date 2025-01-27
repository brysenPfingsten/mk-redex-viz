#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/http/json
         web-server/http
         json
         redex
         redex/reduction-semantics)

(require "definitions.rkt" "reduction-relations.rkt")

(define program (term (prog ((r:appendo (x:l x:s x:out)
                           (((x:l =? empty) ∧ (x:s =? x:out))
                            ∨
                            (∃ (x:a x:d x:res)
                               (((x:a : x:d) =? x:l)
                                ∧
                                (((x:a : x:res) =? x:out)
                                 ∧
                                 (r:appendo x:d x:s x:res)))))))
               ((∃ (x:l x:s) (r:appendo x:l x:s ("dog" : ("cat" : ("bear" : empty)))))
                           (state () 0)))))

;; Define CORS headers as structured 'header' objects
(define cors-headers
  (list
   (header #"Access-Control-Allow-Origin" #"*")
   (header #"Access-Control-Allow-Methods" #"GET, POST, OPTIONS")
   (header #"Access-Control-Allow-Headers" #"Content-Type, Authorization")))

;; API handler that responds with JSON and includes CORS headers
(define (api-handler req)
  (begin
  (define search-tree (term (prog->tree ,program)))
  (define result (term (to-json ,search-tree)))
  (set! program (car (apply-reduction-relation red (term ,program))))
  (display program)
  (newline)
  (newline)
     (response/jsexpr result
   #:mime-type #"application/json; charset=utf-8"
   #:headers cors-headers)))

;; OPTIONS request handler for CORS preflight
(define (options-handler req)
  (response/output
   (lambda (out) (display "" out)) ;; Empty body for preflight
   #:code 204 ;; No Content response
   #:headers cors-headers))

;; Dispatcher: Routes requests based on URL and method
(define (dispatcher req)
  (display req)
  (cond
    [(equal? (request-method req) #"OPTIONS") (options-handler req)] ;; Handle preflight
    [else (api-handler req)]))

(term (to-json (("hello" =? "hello") (state () 0))))

;; Start the Racket web server on port 5000
(serve/servlet dispatcher #:port 5000 #:servlet-regexp #rx"" #:launch-browser? #f)
