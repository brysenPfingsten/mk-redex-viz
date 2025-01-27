#lang racket
(require web-server/servlet
         web-server/servlet-env
         web-server/http/json
         web-server/http
         json)

;; Define CORS headers as structured 'header' objects
(define cors-headers
  (list
   (header #"Access-Control-Allow-Origin" #"*")
   (header #"Access-Control-Allow-Methods" #"GET, POST, OPTIONS")
   (header #"Access-Control-Allow-Headers" #"Content-Type, Authorization")))

;; API handler that responds with JSON and includes CORS headers
(define (api-handler req)
  (display "Here")
     (response/jsexpr "{\"name\": \"Empty\"}"
   #:mime-type #"application/json; charset=utf-8"
   #:headers cors-headers))

;; OPTIONS request handler for CORS preflight
(define (options-handler req)
  (response/output
   (lambda (out) (display "" out)) ;; Empty body for preflight
   #:code 204 ;; No Content response
   #:headers cors-headers))

;; Dispatcher: Routes requests based on URL and method
(define (dispatcher req)
  (api-handler req)
  #;(cond
    [(equal? (request-method req) 'options) (options-handler req)] ;; Handle preflight
    [(regexp-match #rx"^/api" (url->string (request-uri req))) (api-handler req)]
    [else (response/xexpr '(html (body "Fallback Response")))]))

;; Start the Racket web server on port 5000
(serve/servlet dispatcher #:port 5000 #:servlet-regexp #rx"")
