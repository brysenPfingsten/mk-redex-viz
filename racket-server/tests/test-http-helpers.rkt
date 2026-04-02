#lang racket

(require json
         net/url-structs
         rackunit
         web-server/http/request-structs
         web-server/http/response-structs
         "../src/search-strategy.rkt")

(provide response-body->string
         make-post-request
         make-post-init-request
         make-post-source-convert-request
         default-source-options
         default-search-strategy-options
         assert-step-payload-shape)

(define default-source-options
  (hasheq 'sourceMode "mini"
          'compileProfile (hasheq 'conjAssoc "left"
                                  'disjAssoc "right"
                                  'delayPlacement "relbody")))

(define default-search-strategy-options
  (search-strategy->jsexpr default-search-strategy))

(define (response-body->string response)
  (define out (open-output-string))
  ((response-output response) out)
  (get-output-string out))

(define (make-post-request endpoint payload)
  (make-request
   #"POST"
   (make-url #f #f #f #f #t
             (list (make-path/param "post" empty)
                   (make-path/param endpoint empty))
             empty
             #f)
   (list (make-header #"content-type" #"application/json"))
   (delay '())
   (string->bytes/utf-8 (jsexpr->string payload))
   "127.0.0.1"
   5000
   "127.0.0.1"))

(define (strategy->payload strategy)
  (search-strategy->jsexpr (normalize-search-strategy strategy)))

(define (ensure-init-search-strategy payload [strategy #f])
  (cond
    [strategy (hash-set payload 'searchStrategy (strategy->payload strategy))]
    [(hash-has-key? payload 'searchStrategy) payload]
    [else (hash-set payload 'searchStrategy default-search-strategy-options)]))

(define (make-post-init-request src
                                [payload (hash-set default-source-options 'text src)]
                                #:strategy [strategy #f])
  (make-post-request "init"
                     (ensure-init-search-strategy
                      payload
                      strategy)))

(define (make-post-source-convert-request src
                                          [payload
                                           (hasheq 'text src
                                                   'sourceMode "mini"
                                                   'compileProfile (hash-ref default-source-options 'compileProfile)
                                                   'targetSourceMode "micro")])
  (make-post-request "source-convert"
                     payload))

(define (nonempty-string? v)
  (and (string? v)
       (positive? (string-length (string-trim v)))))

(define (assert-step-payload-shape payload where)
  (check-true (hash? payload) (format "~a: payload must be json object" where))
  (match-define (hash* ['step step]
                       ['stepName step-name]
                       ['program program-json]
                       #:open)
    payload)
  (check-true (exact-nonnegative-integer? step)
              (format "~a: missing/non-integer step" where))
  (check-true (nonempty-string? step-name)
              (format "~a: missing/non-string stepName" where))
  (check-true (string? program-json)
              (format "~a: missing/non-string program field" where))
  (define tree (string->jsexpr program-json))
  (check-true (hash? tree)
              (format "~a: program is not a json object" where))
  (match-define (hash* ['name root-name] #:open) tree)
  (check-true (nonempty-string? root-name)
              (format "~a: tree root missing name" where))
  (check-false (equal? root-name "Unknown")
               (format "~a: tree root should not be Unknown" where)))
