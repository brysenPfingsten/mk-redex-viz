#lang racket

(require rackunit
         racket/string
         json
         web-server/http/request-structs
         web-server/http/response-structs
         net/url-structs)

(provide response-body->string
         make-post-request
         make-post-analyze-request
         make-post-model-request
         make-post-init-request
         assert-step-payload-shape
         assert-analyze-payload-shape)

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

(define (make-post-analyze-request src)
  (make-post-request "analyze" (hasheq 'text src)))

(define (make-post-model-request model-id)
  (make-post-request "model" (hasheq 'model model-id)))

(define (make-post-init-request src)
  (make-post-request "init" (hasheq 'text src)))

(define (nonempty-string? v)
  (and (string? v)
       (positive? (string-length (string-trim v)))))

(define (assert-step-payload-shape payload where)
  (check-true (hash? payload) (format "~a: payload must be json object" where))
  (check-true (exact-nonnegative-integer? (hash-ref payload 'step -1))
              (format "~a: missing/non-integer step" where))
  (check-true (nonempty-string? (hash-ref payload 'stepName #f))
              (format "~a: missing/non-string stepName" where))
  (define program-json (hash-ref payload 'program #f))
  (check-true (string? program-json)
              (format "~a: missing/non-string program field" where))
  (define tree (string->jsexpr program-json))
  (check-true (hash? tree)
              (format "~a: program is not a json object" where))
  (define root-name (hash-ref tree 'name #f))
  (check-true (nonempty-string? root-name)
              (format "~a: tree root missing name" where))
  (check-false (equal? root-name "Unknown")
               (format "~a: tree root should not be Unknown" where)))

(define analyze-payload-specs
  (list (list 'requirements list? '())
        (list 'compatibleModelIds list? '())
        (list 'incompatibleModelIds list? '())
        (list 'incompatReasonsByModel hash? #hash())
        (list 'analysisVersion string? "")))

(define (assert-analyze-payload-shape payload where)
  (check-true (hash? payload) (format "~a: payload must be json object" where))
  (check-true (hash-ref payload 'validSyntax #f)
              (format "~a: validSyntax should be true for successful analyze response" where))
  (for ([spec (in-list analyze-payload-specs)])
    (match-define (list key pred default) spec)
    (check-true (pred (hash-ref payload key default))
                (format "~a: malformed ~a field" where key))))
