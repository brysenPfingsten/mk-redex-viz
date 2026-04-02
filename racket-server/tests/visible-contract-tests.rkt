#lang racket

(require json
         racket/list
         racket/runtime-path
         rackunit
         rackunit/text-ui
         "../src/app.rkt"
         "../src/search-strategy.rkt"
         "./frontier-observable-support.rkt"
         "./example-compat-tests.rkt"
         "./test-http-helpers.rkt")

(provide VISIBLE-CONTRACTS)

(define-runtime-path VISIBLE-CONTRACT-PATH
  "../../contracts/visible-node-contract.json")

(define VISIBLE-STEP-CAP 10)

(define REPRESENTATIVE-VISIBLE-LABELS
  '("fives/fours"
    "same"
    "hoist witness"
    "fresh branch disj"))

(define/match (strategy-label strategy)
  [((search-strategy hoist scheduler))
   (format "~a/~a" hoist scheduler)])

(define (read-visible-contract)
  (call-with-input-file VISIBLE-CONTRACT-PATH read-json))

(define (json-node-names node [acc '()])
  (match node
    [(? hash? h)
     (define acc^
       (match (hash-ref h 'name #f)
         [(? string? nm) (cons nm acc)]
         [_ acc]))
     (for/fold ([names acc^]) ([value (in-hash-values h)])
       (json-node-names value names))]
    [(list xs ...)
     (for/fold ([names acc]) ([x (in-list xs)])
       (json-node-names x names))]
    [_ acc]))

(define (response->payload response)
  (string->jsexpr (response-body->string response)))

(define (payload->program payload)
  (string->jsexpr (hash-ref payload 'program)))

(define (trace-programs src [strategy default-search-strategy] [cap VISIBLE-STEP-CAP])
  (for/list ([payload (in-list (trace-payloads src strategy cap))])
    (payload->program payload)))

(define (trace-payloads src [strategy default-search-strategy] [cap VISIBLE-STEP-CAP])
  (define req (make-post-init-request src #:strategy strategy))
  (define ses0 (make-empty-session))
  (define-values (init-response ses1) (init! ses0 req 'visible-contract-id))
  (define init-payload (response->payload init-response))
  (define (loop ses remaining [acc (list init-payload)])
    (cond
      [(zero? remaining) (reverse acc)]
      [else
       (define-values (response ses^) (step! ses))
       (match (response-body->string response)
         ["null" (reverse acc)]
         [out
          (loop ses^
                (sub1 remaining)
                (cons (string->jsexpr out) acc))])]))
  (loop ses1 cap))

(define (adjacent-program-pairs payloads)
  (for/list ([left (in-list payloads)]
             [right (in-list (rest payloads))])
    (list left right)))

(define (payload-node-names src [strategy default-search-strategy])
  (append*
   (for/list ([payload (in-list (trace-payloads src strategy))])
     (json-node-names (payload->program payload)))))

(define/provide-test-suite VISIBLE-CONTRACTS
  (test-case "serializer emits only visible node kinds for the full example corpus under the default strategy"
    (define contract (read-visible-contract))
    (define allowed
      (sort (hash-ref contract 'visibleNodeNames) string<?))
    (define seen
      (sort
       (remove-duplicates
        (append*
         (for/list ([pr (in-list (frontend-example-programs))])
           (match-define (cons _label src) pr)
           (payload-node-names src))))
       string<?))
    (check-true (pair? seen))
    (for ([nm (in-list seen)])
      (check-not-false (member nm allowed)
                       (format "unexpected visible node kind from serializer: ~a" nm))))

  (test-case "serializer emits visible trees that satisfy the explicit stream/search AST shape"
    (for ([pr (in-list (frontend-example-programs))])
      (match-define (cons label src) pr)
      (for ([program (in-list (trace-programs src))])
        (check-true (visible-json-wf? program)
                    (format "visible AST wf failed for default strategy / ~a" label)))))

  (test-case "serializer emits only visible node kinds for surfaced strategies on the representative visible corpus"
    (define contract (read-visible-contract))
    (define allowed
      (sort (hash-ref contract 'visibleNodeNames) string<?))
    (define seen
      (sort
       (remove-duplicates
        (append*
         (for*/list ([strategy (in-list all-surfaced-search-strategies)]
                     [pr (in-list (frontend-example-programs))]
                     #:when (member (car pr) REPRESENTATIVE-VISIBLE-LABELS))
           (match-define (cons _label src) pr)
           (payload-node-names src strategy))))
       string<?))
    (check-true (pair? seen))
    (for ([nm (in-list seen)])
      (check-not-false (member nm allowed)
                       (format "unexpected visible node kind from serializer: ~a" nm))))

  (test-case "representative surfaced strategies satisfy the explicit visible AST shape"
    (for* ([strategy (in-list all-surfaced-search-strategies)]
           [pr (in-list (frontend-example-programs))]
           #:when (member (car pr) REPRESENTATIVE-VISIBLE-LABELS))
      (match-define (cons label src) pr)
      (for ([program (in-list (trace-programs src strategy))])
        (check-true
         (visible-json-wf? program)
         (format "~a / ~a violates visible AST wf"
                 (strategy-label strategy)
                 label)))))

  (test-case "surfaced strategy traces never repeat the visible tree on adjacent steps"
    (for* ([strategy (in-list all-surfaced-search-strategies)]
           [pr (in-list (frontend-example-programs))]
           #:when (member (car pr) REPRESENTATIVE-VISIBLE-LABELS))
      (match-define (cons label src) pr)
      (for ([pair (in-list (adjacent-program-pairs (trace-payloads src strategy)))])
        (match-define (list left right) pair)
        (define left-program (hash-ref left 'program))
        (define right-program (hash-ref right 'program))
        (define left-step (hash-ref left 'step))
        (define right-step (hash-ref right 'step))
        (define left-name (hash-ref left 'stepName))
        (define right-name (hash-ref right 'stepName))
        (check-false
         (equal? (string->jsexpr left-program)
                 (string->jsexpr right-program))
         (format "~a / ~a repeats visible tree on adjacent steps ~a (~a) and ~a (~a)"
                 (strategy-label strategy)
                 label
                 left-step
                 left-name
                 right-step
                 right-name)))))
  )

(module+ test
  (run-tests VISIBLE-CONTRACTS))
