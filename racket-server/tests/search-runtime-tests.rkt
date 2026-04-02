#lang racket

(require rackunit
         rackunit/text-ui
         "../src/search-runtime.rkt"
         "../src/search-strategy.rkt"
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         "./example-compat-tests.rkt")

(provide SEARCH-RUNTIME)

(define (example-src label)
  (for/first ([pr (in-list (frontend-example-programs))]
              #:do [(match-define (cons example-label src) pr)]
              #:when (equal? example-label label))
    src))

(define (parse-src/canonical src)
  (parse-prog/canonical (read-all-sexprs (open-input-string src))))

(define/provide-test-suite SEARCH-RUNTIME
  (test-case "strategy registry covers every surfaced structured strategy"
    (define-values (cfg0 _html) (parse-src/canonical (example-src "fives/fours")))
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (match-define (strategy-spec spec-strategy _ in-domain? well-formed?)
        (lookup-strategy-spec strategy))
      (define internal-cfg (canonical-flat->calls-config cfg0))
      (check-equal? spec-strategy
                    strategy)
      (check-equal? (in-domain? internal-cfg)
                    (search-config-in-domain? strategy cfg0))
      (check-equal? (well-formed? internal-cfg)
                    (search-config-well-formed? strategy cfg0))))

  (test-case "strategy lookup preserves the internal-to-canonical step boundary"
    (define-values (cfg0 _html) (parse-src/canonical (example-src "fives/fours")))
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (match-define (strategy-spec _ step-once _ _) (lookup-strategy-spec strategy))
      (define internal-next* (step-once (canonical-flat->calls-config cfg0)))
      (define surfaced-next* ((lookup-search-step-once strategy) cfg0))
      (check-equal?
       (for/list ([succ (in-list internal-next*)])
         (match succ
           [(list name cfg)
            (list name (calls-config->canonical-flat cfg))]
           [_ succ]))
       surfaced-next*))))

(module+ test
  (run-tests SEARCH-RUNTIME))
