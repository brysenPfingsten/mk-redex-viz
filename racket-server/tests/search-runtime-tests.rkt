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

(define hoist-witness-micro-program
  "(run 2 (q)
     (conj
       (disj
         (== q 'hoist)
         (== q 'witness))
       (== q q)))")

(define (collect-step-names stepper cfg [remaining 8])
  (cond
    [(zero? remaining) '()]
    [else
     (match (stepper cfg)
       ['() '()]
       [(list (list name next))
        (cons name
              (collect-step-names stepper next (sub1 remaining)))])]))

(define/provide-test-suite SEARCH-RUNTIME
  (test-case "strategy registry covers every surfaced structured strategy"
    (define-values (cfg0 _html) (parse-src/canonical (example-src "fives/fours")))
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (match-define (strategy-spec spec-strategy _ in-domain? well-formed?)
        (lookup-strategy-spec strategy))
      (check-equal? spec-strategy
                    strategy)
      (check-equal? (in-domain? cfg0)
                    (search-config-in-domain? strategy cfg0))
      (check-equal? (well-formed? cfg0)
                    (search-config-well-formed? strategy cfg0))))

  (test-case "strategy lookup returns the same internal stepper as the registry"
    (define-values (cfg0 _html) (parse-src/canonical (example-src "fives/fours")))
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (match-define (strategy-spec _ step-once _ _) (lookup-strategy-spec strategy))
      (check-equal? (step-once cfg0)
                    ((lookup-search-step-once strategy) cfg0))))

  (test-case "late flip hoist witness continues past expand-disjunction"
    (define-values (cfg0 _html)
      (parse-prog/canonical
       (read-all-sexprs (open-input-string hoist-witness-micro-program))
       #:source-mode "micro"))
    (define names
      (collect-step-names
       (lookup-search-step-once (search-strategy "late" "flip"))
       cfg0
       6))
    (check-equal? (take names 4)
                  '("fresh-substitute"
                    "conj-distribute-state"
                    "expand-disjunction"
                    "unify-success"))))

(module+ test
  (run-tests SEARCH-RUNTIME))
