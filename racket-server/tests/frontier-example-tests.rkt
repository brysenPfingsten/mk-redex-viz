#lang racket

(require rackunit
         rackunit/text-ui
         "../src/search-runtime.rkt"
         "../src/search-strategy.rkt"
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         "./example-compat-tests.rkt"
         "./frontier-observable-support.rkt"
         "./runtime-test-support.rkt")

(provide FRONTIER-EXAMPLES)

(define FULL-TRACE-CAP 128)
(define CADENCE-TRACE-CAP 12)

(define local-example-specs
  (list
   (list "fresh delay witness"
         "micro"
         "(run* (q)\n  (fresh (x)\n    (conj\n      (Zzz (== x 'nap))\n      (== q x))))")
   (list "bounce cadence witness"
         "micro"
         "(run* (q)\n  (disj\n    (disj\n      (== q 'left-now)\n      (Zzz (== q 'left-later)))\n    (disj\n      (== q 'right-now)\n      (Zzz (== q 'right-later)))))")))

(define/match (strategy-label strategy)
  [((search-strategy hoist scheduler))
   (format "~a/~a" hoist scheduler)])

(define (count-step-name steps expected [count 0])
  (match steps
    ['() count]
    [(cons step-name rest)
     (count-step-name rest
                      expected
                      (if (or (string=? step-name expected)
                              (and (string=? expected "core/fresh-substitute")
                                   (string-prefix? "core/fresh-substitute"
                                                   step-name)))
                          (add1 count)
                          count))]))

(define (example-spec label)
  (or (for/first ([pr (in-list (frontend-example-programs))]
                  #:do [(match-define (cons example-label src) pr)]
                  #:when (equal? example-label label))
        (list "mini" src))
      (for/first ([spec (in-list local-example-specs)]
                  #:do [(match-define (list example-label source-mode src) spec)]
                  #:when (equal? example-label label))
        (list source-mode src))))

(define (parse-example/canonical label)
  (define spec (example-spec label))
  (unless spec
    (error 'parse-example/canonical
           (format "missing example label: ~a" label)))
  (match-define (list source-mode src) spec)
  (define-values (cfg _html)
    (parse-prog/canonical (read-all-sexprs (open-input-string src))
                          #:source-mode source-mode))
  cfg)

(define (trace-stepper step-once cfg [step-cap FULL-TRACE-CAP] [i 0] [acc '()])
  (match (step-once cfg)
    ['()
     (values (reverse acc)
             cfg
             (if (final-config? cfg) 'value 'stuck))]
    [(list _ ...) #:when (>= i step-cap)
     (values (reverse acc) cfg 'cap)]
    [(list (list name cfg^))
     (trace-stepper step-once
                    cfg^
                    step-cap
                    (add1 i)
                    (cons (~a name) acc))]
    [_ (values (reverse acc) cfg 'nondeterministic)]))

(define (trace-example label strategy [step-cap FULL-TRACE-CAP])
  (trace-stepper (lookup-search-step-once strategy)
                 (parse-example/canonical label)
                 step-cap))

(define/provide-test-suite FRONTIER-EXAMPLES
  (test-case "fresh witness introduces one scoped Freshened frontier across strategies"
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define-values (steps final-cfg status)
        (trace-example "fresh witness" strategy))
      (check-equal? status 'value (strategy-label strategy))
      (check-true (config-exact-scope? final-cfg) (strategy-label strategy))
      (check-equal? (count-step-name steps "core/fresh-substitute")
                    2
                    (strategy-label strategy))
      (check-equal? (count-bounced final-cfg)
                    0
                    (strategy-label strategy))
      (check-equal? (count-answers final-cfg)
                    1
                    (strategy-label strategy))))

  (test-case "shared and branch-local fresh examples differ by scoped Freshened count"
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define-values (shared-steps shared-final shared-status)
        (trace-example "fresh shared disj" strategy))
      (define-values (branch-steps branch-final branch-status)
        (trace-example "fresh branch disj" strategy))
      (check-equal? shared-status 'value (strategy-label strategy))
      (check-equal? branch-status 'value (strategy-label strategy))
      (check-true (config-exact-scope? shared-final) (strategy-label strategy))
      (check-true (config-exact-scope? branch-final) (strategy-label strategy))
      (check-equal? (count-answers shared-final)
                    2
                    (strategy-label strategy))
      (check-equal? (count-answers branch-final)
                    2
                    (strategy-label strategy))
      (check-equal? (count-step-name shared-steps "core/fresh-substitute")
                    2
                    (strategy-label strategy))
      (check-equal? (count-step-name branch-steps "core/fresh-substitute")
                    3
                    (strategy-label strategy))))

  (test-case "split fresh conjunction stays live and preserves exact scope"
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define-values (_steps final-cfg status)
        (trace-example "fresh split conj" strategy))
      (check-equal? status 'value (strategy-label strategy))
      (check-true (config-exact-scope? final-cfg) (strategy-label strategy))
      (check-equal? (count-answers final-cfg)
                    1
                    (strategy-label strategy))))

  (test-case "fresh delay witness keeps one bounce inside its exact Freshened scope"
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define-values (steps final-cfg status)
        (trace-example "fresh delay witness" strategy))
      (check-equal? status 'value (strategy-label strategy))
      (check-true (config-exact-scope? final-cfg) (strategy-label strategy))
      (check-equal? (count-step-name steps "core/fresh-substitute")
                    2
                    (strategy-label strategy))
      (check-equal? (count-step-name steps "delay/invoke-delay")
                    1
                    (strategy-label strategy))
      (check-equal? (count-bounced final-cfg)
                    1
                    (strategy-label strategy))
      (check-equal? (count-answers final-cfg)
                    1
                    (strategy-label strategy))))

  (test-case "bounce cadence witness keeps final answers fixed while eager capped cadence stays within one answer"
    (define strategy*
      (list (search-strategy "early" "dfs")
            (search-strategy "early" "flip")
            (search-strategy "early" "rail")))
    (define full-observations
      (for/list ([strategy (in-list strategy*)])
        (define-values (steps final-cfg status)
          (trace-example "bounce cadence witness" strategy))
        (check-equal? status 'value (strategy-label strategy))
        (check-true (config-exact-scope? final-cfg)
                    (format "~a :: ~s"
                            (strategy-label strategy)
                            final-cfg))
        (list (count-answers final-cfg)
              (count-step-name steps "core/fresh-substitute"))))
    (check-true (positive? (caar full-observations)))
    (check-true
     (andmap (lambda (obs)
               (= (second obs) 1))
             full-observations))
    (check-equal? (length (remove-duplicates (map first full-observations)))
                  1)
    (define cadence-observations
      (for/list ([strategy (in-list strategy*)])
        (define-values (_steps final-cfg status)
          (trace-example "bounce cadence witness" strategy CADENCE-TRACE-CAP))
        (check-true (or (eq? status 'value)
                        (eq? status 'cap))
                    (strategy-label strategy))
        (list (count-answers final-cfg)
              (count-bounced final-cfg))))
    (define answer-counts (map first cadence-observations))
    (define bounce-counts (map second cadence-observations))
    (check-equal? (length (remove-duplicates bounce-counts))
                  1
                  (format "expected eager capped bounced cadence agreement, got ~s"
                          cadence-observations))
    (check-true (<= (- (apply max answer-counts)
                       (apply min answer-counts))
                    1)
                (format "expected eager capped answers to stay within one of each other, got ~s"
                        cadence-observations))))

(module+ test
  (run-tests FRONTIER-EXAMPLES))
