#lang racket

(require rackunit rackunit/text-ui redex/reduction-semantics
         "../src/search-runtime.rkt" "../src/search-strategy.rkt"
         "../src/sexpr-read.rkt" "../src/transpiler.rkt"
         "./example-compat-tests.rkt"
         (prefix-in matrix: "../derivations/matrix/full-source.rkt"))

(provide SEARCH-RUNTIME)

(define (compile-source source [mode "mini"])
  (parse-prog/canonical (read-all-sexprs (open-input-string source)) #:source-mode mode))

(define/provide-test-suite SEARCH-RUNTIME
  (test-case "default API selects strict Search separately from the lattice schedulers"
    (check-equal? default-search-strategy (strict-search))
    (check-equal? (normalize-search-strategy #f) default-search-strategy)
    (check-exn exn:fail?
               (lambda () (normalize-search-strategy
                           (hasheq 'hoist "late" 'scheduler "rail"))))
    (check-exn exn:fail?
               (lambda () (normalize-search-strategy (search-strategy "zigzag")))))

  (test-case "every frontend example enters the actual full matrix relation"
    (define spec (lookup-strategy-spec default-search-strategy))
    (for ([example (in-list (frontend-example-programs))])
      (match-define (cons name source) example)
      (define-values (configuration html query) (compile-source source))
      (check-match configuration `(program ,_ (commit (eval (Owners) ,_ ,_))))
      (check-true (query-info? query) name)
      (check-true (search-config-in-domain? default-search-strategy configuration) name)
      (check-true (search-config-well-formed? default-search-strategy configuration) name)
      (check-equal? ((strategy-spec-step-once spec) configuration)
                    (apply-reduction-relation/tag-with-names matrix:strict-s-rel-red configuration)
                    name)))

  (test-case "relation calls expand eagerly with no implicit suspension"
    (define-values (initial html query)
      (compile-source "(defrel (same x y) (== x y)) (run* (q) (same q 'cat))" "micro"))
    (define step (lookup-search-step-once default-search-strategy))
    (define (trace configuration [labels '()])
      (match (step configuration)
        ['()
         (check-equal? (configuration-status configuration) 'complete)
         (reverse labels)]
        [(list (list name next)) (trace next (cons name labels))]))
    (check-equal? (trace initial) '("allocate-fresh" "eval-call" "eval-atom" "commit-one")))

  (test-case "paused Frontiers require an explicit public advance"
    (define state '(state () () () (label "initial")))
    (define paused `(program () (More (Delay (Owners) (eval (Owners) (succeed (label "A")) ,state)))))
    (define step (lookup-search-step-once default-search-strategy))
    (check-equal? (configuration-status paused) 'paused)
    (check-equal? (step paused) '())
    (define resumed (advance-configuration paused))
    (check-equal? resumed `(program () (advance ,(third paused))))
    (check-equal? (configuration-status resumed) 'running)
    (check-match (step resumed) (list (list "advance-delay" _)))
    (define unfinished `(program () (Emit (Owners) (Answer (Owners) ,state) (commit (Empty (Owners))))))
    (check-equal? (configuration-status unfinished) 'running)
    (check-exn exn:fail? (lambda () (advance-configuration unfinished)))
    (for ([frontier (in-list `((Done (Owners)) (Solo (Owners) ,state)))])
      (define completed `(program () ,frontier))
      (check-equal? (configuration-status completed) 'complete)
      (check-equal? (step completed) '())
      (check-exn exn:fail? (lambda () (advance-configuration completed))))
    (check-equal? (configuration-status '(program () (force (Empty (Owners))))) 'stuck))

  (test-case "unguarded recursion remains available for bounded named stepping"
    (define-values (initial html query)
      (compile-source "(defrel (loopo q) (loopo q)) (run* (q) (loopo q))" "micro"))
    (define step (lookup-search-step-once default-search-strategy))
    (for/fold ([configuration initial]) ([index (in-range 24)])
      (check-equal? (configuration-status configuration) 'running)
      (check-true (search-config-well-formed? default-search-strategy configuration))
      (match-define (list (list name next)) (step configuration))
      (check-equal? name (if (zero? index) "allocate-fresh" "eval-call"))
      next))

  (test-case "every current strategy accepts direct Solo and rejects Last packaging"
    (define state '(state () () () (label "terminal")))
    (for ([spec (in-list all-strategy-specs)])
      (define strategy (strategy-spec-strategy spec))
      (check-true (search-config-in-domain? strategy `(program () (Solo (Owners) ,state))))
      (check-false
       (search-config-in-domain? strategy `(program () (Last (Owners) (Answer (Owners) ,state))))))))

(module+ test
  (run-tests SEARCH-RUNTIME))
