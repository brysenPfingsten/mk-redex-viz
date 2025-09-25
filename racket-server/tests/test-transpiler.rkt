#lang racket
(require rackunit
         rackunit/text-ui
         redex 
         "../src/definitions.rkt"
         "../src/transpiler.rkt")

(define-test-suite ASSOCIATIVITY
  (test-case "Conjunctions Left Associate"
    (define PROG '((run* (q) (== 1 1) (== 2 2) (== 3 3))))
    (define-values (PARSED _) (parse-prog PROG))
    (check-true (redex-match? L (((_ _ ((g_1 ∧ g_2 _) ∧ g_3 _) _) _) Γ) PARSED)))

  (test-case "Disjunctions Right Associate"
    (define PROG '((run* (q)
                    (conde
                      [(conde
                        [(same q 'turtle)]
                        [(same q 'cat)]
                        [(== q 'dog)])]
                      [(same q 'fish)]))))
    (define-values (PARSED _) (parse-prog PROG))
    (check-true (redex-match? L (((_ _ ((g_1 ∨ (g_2 ∨ g_3 _) _) ∨ g_4 _) _) σ) Γ) PARSED))

    (define PROG1 '((run* (q)
                      (conde
                        ((conde
                          ((same q 'turtle))
	                      ((conde
	                          ((same q 'cat))
	                          ((== q 'dog))))))
                            ((same q 'fish))))))
    (define-values (PARSED1 _1) (parse-prog PROG1))
    (check-true (redex-match? L (((_ _ ((g_1 ∨ (g_2 ∨ g_3 _) _) ∨ g_4 _) _) σ) Γ) PARSED1))

    (define PROG2 '((run* (q)
                    (conde
                      [(same q 'turtle)]
                      [(same q 'cat)]
                      [(== q 'dog)]
                      [(same q 'fish)]))))
    (define-values (PARSED2 _2) (parse-prog PROG2))
    (check-true (redex-match? L (((_ _ (g_1 ∨ (g_2 ∨ (g_3 ∨ g_4 _) _) _) _) σ) Γ) PARSED2))
    ))

(define/provide-test-suite TRANSPILER
  #:after (thunk (displayln "Finished running tests for transpiler."))

  ASSOCIATIVITY)

#;(run-tests TRANSPILER)
