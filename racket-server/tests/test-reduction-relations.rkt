#lang racket

(require redex
         redex/reduction-semantics
         rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(module+ test
  (define (single-step/tagged prog)
    (apply-reduction-relation/tag-with-names red prog))

  (define two-relations-delay-body
    (term ((delay ())
           ((r:+ () (∃ () ⊤ (sym "oZ")))
            (r:X () (∃ () ⊤ (sym "HYvONcWjZNW")))))))

  (let ([pn (apply-reduction-relation/tag-with-names red two-relations-delay-body)])
    (test-true
     "Regression: decomposing relation env with no relcall should stay deterministic"
     (or (null? pn) (null? (cdr pn)))))

  (define relcall-body
    (term ((proceed ((r:N (nat 4) (sym "r-tag")) (state () 0 () (sym "s"))))
           ((r:N (x:w) (∃ () (∃ () ⊤ (nat 1)) (sym "f")))))))

  (let ([pn (apply-reduction-relation/tag-with-names red relcall-body)])
    (test-true
     "Proceed-body substitution path should remain deterministic"
     (or (null? pn) (null? (cdr pn)))))

  ;; Stronger relation-call checks: verify actual rule names and shapes.
  (define relcall-goal-prog
    (term (((r:same (sym "cat") (sym "cat") (sym "r0"))
            (state () 0 () (sym "s")))
           ((r:same (x:x x:y) (x:x =? x:y (sym "u1")))))))

  (define relcall-goal-steps (single-step/tagged relcall-goal-prog))
  (check-equal? (length relcall-goal-steps) 1)
  (check-equal? (caar relcall-goal-steps) "Relation Call And Add Delay")
  (check-true
   (redex-match?
    L p
    (cadar relcall-goal-steps)))

  (define proceed-steps (single-step/tagged relcall-body))
  (check-equal? (length proceed-steps) 1)
  (check-equal? (caar proceed-steps) "Substitute Relation Body And Proceed")
  (check-true
   (redex-match?
    L p
    (cadar proceed-steps))))
