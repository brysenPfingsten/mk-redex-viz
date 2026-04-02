#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "./variant-test-support.rkt"
         "../src/extensions/variant-languages.rkt"
         "../src/reduction-relations/extensions/variant-relations.rkt")

(provide PROPERTY-VARIANTS)

(define-test-suite VARIANT-SMOKE
  (test-case "eager vs lazy first-step call expansion differs"
    (define eager-next (first (apply-reduction-relation Rcall-eager cfg-call)))
    (define lazy-next (first (apply-reduction-relation Rcall-lazy cfg-call)))
    ;; eager payload has already substituted away the call
    (check-false (member 'r:id (symbols-in (tree-of eager-next))))
    ;; lazy payload still carries relation call in proceed frame
    (check-not-false (member 'r:id (symbols-in (tree-of lazy-next)))))

  (test-case "left disjunction collects left answer first"
    (define next1 (first (apply-reduction-relation Rdisj-left cfg-disj)))
    (check-equal?
     next1
     (term (() ((state () () () (label "a")))
               (⊤ (state () () () (label "b"))))))
    (define next2 (first (apply-reduction-relation Rdisj-left next1)))
    (check-equal?
     next2
     (term (() ((state () () () (label "a"))
                (state () () () (label "b")))
               (empty-tree)))))

  (test-case "Rbase variants can step call and disjunction configs"
    (check-false (null? (apply-reduction-relation Rbase-e cfg-call)))
    (check-false (null? (apply-reduction-relation Rbase-l cfg-call)))
    (check-false (null? (apply-reduction-relation Rbase-e (term (() () ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b)))))))
    (check-false (null? (apply-reduction-relation Rbase-l (term (() () ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))))))))

  (test-case "flip branch keeps left-only disjunction syntax"
    (define flipped (first (apply-reduction-relation Rflip-e cfg-flip)))
    (check-true
     (redex-match? L3 config
                   (term (() () (delay ((⊤ ,sigma-b) <-+ (empty-tree))))))
     "expected flip step to swap left-only disjunction branches")
    (check-true (redex-match? L3 config flipped)))

  (test-case "railroad branch introduces right-pointing syntax"
    (define railed (first (apply-reduction-relation Rrail-e cfg-rail)))
    (check-true
     (redex-match? L4 config
                   (term (() () (delay ((empty-tree) +-> (⊤ ,sigma-b))))))
     "expected railroad step to introduce +->")
    (check-true (redex-match? L4 config railed))))

(define-test-suite VARIANT-INVARIANTS
  (test-case "L1 call variants: progress + uniqueness + state wf + shape closure"
    (for ([rel (in-list (list Rcall-eager Rcall-lazy))])
      (check-true (progress? rel cfg-call))
      (check-true (unique-decomposition? rel cfg-call))
      (check-true (states-wf? cfg-call))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-call))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L1? rel cfg-call))))

  (test-case "L2 disjunction variant: progress + uniqueness + state wf + shape closure"
    (check-true (progress? Rdisj-left cfg-disj))
    (check-true (unique-decomposition? Rdisj-left cfg-disj))
    (check-true (states-wf? cfg-disj))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rdisj-left cfg-disj))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L2? Rdisj-left cfg-disj)))

  (test-case "L3 base variants: progress + uniqueness + state wf + shape closure"
    (for ([rel (in-list (list Rbase-e Rbase-l))])
      (check-true (progress? rel cfg-call))
      (check-true (unique-decomposition? rel cfg-call))
      (check-true (states-wf? cfg-call))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-call))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L3? rel cfg-call))))

  (test-case "L3 flip variants: progress + state wf + shape closure"
    (for ([rel (in-list (list Rflip-e Rflip-l))])
      (check-true (progress? rel cfg-flip))
      (check-true (states-wf? cfg-flip))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-flip))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L3? rel cfg-flip))))

  (test-case "L4 railroad variants: progress + state wf + shape closure"
    (for ([rel (in-list (list Rrail-e Rrail-l))])
      (check-true (progress? rel cfg-rail))
      (check-true (states-wf? cfg-rail))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-rail))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L4? rel cfg-rail)))))

(define/provide-test-suite PROPERTY-VARIANTS
  #:before (thunk (displayln "Running variant lattice tests..."))
  #:after (thunk (displayln "Finished variant lattice tests."))
  VARIANT-SMOKE
  VARIANT-INVARIANTS)

(module+ test
  (run-tests PROPERTY-VARIANTS))
