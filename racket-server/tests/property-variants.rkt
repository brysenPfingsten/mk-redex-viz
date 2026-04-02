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
    (check-true (redex-match? L4 config railed)))

  (test-case "delay(proceed(call)) boundary remains deterministic in final variants"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ()
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () (label "s"))))))))
    (define named-next* (apply-reduction-relation/tag-with-names Rflip-e cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rflip-e on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^call/" (~a fired-name))
                (format "expected call-prefixed rule, got ~a" fired-name)))

  (test-case "delay(proceed(call)) boundary deterministic in Rflip-l"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ()
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () (label "s"))))))))
    (define named-next* (apply-reduction-relation/tag-with-names Rflip-l cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rflip-l on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^call/" (~a fired-name))
                (format "expected call-prefixed rule, got ~a" fired-name)))

  (test-case "delay(proceed(call)) boundary deterministic in Rrail-e"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ()
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () (label "s"))))))))
    (define named-next* (apply-reduction-relation/tag-with-names Rrail-e cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rrail-e on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^call/" (~a fired-name))
                (format "expected call-prefixed rule, got ~a" fired-name)))

  (test-case "delay(proceed(call)) boundary deterministic in Rrail-l"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ()
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () (label "s"))))))))
    (define named-next* (apply-reduction-relation/tag-with-names Rrail-l cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rrail-l on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^call/" (~a fired-name))
                (format "expected call-prefixed rule, got ~a" fired-name))))

(define-test-suite VARIANT-INVARIANTS
  (test-case "L1 call variants safety: progress + state wf + shape closure"
    (for ([rel (in-list (list Rcall-eager Rcall-lazy))])
      (check-true (progress? rel cfg-call))
      (check-true (states-wf? cfg-call))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-call))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L1? rel cfg-call))))

  (test-case "L1 call variants uniqueness"
    (for ([rel (in-list (list Rcall-eager Rcall-lazy))])
      (check-true (unique-decomposition? rel cfg-call))))

  (test-case "L2 disjunction variant safety: progress + state wf + shape closure"
    (check-true (progress? Rdisj-left cfg-disj))
    (check-true (states-wf? cfg-disj))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rdisj-left cfg-disj))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L2? Rdisj-left cfg-disj)))

  (test-case "L2 disjunction variant uniqueness"
    (check-true (unique-decomposition? Rdisj-left cfg-disj)))

  (test-case "L3 base variants safety: progress + state wf + shape closure"
    (for ([rel (in-list (list Rbase-e Rbase-l))])
      (check-true (progress? rel cfg-call))
      (check-true (states-wf? cfg-call))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-call))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L3? rel cfg-call))))

  (test-case "L3 base variants uniqueness"
    (for ([rel (in-list (list Rbase-e Rbase-l))])
      (check-true (unique-decomposition? rel cfg-call))))

  (test-case "L3 Rflip-e safety: progress + state wf + shape closure"
    (check-true (progress? Rflip-e cfg-flip))
    (check-true (states-wf? cfg-flip))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rflip-e cfg-flip))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L3? Rflip-e cfg-flip)))

  (test-case "L3 Rflip-e uniqueness"
    (check-true (unique-decomposition? Rflip-e cfg-flip)))

  (test-case "L3 Rflip-l safety: progress + state wf + shape closure"
    (check-true (progress? Rflip-l cfg-flip))
    (check-true (states-wf? cfg-flip))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rflip-l cfg-flip))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L3? Rflip-l cfg-flip)))

  (test-case "L3 Rflip-l uniqueness"
    (check-true (unique-decomposition? Rflip-l cfg-flip)))

  (test-case "L4 Rrail-e safety: progress + state wf + shape closure"
    (check-true (progress? Rrail-e cfg-rail))
    (check-true (states-wf? cfg-rail))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rrail-e cfg-rail))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L4? Rrail-e cfg-rail)))

  (test-case "L4 Rrail-e uniqueness"
    (check-true (unique-decomposition? Rrail-e cfg-rail)))

  (test-case "L4 Rrail-l safety: progress + state wf + shape closure"
    (check-true (progress? Rrail-l cfg-rail))
    (check-true (states-wf? cfg-rail))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rrail-l cfg-rail))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L4? Rrail-l cfg-rail)))

  (test-case "L4 Rrail-l uniqueness"
    (check-true (unique-decomposition? Rrail-l cfg-rail))))

(define/provide-test-suite PROPERTY-VARIANTS
  #:before (thunk (displayln "Running variant lattice tests..."))
  #:after (thunk (displayln "Finished variant lattice tests."))
  VARIANT-SMOKE
  VARIANT-INVARIANTS)

(module+ test
  (run-tests PROPERTY-VARIANTS))
