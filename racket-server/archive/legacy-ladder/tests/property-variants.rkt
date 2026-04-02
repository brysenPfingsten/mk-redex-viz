#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "./variant-test-support.rkt"
         "../src/languages/all.rkt"
         "../src/reduction-relations/all.rkt")

(provide PROPERTY-VARIANTS)

(define-test-suite VARIANT-SMOKE
  (test-case "eager and lazy first-step plain call expansion both inline the relation body"
    (define eager-next (first (apply-reduction-relation Rl1-call-eager cfg-call)))
    (define lazy-next (first (apply-reduction-relation Rl1-call-lazy cfg-call)))
    (check-false (member 'r:id (symbols-in (tree-of eager-next))))
    (check-false (member 'r:id (symbols-in (tree-of lazy-next))))
    (check-equal? eager-next lazy-next))

  (test-case "left disjunction collects left answer first"
    (define next1 (first (apply-reduction-relation Rl2-disj-left cfg-disj)))
    (check-equal?
     next1
     (term (() (⊤ (state () () () () (label "b")))
               (⊤ (state () () () () (label "a"))))))
    (define next2* (apply-reduction-relation Rl2-disj-left next1))
    (check-equal? (length next2*) 1)
    (check-equal?
     (first next2*)
     (term (() (empty-tree)
               ((⊤ (state () () () () (label "a")))
                +
                (⊤ (state () () () () (label "b"))))))))

  (test-case "Rbase variants can step call and disjunction configs"
    (check-false (null? (apply-reduction-relation Rl3-base-eager cfg-call)))
    (check-false (null? (apply-reduction-relation Rl3-base-lazy cfg-call)))
    (check-false (null? (apply-reduction-relation Rl3-base-eager (term (() ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))
                                                                      (empty-stream))))))
    (check-false (null? (apply-reduction-relation Rl3-base-lazy (term (() ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))
                                                                     (empty-stream)))))))

  (test-case "flip branch keeps left-only disjunction syntax"
    (define flipped (first (apply-reduction-relation Rl3-flip-eager cfg-flip)))
    (check-equal? flipped
                  (term (() (delay ((⊤ ,sigma-b) <-+ (empty-tree)))
                            (empty-stream)))
                  "expected flip step to swap left-only disjunction branches")
    (check-true (redex-match? L3 config flipped)))

  (test-case "railroad branch introduces right-pointing syntax"
    (define railed (first (apply-reduction-relation Rl4-rail-eager cfg-rail)))
    (check-equal? railed
                  (term (() (delay ((empty-tree) +-> (⊤ ,sigma-b)))
                            (empty-stream)))
                  "expected railroad step to introduce +->")
    (check-true (redex-match? L4 config railed)))

  (test-case "delay(proceed(call)) boundary remains deterministic in final variants"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () () (label "s")))))
             (empty-stream))))
    (define named-next* (apply-reduction-relation/tag-with-names Rl3-flip-eager cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rl3-flip-eager on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^l3-base/" (~a fired-name))
                (format "expected l3-base rule, got ~a" fired-name)))

  (test-case "delay(proceed(call)) boundary deterministic in Rl3-flip-lazy"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () () (label "s")))))
             (empty-stream))))
    (define named-next* (apply-reduction-relation/tag-with-names Rl3-flip-lazy cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rl3-flip-lazy on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^l3-base/" (~a fired-name))
                (format "expected l3-base rule, got ~a" fired-name)))

  (test-case "delay(proceed(call)) boundary deterministic in Rl4-rail-eager"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () () (label "s")))))
             (empty-stream))))
    (define named-next* (apply-reduction-relation/tag-with-names Rl4-rail-eager cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rl4-rail-eager on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^l3-base/" (~a fired-name))
                (format "expected l3-base rule, got ~a" fired-name)))

  (test-case "delay(proceed(call)) boundary deterministic in Rl4-rail-lazy"
    (define cfg-delay-proceed-call
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (delay
              (proceed
               ((r:id (sym "ok") (label "call"))
                (state () () () () (label "s")))))
             (empty-stream))))
    (define named-next* (apply-reduction-relation/tag-with-names Rl4-rail-lazy cfg-delay-proceed-call))
    (check-equal? (length named-next*)
                  1
                  "expected exactly one next step for Rl4-rail-lazy on delay/proceed boundary")
    (define fired-name (first (first named-next*)))
    (check-true (regexp-match? #rx"^l3-base/" (~a fired-name))
                (format "expected l3-base rule, got ~a" fired-name))))

(define-test-suite VARIANT-INVARIANTS
  (test-case "L1 call variants safety: progress + state wf + shape closure"
    (for ([rel (in-list (list Rl1-call-eager Rl1-call-lazy))])
      (check-true (progress? rel cfg-call))
      (check-true (states-wf? cfg-call))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-call))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L1? rel cfg-call))))

  (test-case "L1 call variants uniqueness"
    (for ([rel (in-list (list Rl1-call-eager Rl1-call-lazy))])
      (check-true (unique-decomposition? rel cfg-call))))

  (test-case "L2 disjunction variant safety: progress + state wf + shape closure"
    (check-true (progress? Rl2-disj-left cfg-disj))
    (check-true (states-wf? cfg-disj))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rl2-disj-left cfg-disj))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L2? Rl2-disj-left cfg-disj)))

  (test-case "L2 disjunction variant uniqueness"
    (check-true (unique-decomposition? Rl2-disj-left cfg-disj)))

  (test-case "L3 base variants safety: progress + state wf + shape closure"
    (for ([rel (in-list (list Rl3-base-eager Rl3-base-lazy))])
      (check-true (progress? rel cfg-call))
      (check-true (states-wf? cfg-call))
      (check-true (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg-call))])
                    (states-wf? cfg^)))
      (check-true (shape-closed/L3? rel cfg-call))))

  (test-case "L3 base variants uniqueness"
    (for ([rel (in-list (list Rl3-base-eager Rl3-base-lazy))])
      (check-true (unique-decomposition? rel cfg-call))))

  (test-case "L3 Rl3-flip-eager safety: progress + state wf + shape closure"
    (check-true (progress? Rl3-flip-eager cfg-flip))
    (check-true (states-wf? cfg-flip))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rl3-flip-eager cfg-flip))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L3? Rl3-flip-eager cfg-flip)))

  (test-case "L3 Rl3-flip-eager uniqueness"
    (check-true (unique-decomposition? Rl3-flip-eager cfg-flip)))

  (test-case "L3 Rl3-flip-lazy safety: progress + state wf + shape closure"
    (check-true (progress? Rl3-flip-lazy cfg-flip))
    (check-true (states-wf? cfg-flip))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rl3-flip-lazy cfg-flip))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L3? Rl3-flip-lazy cfg-flip)))

  (test-case "L3 Rl3-flip-lazy uniqueness"
    (check-true (unique-decomposition? Rl3-flip-lazy cfg-flip)))

  (test-case "L4 Rl4-rail-eager safety: progress + state wf + shape closure"
    (check-true (progress? Rl4-rail-eager cfg-rail))
    (check-true (states-wf? cfg-rail))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rl4-rail-eager cfg-rail))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L4? Rl4-rail-eager cfg-rail)))

  (test-case "L4 Rl4-rail-eager uniqueness"
    (check-true (unique-decomposition? Rl4-rail-eager cfg-rail)))

  (test-case "L4 Rl4-rail-lazy safety: progress + state wf + shape closure"
    (check-true (progress? Rl4-rail-lazy cfg-rail))
    (check-true (states-wf? cfg-rail))
    (check-true (for/and ([cfg^ (in-list (apply-reduction-relation Rl4-rail-lazy cfg-rail))])
                  (states-wf? cfg^)))
    (check-true (shape-closed/L4? Rl4-rail-lazy cfg-rail)))

  (test-case "L4 Rl4-rail-lazy uniqueness"
    (check-true (unique-decomposition? Rl4-rail-lazy cfg-rail))))

(define/provide-test-suite PROPERTY-VARIANTS
  #:before (thunk (displayln "Running variant lattice tests..."))
  #:after (thunk (displayln "Finished variant lattice tests."))
  VARIANT-SMOKE
  VARIANT-INVARIANTS)

(module+ test
  (run-tests PROPERTY-VARIANTS))
