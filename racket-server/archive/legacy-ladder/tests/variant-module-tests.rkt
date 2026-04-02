#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "./variant-test-support.rkt"
         (prefix-in lang: "../src/languages/all.rkt")
         (prefix-in j: "../src/wf/all.rkt")
         "../src/reduction-relations/all.rkt")

(provide VARIANT-MODULES)

(define (single-named-step named-next*)
  (match named-next*
    [(list (list name cfg))
     (values name cfg)]
    [_ (error 'single-named-step
              (format "expected exactly one named reduction result, got ~v"
                      named-next*))]))

(define-test-suite LANGUAGE-MODULES
  (test-case "L1 includes calls and delay/proceed"
    (check-true
     (redex-match? lang:L1 s
                   (term (delay (proceed ((r:id (sym "ok") (label "call"))
                                          (state () () () () (label "s")))))))))

  (test-case "L2 includes disjunction but not delay/proceed"
    (check-true
     (redex-match? lang:L2 s
                   (term ((⊤ (state () () () () (label "a")))
                          <-+
                          (⊤ (state () () () () (label "b")))))))
    (check-false
     (redex-match? lang:L2 s
                   (term (delay (empty-tree))))))

  (test-case "L3 includes both calls and left disjunction"
    (check-true
     (redex-match? lang:L3 s
                   (term ((r:id (sym "ok") (label "call"))
                          (state () () () () (label "s"))))))
    (check-true
     (redex-match? lang:L3 s
                   (term ((⊤ (state () () () () (label "a")))
                          <-+
                          (⊤ (state () () () () (label "b"))))))))

  (test-case "L4 adds right disjunction"
    (check-true
     (redex-match? lang:L4 s
                   (term ((empty-tree)
                          +-> (⊤ (state () () () () (label "b"))))))))

  (test-case "Variant wf judgments cover L1/L2/L3/L4 syntax"
    (define cfg-l1
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (delay (proceed ((r:id (sym "ok") (label "call"))
                              (state () () () () (label "s")))))
             (empty-stream))))

    (define cfg-l2
      (term (()
             (((succeed (label "a")) (state () () () () (label "sa")))
              <-+
              ((succeed (label "b")) (state () () () () (label "sb"))))
             (empty-stream))))

    (define cfg-l3
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((delay (proceed ((r:id (sym "ok") (label "call"))
                               (state () () () () (label "s")))))
              <-+
              ((succeed (label "b")) (state () () () () (label "sb"))))
             (empty-stream))))

    (define cfg-l4
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (((delay (proceed ((r:id (sym "ok") (label "call"))
                                (state () () () () (label "s")))))
               <-+
               ((succeed (label "b")) (state () () () () (label "sb"))))
              +-> (empty-tree))
             (empty-stream))))

    (define cfg-bad-arity
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((r:id (sym "ok") (sym "extra") (label "call"))
              (state () () () () (label "s")))
             (empty-stream))))

    (check-true (judgment-holds (j:wf-config/L1? ,cfg-l1)))
    (check-true (judgment-holds (j:wf-config/L2? ,cfg-l2)))
    (check-true (judgment-holds (j:wf-config/L3? ,cfg-l3)))
    (check-true (judgment-holds (j:wf-config/L4? ,cfg-l4)))
    (check-false (judgment-holds (j:wf-config/L4? ,cfg-bad-arity)))
    (check-true (j:wf-config/target? "L4/config" cfg-l4))
    (check-false (j:wf-config/target? "L4/config" cfg-bad-arity))))

(define-test-suite RELATION-MODULES
  (test-case "Rl1-call-lazy expands plain relation calls directly"
    (define step1* (apply-reduction-relation/tag-with-names Rl1-call-lazy cfg-call))
    (define-values (step1-name cfg1) (single-named-step step1*))
    (check-equal? step1-name "l1/lazy-expand")
    (check-false (member 'r:id (symbols-in (tree-of cfg1)))))

  (test-case "Rl1-call-eager expands plain relation calls directly"
    (define step1* (apply-reduction-relation/tag-with-names Rl1-call-eager cfg-call))
    (define-values (step1-name cfg1) (single-named-step step1*))
    (check-equal? step1-name "l1/eager-expand")
    (check-false (member 'r:id (symbols-in (tree-of cfg1)))))

  (test-case "Rl1-call-lazy uses suspend/invoke/expand only for explicit source delay"
    (define step1* (apply-reduction-relation/tag-with-names Rl1-call-lazy cfg-call-source-delay))
    (define-values (step1-name cfg1) (single-named-step step1*))
    (check-equal? step1-name "l1/suspend-goal")

    (define step2* (apply-reduction-relation/tag-with-names Rl1-call-lazy cfg1))
    (define-values (step2-name cfg2) (single-named-step step2*))
    (check-equal? step2-name "l1/invoke-delay")

    (define step3* (apply-reduction-relation/tag-with-names Rl1-call-lazy cfg2))
    (define-values (step3-name cfg3) (single-named-step step3*))
    (check-equal? step3-name "l1/lazy-expand")
    (check-false (member 'r:id (symbols-in (tree-of cfg3)))))

  (test-case "Rl1-call-eager uses suspend/invoke/resume only for explicit source delay"
    (define step1* (apply-reduction-relation/tag-with-names Rl1-call-eager cfg-call-source-delay))
    (define-values (step1-name cfg1) (single-named-step step1*))
    (check-equal? step1-name "l1/suspend-goal")

    (define step2* (apply-reduction-relation/tag-with-names Rl1-call-eager cfg1))
    (define-values (step2-name cfg2) (single-named-step step2*))
    (check-equal? step2-name "l1/invoke-delay")

    (define step3* (apply-reduction-relation/tag-with-names Rl1-call-eager cfg2))
    (define-values (step3-name cfg3) (single-named-step step3*))
    (check-equal? step3-name "l1/eager-expand")
    (check-false (member 'r:id (symbols-in (tree-of cfg3)))))

  (test-case "Rl2-disj-left is left-biased deterministic on first answer"
    (define next (first (apply-reduction-relation Rl2-disj-left cfg-disj)))
    (check-equal?
     next
     (term (() (⊤ (state () () () () (label "b")))
               (⊤ (state () () () () (label "a")))))))

  (test-case "Rl2-disj-left bubbles a nested left answer before collecting it"
    (define cfg
      (term (()
             (((⊤ (state () () () () (label "a")))
               <-+
               (⊤ (state () () () () (label "b"))))
              <-+
              (⊤ (state () () () () (label "c"))))
             (empty-stream))))
    (define step1* (apply-reduction-relation/tag-with-names Rl2-disj-left cfg))
    (define-values (step1-name cfg1) (single-named-step step1*))
    (check-equal? step1-name "l2/bubble-left-answer")
    (check-equal?
     cfg1
     (term (()
            ((⊤ (state () () () () (label "a")))
             <-+
             ((⊤ (state () () () () (label "b")))
              <-+
              (⊤ (state () () () () (label "c")))))
            (empty-stream))))

    (define step2* (apply-reduction-relation/tag-with-names Rl2-disj-left cfg1))
    (define-values (step2-name cfg2) (single-named-step step2*))
    (check-equal? step2-name "l2/promote-left-answer")
    (check-equal?
     cfg2
     (term (()
            ((⊤ (state () () () () (label "b")))
             <-+
             (⊤ (state () () () () (label "c"))))
            (⊤ (state () () () () (label "a")))))))

  (test-case "Rl3-base-eager and Rl3-base-lazy both step call and disjunction configs"
    (for ([rel (in-list (list Rl3-base-eager Rl3-base-lazy))])
      (check-false (null? (apply-reduction-relation rel cfg-call)))
      (check-false (null? (apply-reduction-relation rel (term (() ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))
                                                             (empty-stream))))))))

  (test-case "Rl3-dfs variants bubble a nested left answer before collecting it"
    (define cfg
      (term (()
             (((⊤ (state () () () () (label "a")))
               <-+
               (⊤ (state () () () () (label "b"))))
              <-+
              (⊤ (state () () () () (label "c"))))
             (empty-stream))))
    (for ([rel (in-list (list Rl3-dfs-eager Rl3-dfs-lazy))])
      (define step1* (apply-reduction-relation/tag-with-names rel cfg))
      (define-values (step1-name cfg1) (single-named-step step1*))
      (check-equal? step1-name "l3-base/bubble-left-answer")
      (check-equal?
       cfg1
       (term (()
              ((⊤ (state () () () () (label "a")))
               <-+
               ((⊤ (state () () () () (label "b")))
                <-+
                (⊤ (state () () () () (label "c")))))
              (empty-stream))))))

  (test-case "Rl3-flip-eager and Rl3-flip-lazy perform left-only delay swap"
    (for ([rel (in-list (list Rl3-flip-eager Rl3-flip-lazy))])
      (define next (first (apply-reduction-relation rel cfg-flip)))
      (check-equal?
       next
       (term (() (delay ((⊤ (state () () () () (label "b"))) <-+ (empty-tree)))
                 (empty-stream))))))

  (test-case "Rl3-flip-eager and Rl3-flip-lazy propagate delay over left disjunction before resuming proceed"
    (define cfg
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((delay
               (proceed
                ((r:id (sym "ok") (label "call"))
                 (state () () () () (label "s")))))
              <-+
              (⊤ (state () () () () (label "b"))))
             (empty-stream))))
    (for ([rel (in-list (list Rl3-flip-eager Rl3-flip-lazy))])
      (define named-next* (apply-reduction-relation/tag-with-names rel cfg))
      (define-values (step-name next-cfg) (single-named-step named-next*))
      (check-equal? step-name "l3-flip/delay-swap-left")
      (check-equal?
       next-cfg
       (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
              (delay
               ((⊤ (state () () () () (label "b")))
                <-+
                (proceed
                 ((r:id (sym "ok") (label "call"))
                  (state () () () () (label "s"))))))
              (empty-stream))))))

  (test-case "Rl4-rail-eager and Rl4-rail-lazy introduce right-pointing disjunction"
    (for ([rel (in-list (list Rl4-rail-eager Rl4-rail-lazy))])
      (define next (first (apply-reduction-relation rel cfg-rail)))
      (check-equal?
       next
       (term (() (delay ((empty-tree) +-> (⊤ (state () () () () (label "b")))))
                 (empty-stream))))))

  (test-case "Rl4-rail-eager and Rl4-rail-lazy propagate delay into railroad branch before resuming proceed"
    (define cfg
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((delay
               (proceed
                ((r:id (sym "ok") (label "call"))
                 (state () () () () (label "s")))))
              <-+
              (⊤ (state () () () () (label "b"))))
             (empty-stream))))
    (for ([rel (in-list (list Rl4-rail-eager Rl4-rail-lazy))])
      (define named-next* (apply-reduction-relation/tag-with-names rel cfg))
      (define-values (step-name next-cfg) (single-named-step named-next*))
      (check-equal? step-name "l4-rail/enter-right")
      (check-equal?
       next-cfg
       (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
              (delay
               ((proceed
                 ((r:id (sym "ok") (label "call"))
                  (state () () () () (label "s"))))
                +->
                (⊤ (state () () () () (label "b")))))
              (empty-stream))))))

  (test-case "Rl4-rail-eager and Rl4-rail-lazy promote +-> right answer inside <-+ context"
    (define cfg
      (term (()
             (((empty-tree) +-> (⊤ (state () () () () (label "ra"))))
              <-+
              (empty-tree))
             (empty-stream))))
    (for ([rel (in-list (list Rl4-rail-eager Rl4-rail-lazy))])
      (define named-next* (apply-reduction-relation/tag-with-names rel cfg))
      (define-values (step-name next-cfg) (single-named-step named-next*))
      (check-equal? step-name "l4-rail/promote-right-answer")
      (check-equal?
       next-cfg
       (term (() ((empty-tree)
                  <-+
                  (empty-tree))
                (⊤ (state () () () () (label "ra")))))))))

  (test-case "Rl4-rail-eager and Rl4-rail-lazy handle active right-branch left-disjunction roots"
    (define cfg-answer
      (term (()
             ((empty-tree)
              +->
              ((⊤ (state () () () () (label "ra")))
               <-+
               (empty-tree)))
             (empty-stream))))
    (define cfg-fail
      (term (()
             ((empty-tree)
              +->
              ((empty-tree)
               <-+
               (⊤ (state () () () () (label "rb")))))
             (empty-stream))))
    (define expected-answer
      (term (()
             ((empty-tree) +-> (empty-tree))
             (⊤ (state () () () () (label "ra"))))))
    (define expected-fail
      (term (()
             ((empty-tree)
              +->
              (⊤ (state () () () () (label "rb"))))
             (empty-stream))))
    (for ([rel (in-list (list Rl4-rail-eager Rl4-rail-lazy))])
      (define-values (answer-step answer-cfg)
        (single-named-step (apply-reduction-relation/tag-with-names rel cfg-answer)))
      (check-equal? answer-step "l4-rail/promote-right-left-answer")
      (check-equal? answer-cfg expected-answer)

      (define-values (fail-step fail-cfg)
        (single-named-step (apply-reduction-relation/tag-with-names rel cfg-fail)))
      (check-equal? fail-step "l4-rail/skip-right-left-fail")
      (check-equal? fail-cfg expected-fail)))

(define/provide-test-suite VARIANT-MODULES
  LANGUAGE-MODULES
  RELATION-MODULES)

(module+ test
  (run-tests VARIANT-MODULES))
