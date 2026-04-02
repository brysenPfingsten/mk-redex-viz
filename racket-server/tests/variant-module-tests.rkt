#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "./variant-test-support.rkt"
         (prefix-in lang: "../src/extensions/variant-languages.rkt")
         (prefix-in j: "../src/wf-variants.rkt")
         "../src/reduction-relations/extensions/variant-relations.rkt"
         (prefix-in dn: "../src/reduction-relations/extensions/rdfs-nodelay.rkt"))

(provide VARIANT-MODULES)

(define-test-suite LANGUAGE-MODULES
  (test-case "L1 includes calls and delay/proceed"
    (check-true
     (redex-match? lang:L1 s
                   (term (delay (proceed ((r:id (sym "ok") (label "call"))
                                          (state () () () (label "s")))))))))

  (test-case "L2 includes disjunction but not delay/proceed"
    (check-true
     (redex-match? lang:L2 s
                   (term ((⊤ (state () () () (label "a")))
                          <-+
                          (⊤ (state () () () (label "b")))))))
    (check-false
     (redex-match? lang:L2 s
                   (term (delay (empty-tree))))))

  (test-case "L3 includes both calls and left disjunction"
    (check-true
     (redex-match? lang:L3 s
                   (term ((r:id (sym "ok") (label "call"))
                          (state () () () (label "s"))))))
    (check-true
     (redex-match? lang:L3 s
                   (term ((⊤ (state () () () (label "a")))
                          <-+
                          (⊤ (state () () () (label "b"))))))))

  (test-case "L4 adds right disjunction"
    (check-true
     (redex-match? lang:L4 s
                   (term ((empty-tree)
                          +-> (⊤ (state () () () (label "b"))))))))

  (test-case "Variant wf judgments cover L1/L2/L3/L4 syntax"
    (define cfg-l1
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (delay (proceed ((r:id (sym "ok") (label "call"))
                              (state () () () (label "s")))))
             (empty-stream))))

    (define cfg-l2
      (term (()
             (((succeed (label "a")) (state () () () (label "sa")))
              <-+
              ((succeed (label "b")) (state () () () (label "sb"))))
             (empty-stream))))

    (define cfg-l3
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((delay (proceed ((r:id (sym "ok") (label "call"))
                               (state () () () (label "s")))))
              <-+
              ((succeed (label "b")) (state () () () (label "sb"))))
             (empty-stream))))

    (define cfg-l4
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             (((delay (proceed ((r:id (sym "ok") (label "call"))
                                (state () () () (label "s")))))
               <-+
               ((succeed (label "b")) (state () () () (label "sb"))))
              +-> (empty-tree))
             (empty-stream))))

    (define cfg-bad-arity
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((r:id (sym "ok") (sym "extra") (label "call"))
              (state () () () (label "s")))
             (empty-stream))))

    (check-true (judgment-holds (j:wf-config/L1? ,cfg-l1)))
    (check-true (judgment-holds (j:wf-config/L2? ,cfg-l2)))
    (check-true (judgment-holds (j:wf-config/L3? ,cfg-l3)))
    (check-true (judgment-holds (j:wf-config/L4? ,cfg-l4)))
    (check-false (judgment-holds (j:wf-config/L4? ,cfg-bad-arity)))
    (check-true (j:wf-config/target? "L4/config" cfg-l4))
    (check-false (j:wf-config/target? "L4/config" cfg-bad-arity))))

(define-test-suite RELATION-MODULES
  (test-case "Rl1-call-lazy relation call lifecycle emits deterministic suspend/invoke/expand sequence"
    (define step1* (apply-reduction-relation/tag-with-names Rl1-call-lazy cfg-call))
    (check-equal? (length step1*) 1)
    (check-equal? (caar step1*) "call/lazy-suspend-call")
    (define cfg1 (cadar step1*))

    (define step2* (apply-reduction-relation/tag-with-names Rl1-call-lazy cfg1))
    (check-equal? (length step2*) 1)
    (check-equal? (caar step2*) "call/lazy-invoke-delay")
    (define cfg2 (cadar step2*))

    (define step3* (apply-reduction-relation/tag-with-names Rl1-call-lazy cfg2))
    (check-equal? (length step3*) 1)
    (check-equal? (caar step3*) "call/lazy-expand-on-resume")
    (check-false (member 'r:id (symbols-in (tree-of (cadar step3*))))))

  (test-case "Rl1-call-eager relation call lifecycle emits deterministic suspend/invoke/resume sequence"
    (define step1* (apply-reduction-relation/tag-with-names Rl1-call-eager cfg-call))
    (check-equal? (length step1*) 1)
    (check-equal? (caar step1*) "call/eager-suspend-expanded")
    (define cfg1 (cadar step1*))

    (define step2* (apply-reduction-relation/tag-with-names Rl1-call-eager cfg1))
    (check-equal? (length step2*) 1)
    (check-equal? (caar step2*) "call/eager-invoke-delay")
    (define cfg2 (cadar step2*))

    (define step3* (apply-reduction-relation/tag-with-names Rl1-call-eager cfg2))
    (check-equal? (length step3*) 1)
    (check-equal? (caar step3*) "call/eager-resume-goal")
    (check-false (member 'r:id (symbols-in (tree-of (cadar step3*))))))

  (test-case "Rl1-call-eager expands relation body before proceed resume"
    (define next (first (apply-reduction-relation Rl1-call-eager cfg-call)))
    (check-false (member 'r:id (symbols-in (tree-of next)))))

  (test-case "Rl1-call-lazy keeps relation call suspended under proceed"
    (define next (first (apply-reduction-relation Rl1-call-lazy cfg-call)))
    (check-not-false (member 'r:id (symbols-in (tree-of next)))))

  (test-case "Rl2-disj-left is left-biased deterministic on first answer"
    (define next (first (apply-reduction-relation Rl2-disj-left cfg-disj)))
    (check-equal?
     next
     (term (() (emit (state () () () (label "a"))
                     (⊤ (state () () () (label "b"))))
               (empty-stream)))))

  (test-case "Rdfs-nodelay matches left-biased DFS behavior without delay/proceed machinery"
    (define next (first (apply-reduction-relation dn:Rdfs-nodelay cfg-disj)))
    (check-equal?
     next
     (term (() (emit (state () () () (label "a"))
                     (⊤ (state () () () (label "b"))))
               (empty-stream)))))

  (test-case "Rl3-pre-eager and Rl3-pre-lazy both step call and disjunction configs"
    (for ([rel (in-list (list Rl3-pre-eager Rl3-pre-lazy))])
      (check-false (null? (apply-reduction-relation rel cfg-call)))
      (check-false (null? (apply-reduction-relation rel (term (() ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))
                                                             (empty-stream))))))))

  (test-case "Rl3-flip-eager and Rl3-flip-lazy perform left-only delay swap"
    (for ([rel (in-list (list Rl3-flip-eager Rl3-flip-lazy))])
      (define next (first (apply-reduction-relation rel cfg-flip)))
      (check-equal?
       next
       (term (() (delay ((⊤ (state () () () (label "b"))) <-+ (empty-tree)))
                 (empty-stream))))))

  (test-case "Rl3-flip-eager and Rl3-flip-lazy propagate delay over left disjunction before resuming proceed"
    (define cfg
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((delay
               (proceed
                ((r:id (sym "ok") (label "call"))
                 (state () () () (label "s")))))
              <-+
              (⊤ (state () () () (label "b"))))
             (empty-stream))))
    (for ([rel (in-list (list Rl3-flip-eager Rl3-flip-lazy))])
      (define named-next* (apply-reduction-relation/tag-with-names rel cfg))
      (check-equal? (length named-next*) 1)
      (check-equal? (first (first named-next*)) "flip/delay-swap-left")
      (check-equal?
       (second (first named-next*))
       (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
              (delay
               ((⊤ (state () () () (label "b")))
                <-+
                (proceed
                 ((r:id (sym "ok") (label "call"))
                  (state () () () (label "s"))))))
              (empty-stream))))))

  (test-case "Rl4-rail-eager and Rl4-rail-lazy introduce right-pointing disjunction"
    (for ([rel (in-list (list Rl4-rail-eager Rl4-rail-lazy))])
      (define next (first (apply-reduction-relation rel cfg-rail)))
      (check-equal?
       next
       (term (() (delay ((empty-tree) +-> (⊤ (state () () () (label "b")))))
                 (empty-stream))))))

  (test-case "Rl4-rail-eager and Rl4-rail-lazy propagate delay into railroad branch before resuming proceed"
    (define cfg
      (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
             ((delay
               (proceed
                ((r:id (sym "ok") (label "call"))
                 (state () () () (label "s")))))
              <-+
              (⊤ (state () () () (label "b"))))
             (empty-stream))))
    (for ([rel (in-list (list Rl4-rail-eager Rl4-rail-lazy))])
      (define named-next* (apply-reduction-relation/tag-with-names rel cfg))
      (check-equal? (length named-next*) 1)
      (check-equal? (first (first named-next*)) "rail/enter-right")
      (check-equal?
       (second (first named-next*))
       (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
              (delay
               ((proceed
                 ((r:id (sym "ok") (label "call"))
                  (state () () () (label "s"))))
                +->
                (⊤ (state () () () (label "b")))))
              (empty-stream))))))

  (test-case "Rl4-rail-eager and Rl4-rail-lazy promote +-> right answer inside <-+ context"
    (define cfg
      (term (()
             (((empty-tree) +-> (⊤ (state () () () (label "ra"))))
              <-+
              (empty-tree))
             (empty-stream))))
    (for ([rel (in-list (list Rl4-rail-eager Rl4-rail-lazy))])
      (define named-next* (apply-reduction-relation/tag-with-names rel cfg))
      (check-equal? (length named-next*) 1)
      (check-equal? (first (first named-next*)) "rail/promote-right-answer")
      (check-equal?
       (second (first named-next*))
       (term (() ((emit (state () () () (label "ra")) (empty-tree))
                  <-+
                  (empty-tree))
                (empty-stream)))))))

(define/provide-test-suite VARIANT-MODULES
  LANGUAGE-MODULES
  RELATION-MODULES)

(module+ test
  (run-tests VARIANT-MODULES))
