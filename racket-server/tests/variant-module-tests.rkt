#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "./variant-test-support.rkt"
         (prefix-in lang: "../src/extensions/variant-languages.rkt")
         (prefix-in e: "../src/reduction-relations/extensions/rcall-eager.rkt")
         (prefix-in l: "../src/reduction-relations/extensions/rcall-lazy.rkt")
         (prefix-in d: "../src/reduction-relations/extensions/rdisj-left.rkt")
         (prefix-in be: "../src/reduction-relations/extensions/rbase-e.rkt")
         (prefix-in bl: "../src/reduction-relations/extensions/rbase-l.rkt")
         (prefix-in fe: "../src/reduction-relations/extensions/rflip-e.rkt")
         (prefix-in fl: "../src/reduction-relations/extensions/rflip-l.rkt")
         (prefix-in re: "../src/reduction-relations/extensions/rrail-e.rkt")
         (prefix-in rl: "../src/reduction-relations/extensions/rrail-l.rkt"))

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
                          +-> (⊤ (state () () () (label "b")))))))))

(define-test-suite RELATION-MODULES
  (test-case "Rcall-eager expands relation body before proceed resume"
    (define next (first (apply-reduction-relation e:Rcall-eager cfg-call)))
    (check-false (member 'r:id (symbols-in (tree-of next)))))

  (test-case "Rcall-lazy keeps relation call suspended under proceed"
    (define next (first (apply-reduction-relation l:Rcall-lazy cfg-call)))
    (check-not-false (member 'r:id (symbols-in (tree-of next)))))

  (test-case "Rdisj-left is left-biased deterministic on first answer"
    (define next (first (apply-reduction-relation d:Rdisj-left cfg-disj)))
    (check-equal?
     next
     (term (() ((state () () () (label "a")))
               (⊤ (state () () () (label "b")))))))

  (test-case "Rbase-e and Rbase-l both step call and disjunction configs"
    (for ([rel (in-list (list be:Rbase-e bl:Rbase-l))])
      (check-false (null? (apply-reduction-relation rel cfg-call)))
      (check-false (null? (apply-reduction-relation rel (term (() () ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b)))))))))

  (test-case "Rflip-e and Rflip-l perform left-only delay swap"
    (for ([rel (in-list (list fe:Rflip-e fl:Rflip-l))])
      (define next (first (apply-reduction-relation rel cfg-flip)))
      (check-equal?
       next
       (term (() () (delay ((⊤ (state () () () (label "b"))) <-+ (empty-tree))))))))

  (test-case "Rrail-e and Rrail-l introduce right-pointing disjunction"
    (for ([rel (in-list (list re:Rrail-e rl:Rrail-l))])
      (define next (first (apply-reduction-relation rel cfg-rail)))
      (check-equal?
       next
       (term (() () (delay ((empty-tree) +-> (⊤ (state () () () (label "b")))))))))))

(define/provide-test-suite VARIANT-MODULES
  LANGUAGE-MODULES
  RELATION-MODULES)

(module+ test
  (run-tests VARIANT-MODULES))
