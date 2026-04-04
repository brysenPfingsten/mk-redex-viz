#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         (only-in "../languages/core-lang.rkt"
                  fresh-tree-prefix->shell-prefix)
         (only-in "./core-red.rkt"
                  core-local/base
                  core-shell/base))

(provide disj-core-local/base
         disj-core-shell/base
         disj-goal-local/base
         disj-frontier/local-base)

(check-redundancy #t)

(define disj-core-local/base
  (context-closure
   (extend-reduction-relation core-local/base disj-lang)
   disj-lang
   KLocal))

(define disj-core-shell/base
  (extend-reduction-relation core-shell/base disj-lang))

(define disj-goal-local/base
  (reduction-relation
   disj-lang
   #:domain cfg
   [--> (in-hole KLocal ((g_1 ∨ g_2 tag) σ))
        (in-hole KLocal ((g_1 σ) <-+ (g_2 σ)))
        "disj/goal-to-tree"]))

(define disj-frontier/local-base
  (reduction-relation
   disj-lang
   #:domain cfg
   [--> (in-hole QFresh_1 (((in-hole QFresh_2 (⊤ σ)) <-+ search_mid) <-+ search_right))
        (fresh-tree-prefix->shell-prefix
         (in-hole QFresh_1 ((in-hole QFresh_2 (⊤ σ)) <-+ (search_mid <-+ search_right))))
        "disj/reassociate-left-answer"]
   [--> (in-hole QFresh_1 ((in-hole QFresh_2 (⊤ σ)) <-+ search_right))
        (fresh-tree-prefix->shell-prefix
         (in-hole QFresh_1 ((in-hole QFresh_2 (⊤ σ)) + search_right)))
        "disj/promote-left-answer"]
   [--> (in-hole QFresh_1 (((in-hole QFresh_2 (empty-tree)) <-+ search_mid) <-+ search_right))
        (fresh-tree-prefix->shell-prefix (in-hole QFresh_1 (search_mid <-+ search_right)))
        "disj/erase-left-fail"]
   [--> (in-hole QFresh_1 ((in-hole QFresh_2 (empty-tree)) <-+ search_right))
        (fresh-tree-prefix->shell-prefix (in-hole QFresh_1 search_right))
        "disj/skip-left-fail"]))
