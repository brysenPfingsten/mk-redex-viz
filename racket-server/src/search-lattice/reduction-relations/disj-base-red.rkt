#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         (only-in "../languages/core-lang.rkt"
                  fresh-tree-prefix->shell-prefix)
         (only-in "./core-red.rkt"
                  core-local/base
                  core-shell/base))

(provide disj-goal-local/base
         disj-frontier/base
         disj-local/base
         disj-shell/base)

(check-redundancy #t)

(define disj-goal-local/base
  (reduction-relation
   disj-lang
   #:domain cfg
   [--> (in-hole KLocal ((g_1 ∨ g_2 tag) σ))
        (in-hole KLocal ((g_1 σ) <-+ (g_2 σ)))
        "disj/goal-to-tree"]))

(define disj-frontier/base
  (let ([disj-frontier/local-base
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
                "disj/skip-left-fail"])])
    (context-closure disj-frontier/local-base disj-lang QShell)))

(define disj-local/base
  (union-reduction-relations
   (context-closure
    (extend-reduction-relation core-local/base disj-lang)
    disj-lang
    KLocal)
   disj-goal-local/base))

(define disj-shell/base
  (union-reduction-relations
   (extend-reduction-relation core-shell/base disj-lang)
   disj-frontier/base))
