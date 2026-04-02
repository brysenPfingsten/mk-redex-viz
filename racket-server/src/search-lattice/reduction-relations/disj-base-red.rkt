#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         (only-in "./core-red.rkt"
                  extend-core-local-redex
                  extend-core-shell-redex)
         "./private/common.rkt")

(provide disj-core-local/base
         disj-core-shell/base
         disj-goal-local/base
         disj-frontier/local-base)

(check-redundancy #t)

(define core-local/disj/base
  (extend-core-local-redex disj-lang))

(define core-shell/disj/base
  (extend-core-shell-redex disj-lang))

(define disj-core-local/base
  (context-closure core-local/disj/base disj-lang KLocal))

(define disj-core-shell/base
  core-shell/disj/base)

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
        cfg_i
        (where cfg_i
               ,(tree-prefix->shell/host
                 (term (in-hole QFresh_1
                                ((in-hole QFresh_2 (⊤ σ))
                                 <-+
                                 (search_mid <-+ search_right))))))
        "disj/reassociate-left-answer"]
   [--> (in-hole QFresh_1 ((in-hole QFresh_2 (⊤ σ)) <-+ search_right))
        cfg_i
        (where promoted_i
               ,(tree-prefix->shell/host
                 (term (in-hole QFresh_2 (⊤ σ)))))
        (where cfg_i
               ,(tree-prefix->shell/host
                 (term (in-hole QFresh_1 (promoted_i + search_right)))))
        "disj/promote-left-answer"]
   [--> (in-hole QFresh_1 (((in-hole QFresh_2 (empty-tree)) <-+ search_mid) <-+ search_right))
        cfg_i
        (where cfg_i
               ,(tree-prefix->shell/host
                 (term (in-hole QFresh_1
                                (search_mid <-+ search_right)))))
        "disj/erase-left-fail"]
   [--> (in-hole QFresh_1 ((in-hole QFresh_2 (empty-tree)) <-+ search_right))
        cfg_i
        (where cfg_i
               ,(tree-prefix->shell/host
                 (term (in-hole QFresh_1 search_right))))
        "disj/skip-left-fail"]))
