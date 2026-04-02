#lang racket

(require redex/reduction-semantics
         "../../extensions/l2-left-disjunction.rkt"
         "../../core-definitions.rkt")

(check-redundancy #t)

(provide L2
         core-base-l2)

(define core-redex/l2
  (reduction-relation
    L2
    #:domain s
    [--> ((g_1 ∧ g_2 tag) (state sub c trail tag_1))
         ((g_1 (state sub c trail tag_1)) × g_2 c)
         "core/conj-distribute-state"]
    [--> ((succeed tag) σ)
         (⊤ σ)
         "core/succeed"]
    [--> ((⊤ σ) × g c)
         (g σ)
         "core/conj-bring-success"]
    [--> ((empty-tree) × g c)
         (empty-tree)
         "core/conj-prune-fail"]
    [--> ((∃ d g tag) (state sub c trail tag_1))
         ((subst-goal g ((x_1 u_1) ...))
          (state sub (u_1 ... ,@(term c)) trail tag_1))
         (where ((x_1 u_1) ...)
                (fresh-substitution c d))
         "core/fresh-substitute"]
    [--> ((t_1 =? t_2 tag) (state sub c ((t_3 =? t_4 tag_1) ...) tag_2))
         (⊤ (state sub_1 c ((t_3 =? t_4 tag_1) ... (t_1 =? t_2 tag)) tag_2))
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         "core/unify-success"]
    [--> ((t_1 =? t_2 tag) (state sub c trail tag_2))
         (empty-tree)
         (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
         "core/unify-fail"]))

(define core-step/l2 (compatible-closure core-redex/l2 L2 s))
(define core-cfg/l2 (context-closure core-step/l2 L2 (Γ ans* hole)))

(define whole-cfg/l2
  (reduction-relation
    L2
    #:domain config
    [--> (Γ (σ ...) (⊤ σ_new))
         (Γ (σ ... σ_new) (empty-tree))
         "core/collect-answer"]))

(define core-base-l2 (union-reduction-relations core-cfg/l2 whole-cfg/l2))
