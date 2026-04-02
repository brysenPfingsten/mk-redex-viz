#lang racket

(require redex/reduction-semantics
         "../../extensions/l3-union-base.rkt"
         "../../core-definitions.rkt")

(check-redundancy #t)

(provide L3
         L3/K
         core-base-l3
         core-base-extra-l3)

(define core-redex/l3
  (reduction-relation
    L3
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

(define core-step/l3 (compatible-closure core-redex/l3 L3 s))
(define core-cfg/l3 (context-closure core-step/l3 L3 (Γ ans* hole)))

(define whole-cfg/l3
  (reduction-relation
    L3
    #:domain config
    [--> (Γ (σ ...) (⊤ σ_new))
         (Γ (σ ... σ_new) (empty-tree))
         "core/collect-answer"]))

(define core-base-l3 (union-reduction-relations core-cfg/l3 whole-cfg/l3))

(define-extended-language L3/K
  L3
  [K3 ::= hole
          (K3 × g c)
          (delay K3)
          (K3 <-+ s)])

(define core-base-extra-l3
  (extend-reduction-relation
    core-base-l3
    L3/K))
