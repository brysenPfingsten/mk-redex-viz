#lang racket

(require redex/reduction-semantics
         "../../core-definitions.rkt"
         "./common.rkt")

(check-redundancy #t)

(provide core-redex/core
         extend-core-redex
         make-core-collector)

;; Shared core stepping rules over the base Core syntax.
(define core-redex/core
  (reduction-relation
    Core
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
    [--> ((emit σ_head s_tail) × g c)
         (emit σ_head (s_tail × g c))
         "core/conj-distribute-emit"]
    [--> ((∃ d g tag) (state sub c trail tag_1))
         (g_new
          (state sub (u_1 ... ,@(term c)) trail tag_1))
         (where ((x_1 u_1) ...)
                (fresh-substitution c d))
         (where g_new
                ,(subst-goal-host (term g)
                                  (term ((x_1 u_1) ...))))
         "core/fresh-substitute"]
    [--> ((t_1 =? t_2 tag) (state sub c ((t_3 =? t_4 tag_1) ...) tag_2))
         (⊤ (state sub_1 c ((t_3 =? t_4 tag_1) ... (t_1 =? t_2 tag)) tag_2))
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         "core/unify-success"]
    [--> ((t_1 =? t_2 tag) (state sub c trail tag_2))
         (empty-tree)
         (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
         "core/unify-fail"]
    ))

(define-syntax-rule (extend-core-redex lang)
  (extend-reduction-relation core-redex/core lang))

(define-syntax-rule (make-core-collector lang)
  (reduction-relation
   lang
   #:domain config
   [--> (Γ (⊤ σ_new) as_old)
        (Γ (empty-tree) (append-answer as_old σ_new))
        "core/collect-single-answer"]
   [--> (Γ (emit σ_new s_next) as_old)
        (Γ s_next (append-answer as_old σ_new))
        "core/collect-emit"]))
