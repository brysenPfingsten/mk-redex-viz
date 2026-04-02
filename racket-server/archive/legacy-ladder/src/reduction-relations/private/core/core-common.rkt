#lang racket

(require redex/reduction-semantics
         "../../../languages/l0.rkt"
         "../support/common.rkt")

(check-redundancy #t)

(provide core-redex/core
         extend-core-redex
         make-core-collector)

;; Shared core stepping rules over the base L0 syntax.
(define core-redex/core
  (reduction-relation
    L0
    #:domain s
    [--> ((g_1 ∧ g_2 tag) (state sub dis c trail tag_1))
         ((g_1 (state sub dis c trail tag_1)) × g_2 c)
         "l0/conj-distribute-state"]
    [--> ((succeed tag) σ)
         (⊤ σ)
         "l0/succeed"]
    [--> ((fail tag) σ)
         (empty-tree)
         "l0/fail"]
    [--> ((⊤ σ) × g c)
         (g σ)
         "l0/conj-bring-success"]
    [--> ((empty-tree) × g c)
         (empty-tree)
         "l0/conj-prune-fail"]
    [--> ((∃ d g tag) (state sub dis c trail tag_1))
         (g_new
          (state sub dis (u_1 ... ,@(term c)) trail tag_1))
         (where ((x_1 u_1) ...)
                (fresh-substitution c d))
         (where g_new
                ,(subst-goal-host (term g)
                                  (term ((x_1 u_1) ...))))
         "l0/fresh-substitute"]
    [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
         (⊤ (state sub_1 dis c ((t_3 =? t_4 tag_1) ... (t_1 =? t_2 tag)) tag_2))
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         (where #f (invalid? sub_1 dis))
         "l0/unify-success"]
    [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
         (empty-tree)
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         (where #t (invalid? sub_1 dis))
         "l0/unify-violates-disequality"]
    [--> ((t_1 =? t_2 tag) (state sub dis c trail tag_2))
         (empty-tree)
         (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
         "l0/unify-fail"]
    [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
         (⊤ (state sub dis_1 c trail tag_2))
         (where dis_1 ((t_1 t_2) ,@(term dis)))
         (where #f (invalid? sub dis_1))
         "l0/disequality-success"]
    [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
         (empty-tree)
         (where dis_1 ((t_1 t_2) ,@(term dis)))
         (where #t (invalid? sub dis_1))
         "l0/disequality-fail"]
    ))

(define-syntax-rule (extend-core-redex lang)
  (extend-reduction-relation core-redex/core lang))

(define-syntax-rule (make-core-collector lang)
  (reduction-relation
   lang
   #:domain config
   [--> (Γ (⊤ σ_new) as_old)
        (Γ (empty-tree) (append-answer as_old σ_new))
        "l0/collect-single-answer"]))
