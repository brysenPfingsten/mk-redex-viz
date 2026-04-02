#lang racket

(require redex/reduction-semantics
         "../../core-definitions.rkt"
         "./common.rkt")

(check-redundancy #t)

(provide core-redex/core
         whole-cfg/core
         extend-core-redex
         extend-whole-cfg)

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
         "core/unify-fail"]))

;; Shared answer-collection rule over base Core syntax.
(define whole-cfg/core
  (reduction-relation
    Core
    #:domain config
    [--> (Γ (σ ...) (⊤ σ_new))
         (Γ (σ ... σ_new) (empty-tree))
         "core/collect-answer"]))

(define-syntax-rule (extend-core-redex lang)
  (extend-reduction-relation core-redex/core lang))

(define-syntax-rule (extend-whole-cfg lang)
  (extend-reduction-relation whole-cfg/core lang))
