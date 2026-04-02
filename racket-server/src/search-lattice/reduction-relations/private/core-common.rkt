#lang racket

(require redex/reduction-semantics
         "../../languages/core-lang.rkt"
         "./common.rkt")

(provide core-redex/core
         extend-core-redex
         make-core-collector)

(check-redundancy #t)

(define core-redex/core
  (reduction-relation
   core-lang
   #:domain w
   [--> ((g_1 ∧ g_2 tag) (state sub dis c trail tag_1))
        ((g_1 (state sub dis c trail tag_1)) × g_2 c)
        "core/conj-distribute-state"]
   [--> ((succeed tag) σ)
        (⊤ σ)
        "core/succeed"]
   [--> ((fail tag) σ)
        (empty-tree)
        "core/fail"]
   [--> (Freshened () tag_i cfg_tail)
        cfg_tail
        "core/prune-empty-scope"]
   [--> ((Freshened c_1 tag_1 f_left) × g c_2)
        (Freshened c_1 tag_1 (f_left × g c_new))
        (where c_new ,(append (term c_1) (term c_2)))
        "core/push-scope-through-conj"]
   [--> ((Bounced + f_rest) × g c)
        (Bounced + (f_rest × g c))
        "core/preserve-bounce-event-over-conj"]
   [--> ((⊤ σ) × g c)
        (g σ)
        "core/conj-bring-success"]
   [--> ((empty-tree) × g c)
        (empty-tree)
        "core/conj-prune-fail"]
   [--> ((∃ () g tag) (state sub dis c trail tag_1))
        (g (state sub dis c trail tag_1))
        "core/elide-empty-fresh"]
   [--> ((∃ (x_first x_rest ...) g tag) (state sub dis c trail tag_1))
        (Freshened
         (u_1 ...)
         tag
         (g_new
          (state sub dis (u_1 ... ,@(term c)) trail tag_1)))
        (where ((x_bound u_1) ...)
               (fresh-substitution c (x_first x_rest ...)))
        (where g_new
               ,(subst-goal-host (term g) (term ((x_bound u_1) ...))))
        "core/fresh-substitute"]
   [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
        (⊤ (state sub_1 dis c ((t_3 =? t_4 tag_1) ... (t_1 =? t_2 tag)) tag_2))
        (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
        (where #f (invalid? sub_1 dis))
        "core/unify-success"]
   [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
        (empty-tree)
        (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
        (where #t (invalid? sub_1 dis))
        "core/unify-violates-disequality"]
   [--> ((t_1 =? t_2 tag) (state sub dis c trail tag_2))
        (empty-tree)
        (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
        "core/unify-fail"]
   [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
        (⊤ (state sub dis_1 c trail tag_2))
        (where dis_1 ((t_1 t_2) ,@(term dis)))
        (where #f (invalid? sub dis_1))
        "core/disequality-success"]
   [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
        (empty-tree)
        (where dis_1 ((t_1 t_2) ,@(term dis)))
        (where #t (invalid? sub dis_1))
        "core/disequality-fail"]))

(define-syntax-rule (extend-core-redex lang)
  (extend-reduction-relation core-redex/core lang))

(define-syntax-rule (make-core-collector lang)
  (reduction-relation
   lang
   #:domain cfg
   [--> (in-hole P (⊤ σ_new))
        (in-hole P ((⊤ σ_new) + (empty-tree)))
        "core/collect-single-answer"]))
