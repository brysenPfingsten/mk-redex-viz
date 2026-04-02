#lang racket

(require redex/reduction-semantics
         "./core-l2.rkt")

(check-redundancy #t)

(provide Rdisj-left)

(define-extended-language L2/K
  L2
  [K2 ::= hole
          (K2 × g c)
          (K2 <-+ s)])

(define Rdisj-left
  (extend-reduction-relation
    core-base-l2
    L2/K
    [--> (Γ ans* (in-hole K2 ((g_1 ∨ g_2 tag) σ)))
         (Γ ans* (in-hole K2 ((g_1 σ) <-+ (g_2 σ))))
         "disj/goal-to-tree"]

    [--> (Γ ans* (in-hole K2 ((s_1 <-+ s_2) × g c)))
         (Γ ans* (in-hole K2 ((s_1 × g c) <-+ (s_2 × g c))))
         "disj/distribute-over-conj"]

    [--> (Γ (σ ...) ((⊤ σ_new) <-+ s_right))
         (Γ (σ ... σ_new) s_right)
         "disj/collect-left-answer"]

    [--> (Γ ans* ((empty-tree) <-+ s_right))
         (Γ ans* s_right)
         "disj/skip-left-fail"]))
