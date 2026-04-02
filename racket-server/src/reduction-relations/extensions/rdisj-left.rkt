#lang racket

(require redex/reduction-semantics
         "./core-l2.rkt")

(check-redundancy #t)

(provide disj-extra/l2
         Rdisj-left)

(define disj-extra/l2
  (reduction-relation
    L2/K
    #:domain config
    ;; Stage 1 (inside active branch): core-conjunction contexts.
    ;; Stage 2 (outside): left-disjunction scheduler contexts.
    [--> (Γ ans* (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))))
         (Γ ans* (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))))
         "disj/goal-to-tree"]

    [--> (Γ ans* (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))))
         (Γ ans* (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))))
         "disj/distribute-over-conj"]

    [--> (Γ (σ ...) (in-hole Kleft ((⊤ σ_new) <-+ s_right)))
         (Γ (σ ... σ_new) (in-hole Kleft s_right))
         "disj/collect-left-answer"]

    [--> (Γ ans* (in-hole Kleft ((empty-tree) <-+ s_right)))
         (Γ ans* (in-hole Kleft s_right))
         "disj/skip-left-fail"]))

(define base-l2/k
  (extend-reduction-relation
    core-base-l2
    L2/K))

(define Rdisj-left
  (union-reduction-relations
   disj-extra/l2
   base-l2/k))
