#lang racket

(require redex/reduction-semantics
         "../../../languages/l0.rkt"
         "../../../languages/l3-base.rkt")

(check-redundancy #t)

(provide disj-extra/l3)

(define disj-extra/l3
  (reduction-relation
   L3
   #:domain config
   [--> (Γ (in-hole Kdisj (in-hole Kconj ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kdisj (in-hole Kconj ((g_1 σ) <-+ (g_2 σ)))) as)
        "l3-base/goal-to-tree"]
   [--> (Γ (in-hole Kdisj (in-hole Kconj ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kdisj (in-hole Kconj ((s_1 × g c) <-+ (s_2 × g c)))) as)
        (side-condition (redex-match? L3 s (term s_1)))
        (side-condition (redex-match? L3 s (term s_2)))
        "l3-base/distribute-over-conj"]
   [--> (Γ (in-hole Kdisj (((⊤ σ_new) <-+ s_mid) <-+ s_right)) as)
        (Γ (in-hole Kdisj ((⊤ σ_new) <-+ (s_mid <-+ s_right))) as)
        "l3-base/bubble-left-answer"]
   [--> (Γ ((⊤ σ_new) <-+ s_right) as)
        (Γ s_right (append-answer as σ_new))
        "l3-base/promote-left-answer"]
   [--> (Γ (in-hole Kdisj (((empty-tree) <-+ s_mid) <-+ s_right)) as)
        (Γ (in-hole Kdisj ((empty-tree) <-+ (s_mid <-+ s_right))) as)
        "l3-base/bubble-left-fail"]
   [--> (Γ ((empty-tree) <-+ s_right) as)
        (Γ s_right as)
        "l3-base/skip-left-fail"]))
