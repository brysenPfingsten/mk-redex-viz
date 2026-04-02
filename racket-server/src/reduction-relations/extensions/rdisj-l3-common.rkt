#lang racket

(require redex/reduction-semantics
         "../../core-definitions.rkt"
         "./context-l3.rkt")

(check-redundancy #t)

(provide make-disj-extra/l3)

(define (make-disj-extra/l3)
  (reduction-relation
   L3/K
   #:domain config
   [--> (Γ (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))) as)
        "disj/goal-to-tree"]
   [--> (Γ (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))) as)
        (side-condition (redex-match? L3/K s (term s_1)))
        (side-condition (redex-match? L3/K s (term s_2)))
        "disj/distribute-over-conj"]
   [--> (Γ (in-hole Kleft (((⊤ σ_new) <-+ s_mid) <-+ s_right)) as)
        (Γ (in-hole Kleft ((⊤ σ_new) <-+ (s_mid <-+ s_right))) as)
        "disj/bubble-left-answer"]
   [--> (Γ ((⊤ σ_new) <-+ s_right) as)
        (Γ s_right (append-answer as σ_new))
        "disj/promote-left-answer"]
   [--> (Γ (in-hole Kleft (((empty-tree) <-+ s_mid) <-+ s_right)) as)
        (Γ (in-hole Kleft ((empty-tree) <-+ (s_mid <-+ s_right))) as)
        "disj/bubble-left-fail"]
   [--> (Γ ((empty-tree) <-+ s_right) as)
        (Γ s_right as)
        "disj/skip-left-fail"]))
