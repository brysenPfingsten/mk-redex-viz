#lang racket

(require redex/reduction-semantics
         "../languages/l0.rkt"
         "../languages/l2-disjunction-left.rkt"
         "./private/core/core-l2.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl2-disj-left
         step-once)

(define disj-extra/l2
  (reduction-relation
   L2
   #:domain config
   ;; Stage 1 (inside active branch): core-conjunction contexts.
   ;; Stage 2 (outside): disjunction-spine contexts.
   [--> (Γ (in-hole Kdisj (in-hole Kconj ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kdisj (in-hole Kconj ((g_1 σ) <-+ (g_2 σ)))) as)
        "l2/goal-to-tree"]
   [--> (Γ (in-hole Kdisj (in-hole Kconj ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kdisj (in-hole Kconj ((s_1 × g c) <-+ (s_2 × g c)))) as)
        "l2/distribute-over-conj"]
   [--> (Γ (in-hole Kdisj (((⊤ σ_new) <-+ s_mid) <-+ s_right)) as)
        (Γ (in-hole Kdisj ((⊤ σ_new) <-+ (s_mid <-+ s_right))) as)
        "l2/bubble-left-answer"]
   [--> (Γ ((⊤ σ_new) <-+ s_right) as)
        (Γ s_right (append-answer as σ_new))
        "l2/promote-left-answer"]
   [--> (Γ (in-hole Kdisj (((empty-tree) <-+ s_mid) <-+ s_right)) as)
        (Γ (in-hole Kdisj ((empty-tree) <-+ (s_mid <-+ s_right))) as)
        "l2/bubble-left-fail"]
   [--> (Γ ((empty-tree) <-+ s_right) as)
        (Γ s_right as)
        "l2/skip-left-fail"]))

(define Rl2-disj-left
  (union-reduction-relations
   disj-extra/l2
   core-cfg/l2))

(define (step-once prog)
  (step-once/deterministic Rl2-disj-left prog))
