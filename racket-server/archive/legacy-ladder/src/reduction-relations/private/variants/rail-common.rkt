#lang racket

(require redex/reduction-semantics
         "../../../languages/l0.rkt"
         "../../../languages/l4-railroad.rkt")

(check-redundancy #t)

(provide extend-with-rail-rules)

;; Determinism invariant:
;; railroad rules must be structurally disjoint from other scheduler rules.
;; Do not introduce dynamic precedence fences that inspect available rule names.

(define (extend-with-rail-rules base-rel)
  (extend-reduction-relation
    base-rel
    L4
    [--> (Γ (in-hole Kdisj ((delay s_1) <-+ s_2)) as)
         (Γ (in-hole Kdisj (delay (s_1 +-> s_2))) as)
         "l4-rail/enter-right"]

    [--> (Γ (in-hole Kdisj (s_2 +-> (delay s_1))) as)
         (Γ (in-hole Kdisj (delay (s_2 <-+ s_1))) as)
         "l4-rail/return-left"]

    [--> (Γ (in-hole Kdisj (in-hole Kconj (s_left +-> ((⊤ σ_new) <-+ s_right)))) as)
         (Γ (in-hole Kdisj (in-hole Kconj (s_left +-> s_right)))
            (append-answer as σ_new))
         "l4-rail/promote-right-left-answer"]

    [--> (Γ (in-hole Kdisj (in-hole Kconj (s_left +-> ((empty-tree) <-+ s_right)))) as)
         (Γ (in-hole Kdisj (in-hole Kconj (s_left +-> s_right))) as)
         "l4-rail/skip-right-left-fail"]

    [--> (Γ (in-hole Kdisj (in-hole Kconj (s_left +-> (⊤ σ_new)))) as)
         (Γ (in-hole Kdisj (in-hole Kconj s_left))
            (append-answer as σ_new))
         "l4-rail/promote-right-answer"]

    [--> (Γ (in-hole Kdisj (in-hole Kconj (s_left +-> (empty-tree)))) as)
         (Γ (in-hole Kdisj (in-hole Kconj s_left)) as)
         "l4-rail/skip-right-fail"]))
