#lang racket

(require redex/reduction-semantics
         "./context-l3.rkt")

(check-redundancy #t)

(provide L4/K
         extend-with-rail-rules)

;; Determinism invariant:
;; railroad rules must be structurally disjoint from other scheduler rules.
;; Do not introduce dynamic precedence fences that inspect available rule names.

;; L4/K is a strict context/language extension of L3/K:
;; add right-pointing disjunction syntax and allow scheduler/strategy
;; contexts to descend through +-> positions.
(define-extended-language L4/K
  L3/K
  [s .... (s +-> s)]
  [K .... (s +-> K)]
  [Kleft .... (s +-> Kleft)]
  [Ksched .... (s +-> Ksched)])

(define (extend-with-rail-rules base-rel)
  (extend-reduction-relation
    base-rel
    L4/K
    [--> (Γ (in-hole Kdelay (delay s_1)) as)
         (Γ (in-hole Kdelay s_1) as)
         (side-condition (not (redex-match? L4/K (proceed pr) (term s_1))))
         "rail/invoke-delay"]

    [--> (Γ (in-hole Ksched ((delay s_1) <-+ s_2)) as)
         (Γ (in-hole Ksched (delay (s_1 +-> s_2))) as)
         "rail/enter-right"]

    [--> (Γ (in-hole Ksched (s_2 +-> (delay s_1))) as)
         (Γ (in-hole Ksched (delay (s_2 <-+ s_1))) as)
         "rail/return-left"]

    [--> (Γ (in-hole K (s_left +-> (emit σ_new s_right_tail))) as)
         (Γ (in-hole K (emit σ_new (s_left +-> s_right_tail))) as)
         (side-condition (not (redex-match? L4/K (empty-tree) (term s_right_tail))))
         "rail/promote-right-stream"]

    [--> (Γ (in-hole K (s_left +-> (emit σ_new (empty-tree)))) as)
         (Γ (in-hole K (emit σ_new s_left)) as)
         "rail/promote-right-singleton-stream"]

    [--> (Γ (in-hole K (s_left +-> (⊤ σ_new))) as)
         (Γ (in-hole K (emit σ_new s_left)) as)
         "rail/promote-right-answer"]

    [--> (Γ (in-hole K (s_left +-> (empty-tree))) as)
         (Γ (in-hole K s_left) as)
         "rail/skip-right-fail"]))
