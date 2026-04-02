#lang racket

(require redex/reduction-semantics
         "../../extensions/l4-railroad-syntax.rkt"
         "./rbase-l.rkt")

(check-redundancy #t)

(provide Rrail-l)

(define-extended-language L4/K
  L4
  [K3 ::= hole
          (K3 × g c)
          (delay K3)
          (K3 <-+ s)]
  [K4 ::= hole
          (K4 × g c)
          (delay K4)
          (K4 <-+ s)
          (K4 +-> s)
          (s +-> K4)])

(define base-l/l4
  (extend-reduction-relation
    Rbase-l
    L4/K))

(define Rrail-l
  (extend-reduction-relation
    base-l/l4
    L4/K
    [--> (Γ ans* (in-hole K4 ((delay s_1) <-+ s_2)))
         (Γ ans* (in-hole K4 (delay (s_1 +-> s_2))))
         "rail/enter-right"]

    [--> (Γ ans* (in-hole K4 (s_2 +-> (delay s_1))))
         (Γ ans* (in-hole K4 (delay (s_2 <-+ s_1))))
         "rail/return-left"]

    [--> (Γ (σ ...) (s_left +-> (⊤ σ_new)))
         (Γ (σ ... σ_new) s_left)
         "rail/collect-right-answer"]

    [--> (Γ ans* (s_left +-> (empty-tree)))
         (Γ ans* s_left)
         "rail/skip-right-fail"]))
