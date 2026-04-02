#lang racket

(require redex/reduction-semantics
         "./core-l3.rkt"
         "./rbase-e.rkt")

(check-redundancy #t)

(provide Rflip-e)

(define Rflip-e
  (extend-reduction-relation
    Rbase-e
    L3/K
    [--> (Γ ans* (in-hole K3 ((delay s_1) <-+ s_2)))
         (Γ ans* (in-hole K3 (delay (s_2 <-+ s_1))))
         "flip/delay-swap-left"]))
