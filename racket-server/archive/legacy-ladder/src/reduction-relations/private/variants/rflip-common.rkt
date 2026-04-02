#lang racket

(require redex/reduction-semantics
         "../../../languages/l3-base.rkt")

(check-redundancy #t)

(provide extend-with-flip-rules)

(define (extend-with-flip-rules base-rel)
  (extend-reduction-relation
    base-rel
    L3
    [--> (Γ (in-hole Kdisj ((delay s_1) <-+ s_2)) as)
         (Γ (in-hole Kdisj (delay (s_2 <-+ s_1))) as)
         "l3-flip/delay-swap-left"]))
