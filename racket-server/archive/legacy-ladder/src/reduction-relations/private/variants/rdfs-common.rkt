#lang racket

(require redex/reduction-semantics
         "../../../languages/l3-base.rkt")

(check-redundancy #t)

(provide extend-with-dfs-rules)

(define (extend-with-dfs-rules base-rel)
  (extend-reduction-relation
   base-rel
   L3
   [--> (Γ (in-hole Kdisj ((delay s_1) <-+ s_2)) as)
        (Γ (in-hole Kdisj (delay (s_1 <-+ s_2))) as)
        "l3-dfs/delay-through-left"]))
