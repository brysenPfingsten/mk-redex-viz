#lang racket

(require redex/reduction-semantics
         "../languages/search-base-fused-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide search-dfs-fused-red
         step-once)

(check-redundancy #t)

(define search-dfs-fused-red
  (extend-reduction-relation
   search-base-fused-red
   search-base-fused-lang
   [--> ((in-hole K ((delay s_1) <-+ s_2)) as_1)
        ((in-hole K (delay (s_1 <-+ s_2))) as_1)
        "search-dfs-fused/delay-through-left"]))

(define (step-once prog)
  (step-once/deterministic search-dfs-fused-red prog))
