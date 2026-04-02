#lang racket

(require redex/reduction-semantics
         "../languages/search-base-fused-calls-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-calls-red.rkt")

(provide search-flip-fused-calls-red
         step-once)

(check-redundancy #t)

(define search-flip-fused-calls-red
  (extend-reduction-relation
   search-base-fused-calls-red
   search-base-fused-calls-lang
   [--> (Γ ((in-hole K ((delay s_1) <-+ s_2)) as_1))
        (Γ ((in-hole K (delay (s_2 <-+ s_1))) as_1))
        "search-flip-fused-calls/delay-swap-left"]))

(define (step-once prog)
  (step-once/deterministic search-flip-fused-calls-red prog))
