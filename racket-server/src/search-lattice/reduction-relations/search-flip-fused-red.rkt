#lang racket

(require redex/reduction-semantics
         "../languages/search-base-fused-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide search-flip-fused-red
         step-once)

(check-redundancy #t)

(define search-flip-fused-red
  (extend-reduction-relation
   search-base-fused-red
   search-base-fused-lang
   [--> (in-hole Q (in-hole KScopePath (in-hole K ((delay f_1) <-+ f_2))))
        (in-hole Q (in-hole KScopePath (in-hole K (delay (f_2 <-+ f_1)))))
        "search-flip-fused/delay-swap-left"]))

(define (step-once prog)
  (step-once/deterministic search-flip-fused-red prog))
