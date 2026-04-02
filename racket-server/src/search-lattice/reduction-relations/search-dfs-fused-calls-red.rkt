#lang racket

(require redex/reduction-semantics
         "../languages/search-base-fused-calls-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-calls-red.rkt")

(provide search-dfs-fused-calls-red
         step-once)

(check-redundancy #t)

(define search-dfs-fused-calls-red
  (extend-reduction-relation
   search-base-fused-calls-red
   search-base-fused-calls-lang
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K ((delay f_1) <-+ f_2)))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (delay (f_1 <-+ f_2))))))
        "search-dfs-fused-calls/delay-through-left"]))

(define (step-once prog)
  (step-once/deterministic search-dfs-fused-calls-red prog))
