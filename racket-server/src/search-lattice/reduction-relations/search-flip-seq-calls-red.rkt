#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-calls-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-calls-red.rkt")

(provide search-flip-seq-calls-red
         step-once)

(check-redundancy #t)

(define search-flip-seq-calls-red
  (extend-reduction-relation
   search-base-seq-calls-red
   search-base-seq-calls-lang
   [--> (Γ (in-hole Q (in-hole KScopePath ((delay f_1) <-+ f_2))))
        (Γ (in-hole Q (in-hole KScopePath (delay (f_2 <-+ f_1)))))
        "search-flip-seq-calls/delay-swap-left"]))

(define (step-once prog)
  (step-once/deterministic search-flip-seq-calls-red prog))
