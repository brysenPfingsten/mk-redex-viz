#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-fused-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-flip-fused-calls-extra
         search-flip-fused-calls-red
         step-once)

(check-redundancy #t)

(define search-flip-fused-calls-extra
  (reduction-relation
   search-base-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KLate ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole QShell
                      (in-hole KLate
                               (delay (search_2
                                       <-+
                                       (in-hole QFresh runnable-search_1))))))
        "delay-swap-left"]))

(define search-flip-fused-calls-red
  (union-reduction-relations
   search-base-fused-calls-red
   search-flip-fused-calls-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-fused-calls-red prog))
