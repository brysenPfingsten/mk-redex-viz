#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide search-flip-fused-red
         step-once)

(check-redundancy #t)

(define search-flip-fused-extra
  (let ([search-flip-fused-extra/base
         (reduction-relation
          search-base-lang
          #:domain cfg
          [--> (in-hole KLate ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))
               (in-hole KLate
                        (delay (search_2 <-+ (in-hole QFresh runnable-search_1))))
               "search-flip-fused/delay-swap-left"])])
    (context-closure search-flip-fused-extra/base search-base-lang QShell)))

(define search-flip-fused-red
  (union-reduction-relations
   search-base-fused-red
   search-flip-fused-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-fused-red prog))
