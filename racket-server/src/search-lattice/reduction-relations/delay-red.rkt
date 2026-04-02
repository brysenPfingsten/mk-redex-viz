#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/core-common.rkt"
         "./private/step-utils.rkt")

(provide delay-red
         step-once)

(check-redundancy #t)

(define core-redex/delay (extend-core-redex delay-lang))
(define-search-frontier/one-stage core-frontier/delay core-redex/delay delay-lang K)

(define delay-local
  (reduction-relation
   delay-lang
   #:domain f
   [--> (in-hole K ((suspend g tag) σ))
        (in-hole K (delay (g σ)))
        "delay/suspend-goal"]
   [--> (in-hole K ((delay f_1) × g c))
        (in-hole K (delay (f_1 × g c)))
        "delay/delay-through-conj"]))

(define delay-frontier-extra
  (reduction-relation
   delay-lang
   #:domain cfg
   [--> (in-hole Q (delay f_1))
        (in-hole Q (Bounced + f_1))
        "delay/invoke-delay"]))

(define delay-extra
  (context-closure delay-local delay-lang Q))

(define delay-red
  (union-reduction-relations
   delay-frontier-extra
   core-frontier/delay
   delay-extra))

(define (step-once prog)
  (step-once/deterministic delay-red prog))
