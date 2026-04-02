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
(define core-collector/delay (make-core-collector delay-lang))
(define-search-cfg/one-stage core-cfg/delay core-redex/delay delay-lang K)

(define delay-extra
  (reduction-relation
   delay-lang
   #:domain cfg
   [--> ((in-hole K ((suspend g tag) σ)) as_1)
        ((in-hole K (delay (g σ))) as_1)
        "delay/suspend-goal"]
   [--> ((delay s_1) as_1)
        (s_1 as_1)
        "delay/invoke-delay"]
   [--> ((in-hole K ((delay s_1) × g c)) as_1)
        ((in-hole K (delay (s_1 × g c))) as_1)
        "delay/delay-through-conj"]))

(define delay-red
  (union-reduction-relations
   delay-extra
   core-cfg/delay
   core-collector/delay))

(define (step-once prog)
  (step-once/deterministic delay-red prog))
