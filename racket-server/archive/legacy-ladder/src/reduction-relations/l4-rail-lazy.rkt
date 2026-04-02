#lang racket

(require redex/reduction-semantics
         "./private/variants/rail-common.rkt"
         "../languages/l4-railroad.rkt"
         "./l3-base-lazy.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl4-rail-lazy
         step-once)

(define Rl4-rail-lazy
  (extend-with-rail-rules
   (extend-reduction-relation Rl3-base-lazy L4)))

(define (step-once prog)
  (step-once/deterministic Rl4-rail-lazy prog))
