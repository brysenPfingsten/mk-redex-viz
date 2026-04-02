#lang racket

(require redex/reduction-semantics
         "./private/variants/rail-common.rkt"
         "../languages/l4-railroad.rkt"
         "./l3-base-eager.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl4-rail-eager
         step-once)

(define Rl4-rail-eager
  (extend-with-rail-rules
   (extend-reduction-relation Rl3-base-eager L4)))

(define (step-once prog)
  (step-once/deterministic Rl4-rail-eager prog))
