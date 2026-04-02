#lang racket

(require redex/reduction-semantics
         "./private/variants/rflip-common.rkt"
         "./l3-base-eager.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl3-flip-eager
         step-once)

(define Rl3-flip-eager
  (extend-with-flip-rules Rl3-base-eager))

(define (step-once prog)
  (step-once/deterministic Rl3-flip-eager prog))
