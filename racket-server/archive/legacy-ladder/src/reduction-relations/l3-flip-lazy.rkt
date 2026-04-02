#lang racket

(require redex/reduction-semantics
         "./private/variants/rflip-common.rkt"
         "./l3-base-lazy.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl3-flip-lazy
         step-once)

(define Rl3-flip-lazy
  (extend-with-flip-rules Rl3-base-lazy))

(define (step-once prog)
  (step-once/deterministic Rl3-flip-lazy prog))
