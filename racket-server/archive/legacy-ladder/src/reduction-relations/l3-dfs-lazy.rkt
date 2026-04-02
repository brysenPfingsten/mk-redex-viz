#lang racket

(require redex/reduction-semantics
         "./private/variants/rdfs-common.rkt"
         "./l3-base-lazy.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl3-dfs-lazy
         step-once)

(define Rl3-dfs-lazy
  (extend-with-dfs-rules Rl3-base-lazy))

(define (step-once prog)
  (step-once/deterministic Rl3-dfs-lazy prog))
