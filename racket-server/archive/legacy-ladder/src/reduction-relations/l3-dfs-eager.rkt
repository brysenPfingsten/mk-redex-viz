#lang racket

(require redex/reduction-semantics
         "./private/variants/rdfs-common.rkt"
         "./l3-base-eager.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl3-dfs-eager
         step-once)

(define Rl3-dfs-eager
  (extend-with-dfs-rules Rl3-base-eager))

(define (step-once prog)
  (step-once/deterministic Rl3-dfs-eager prog))
