#lang racket

(require redex/reduction-semantics
         "./rflip-common.rkt"
         "./rbase-e.rkt")

(check-redundancy #t)

(provide Rflip-e)

(define Rflip-e
  (extend-with-flip-rules Rbase-e))
