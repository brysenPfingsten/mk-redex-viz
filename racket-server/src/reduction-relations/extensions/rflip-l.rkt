#lang racket

(require redex/reduction-semantics
         "./rflip-common.rkt"
         "./rbase-l.rkt")

(check-redundancy #t)

(provide Rflip-l)

(define Rflip-l
  (extend-with-flip-rules Rbase-l))
