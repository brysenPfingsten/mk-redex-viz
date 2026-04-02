#lang racket

(require redex/reduction-semantics
         "./rail-common.rkt"
         "./rbase-e.rkt")

(check-redundancy #t)

(provide Rrail-e)

(define Rrail-e
  (extend-with-rail-rules
   (extend-reduction-relation Rbase-e L4/K)))
