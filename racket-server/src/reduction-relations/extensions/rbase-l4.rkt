#lang racket

(require redex/reduction-semantics
         "./rail-common.rkt"
         "./rbase-l.rkt")

(check-redundancy #t)

(provide Rbase-l4)

;; Railroad-syntax projection of the non-interleaving base relation.
;; This keeps the scheduler left-biased/deterministic while allowing L4 terms.
(define Rbase-l4
  (extend-reduction-relation
    Rbase-l
    L4/K))

