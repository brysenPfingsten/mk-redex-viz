#lang racket

(require redex/reduction-semantics
         "./l1-calls-delay.rkt"
         "./l2-left-disjunction.rkt")

(check-redundancy #t)

(provide L3)

;; L3 is the syntax union of L1 and L2.
(define-union-language L3 L1 L2)
