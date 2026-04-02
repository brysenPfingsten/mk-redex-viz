#lang racket

(require redex/reduction-semantics
         "./l1-calls-delay.rkt")

(check-redundancy #t)

(provide L2)

;; L2 adds disjunction goals and left-pointing tree disjunction.
(define-extended-language L2 L0
  [g .... (g ∨ g tag)]
  [s .... (s <-+ s)])
