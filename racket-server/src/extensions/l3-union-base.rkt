#lang racket

(require redex/reduction-semantics
         "./l1-calls-delay.rkt")

(check-redundancy #t)

(provide L3)

;; L3 merges L1 with left-disjunction syntax.
(define-extended-language L3 L1
  [g .... (g ∨ g tag)]
  [s .... (s <-+ s)])
