#lang racket

(require redex/reduction-semantics
         "./l3-union-base.rkt")

(check-redundancy #t)

(provide L4)

;; L4 adds right-pointing disjunction for railroad variants.
(define-extended-language L4 L3
  [s .... (s +-> s)])
