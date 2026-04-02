#lang racket

(require redex/reduction-semantics
         "./l1-calls-delay.rkt"
         "./l2-disjunction-left.rkt")

(check-redundancy #t)

(provide L3)

;; L3 delta:
;; - union of L1 calls/delay and L2 disjunction
;; - inherited contexts: Kconj, Kdisj
(define-union-language L3/join L1 L2)

(define-extended-language L3 L3/join)
