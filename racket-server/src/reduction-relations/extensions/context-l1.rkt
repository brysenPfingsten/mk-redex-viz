#lang racket

(require redex/reduction-semantics
         "../../extensions/l1-calls-delay.rkt")

(check-redundancy #t)

(provide L1
         L1/K)

(define-extended-language L1/K
  L1
  ;; Deterministic search context: step in conjunction's left tree only.
  ;; `delay` is an administrative barrier, so we do not descend into it.
  [K ::= hole
         (K × g c)])
