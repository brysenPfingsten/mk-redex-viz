#lang racket

(require redex/reduction-semantics
         "../../extensions/l1-calls-delay.rkt"
         "./core-common.rkt")

(check-redundancy #t)

(provide L1
         L1/K
         core-base-l1)

(define core-redex/l1 (extend-core-redex L1))
(define-extended-language L1/K
  L1
  ;; Deterministic search context: step in conjunction's left tree only.
  ;; `delay` is an administrative barrier, so we do not descend into it.
  [K ::= hole
         (K × g c)]
  [Kcall ::= K]
  [K1 ::= K])

(define core-step/l1 (context-closure core-redex/l1 L1/K K))
(define core-cfg/l1 (context-closure core-step/l1 L1/K (Γ ans* hole)))

(define whole-cfg/l1 (extend-whole-cfg L1/K))
(define core-base-l1 (union-reduction-relations core-cfg/l1 whole-cfg/l1))
