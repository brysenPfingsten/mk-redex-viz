#lang racket

(require redex/reduction-semantics
         "../../extensions/l2-left-disjunction.rkt"
         "./core-common.rkt")

(check-redundancy #t)

(provide L2
         L2/K
         core-base-l2)

(define core-redex/l2 (extend-core-redex L2))
(define-extended-language L2/K
  L2
  ;; General strategic context used by disjunction extension rules.
  [K ::= hole
         (K × g c)
         (K <-+ s)]
  ;; Core reduction context: conjunction only (no disjunction descent).
  [Kcore ::= hole
             (Kcore × g c)]
  ;; Left-disjunction scheduler context.
  [Kleft ::= hole
             (Kleft <-+ s)]
  [K2 ::= K])

(define core-step/base-l2 (context-closure core-redex/l2 L2/K Kcore))
(define core-step/l2 (context-closure core-step/base-l2 L2/K Kleft))
(define core-cfg/l2 (context-closure core-step/l2 L2/K (Γ ans* hole)))

(define whole-cfg/l2 (extend-whole-cfg L2/K))
(define core-base-l2 (union-reduction-relations core-cfg/l2 whole-cfg/l2))
