#lang racket

(require redex/reduction-semantics
         "../../extensions/l3-union-base.rkt"
         "./core-common.rkt")

(check-redundancy #t)

(provide L3
         L3/K
         core-base-l3
         core-base-extra-l3)

(define core-redex/l3 (extend-core-redex L3))
(define-extended-language L3/K
  L3
  ;; General strategic context used by call/disjunction extension rules.
  [K ::= hole
         (K × g c)
         (K <-+ s)]
  ;; Core reduction context: conjunction only (no disjunction or delay descent).
  [Kcore ::= hole
             (Kcore × g c)]
  ;; Left-disjunction scheduler context.
  [Kleft ::= hole
             (Kleft <-+ s)]
  ;; Call-step base context; left-disjunction lifting is applied separately.
  [Kcall ::= hole
             (Kcall × g c)]
  ;; Administrative delay invocation context (no disjunction descent).
  [Kinvoke ::= hole
               (Kinvoke × g c)]
  [K3 ::= K])

(define core-step/base-l3 (context-closure core-redex/l3 L3/K Kcore))
(define core-step/l3 (context-closure core-step/base-l3 L3/K Kleft))
(define core-cfg/l3 (context-closure core-step/l3 L3/K (Γ ans* hole)))

(define whole-cfg/l3 (extend-whole-cfg L3/K))
(define core-base-l3 (union-reduction-relations core-cfg/l3 whole-cfg/l3))

(define core-base-extra-l3
  (extend-reduction-relation
    core-base-l3
    L3/K))
