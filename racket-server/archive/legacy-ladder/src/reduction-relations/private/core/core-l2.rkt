#lang racket

(require redex/reduction-semantics
         "../../../languages/l2-disjunction-left.rkt"
         "./core-common.rkt"
         "../support/context-pipeline.rkt")

(check-redundancy #t)

(provide core-cfg/l2)

(define core-redex/l2 (extend-core-redex L2))
(define core-collector/l2 (make-core-collector L2))

(define-cfg/two-stage core-cfg-work/l2 core-redex/l2 L2 Kconj Kdisj)

(define core-cfg/l2
  (union-reduction-relations
   core-cfg-work/l2
   core-collector/l2))
