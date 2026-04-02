#lang racket

(require redex/reduction-semantics
         "../../../languages/l3-base.rkt"
         "./core-common.rkt"
         "../support/context-pipeline.rkt")

(check-redundancy #t)

(provide core-cfg/l3)

(define core-redex/l3 (extend-core-redex L3))
(define core-collector/l3 (make-core-collector L3))

(define-cfg/two-stage core-cfg-work/l3 core-redex/l3 L3 Kconj Kdisj)

(define core-cfg/l3
  (union-reduction-relations
   core-cfg-work/l3
   core-collector/l3))
