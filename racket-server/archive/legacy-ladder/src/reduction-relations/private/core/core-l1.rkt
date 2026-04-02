#lang racket

(require redex/reduction-semantics
         "../../../languages/l1-calls-delay.rkt"
         "./core-common.rkt"
         "../support/context-pipeline.rkt")

(check-redundancy #t)

(provide core-cfg/l1)

(define core-redex/l1 (extend-core-redex L1))
(define core-collector/l1 (make-core-collector L1))

(define-cfg/one-stage core-cfg-work/l1 core-redex/l1 L1 Kconj)

(define core-cfg/l1
  (union-reduction-relations
   core-cfg-work/l1
   core-collector/l1))
