#lang racket

(require redex/reduction-semantics
         "./context-l1.rkt"
         "./core-common.rkt"
         "./context-pipeline.rkt")

(check-redundancy #t)

(provide L1
         L1/K
         core-cfg/l1)

(define core-redex/l1 (extend-core-redex L1))
(define core-collector/l1 (make-core-collector L1/K))

(define-cfg/one-stage core-cfg-work/l1 core-redex/l1 L1/K K)

(define core-cfg/l1
  (union-reduction-relations
   core-cfg-work/l1
   core-collector/l1))
