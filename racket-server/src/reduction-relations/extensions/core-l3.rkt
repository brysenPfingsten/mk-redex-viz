#lang racket

(require redex/reduction-semantics
         "./context-l3.rkt"
         "./core-common.rkt"
         "./context-pipeline.rkt")

(check-redundancy #t)

(provide L3
         L3/K
         core-cfg/l3)

(define core-redex/l3 (extend-core-redex L3))
(define core-collector/l3 (make-core-collector L3/K))

(define-cfg/two-stage core-cfg-work/l3 core-redex/l3 L3/K Kcore Kleft)

(define core-cfg/l3
  (union-reduction-relations
   core-cfg-work/l3
   core-collector/l3))
