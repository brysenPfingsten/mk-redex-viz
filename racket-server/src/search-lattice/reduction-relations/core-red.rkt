#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/core-common.rkt"
         "./private/step-utils.rkt")

(provide core-red
         step-once)

(check-redundancy #t)

(define core-redex/search (extend-core-redex core-lang))
(define core-collector/search (make-core-collector core-lang))

(define-search-cfg/one-stage core-cfg/search core-redex/search core-lang K)

(define core-red
  (union-reduction-relations
   core-cfg/search
   core-collector/search))

(define (step-once prog)
  (step-once/deterministic core-red prog))
