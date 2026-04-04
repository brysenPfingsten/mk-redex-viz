#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         "./disj-base-red.rkt"
         "./private/step-utils.rkt")

(provide disj-seq-red
         step-once)

(check-redundancy #t)

(define disj-seq-shared-local/under-QShell
  (context-closure
   (context-closure disj-local/base disj-lang KBranch)
   disj-lang
   QShell))

(define disj-seq-local/under-QShell
  (let ([disj-seq-local/base
         (reduction-relation
          disj-lang
          #:domain cfg
          [--> (in-hole KBranch ((search_1 <-+ search_2) × g c))
               (in-hole KBranch ((search_1 × g c) <-+ (search_2 × g c)))
               "disj-seq/distribute-over-conj"])])
    (context-closure disj-seq-local/base disj-lang QShell)))

(define disj-seq-red
  (union-reduction-relations
   disj-shell/base
   disj-seq-shared-local/under-QShell
   disj-seq-local/under-QShell))

(define (step-once prog)
  (step-once/deterministic disj-seq-red prog))
