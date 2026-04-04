#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         "./disj-base-red.rkt"
         "./private/step-utils.rkt")

(provide disj-fused-red
         step-once)

(check-redundancy #t)

(define disj-fused-shared-local/under-QShell
  (context-closure
   (context-closure disj-local/base disj-lang KLate)
   disj-lang
   QShell))

(define disj-fused-local/under-QShell
  (let ([disj-fused-local/base
         (reduction-relation
          disj-lang
          #:domain cfg
          [--> (in-hole KLate (((in-hole QFresh (⊤ σ_new)) <-+ search_rest) × g c))
               (in-hole KLate ((in-hole QFresh (g σ_new)) <-+ (search_rest × g c)))
               "continue-left-answer"]
          [--> (in-hole KLate (((in-hole QFresh (empty-tree)) <-+ search_rest) × g c))
               (in-hole KLate (search_rest × g c))
               "continue-left-fail"])])
    (context-closure disj-fused-local/base disj-lang QShell)))

(define disj-fused-red
  (union-reduction-relations
   disj-shell/base
   disj-fused-shared-local/under-QShell
   disj-fused-local/under-QShell))

(define (step-once prog)
  (step-once/deterministic disj-fused-red prog))
