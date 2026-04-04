#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-fused-pre-red.rkt"
         "./private/step-utils.rkt")

(provide search-base-fused-red
         step-once)

(check-redundancy #t)

;; Fused-only policy steps, still wrapped by the outer committed shell.
(define search-base-fused-branch-local/under-QShell
  (let ([search-base-fused-branch-local/base
         (reduction-relation
          search-base-lang
          #:domain cfg
          [--> (in-hole KLate (((in-hole QFresh (⊤ σ_new)) <-+ search_rest) × g c))
               (in-hole KLate ((in-hole QFresh (g σ_new)) <-+ (search_rest × g c)))
               "search-base-fused/continue-left-answer"]
          [--> (in-hole KLate (((in-hole QFresh (empty-tree)) <-+ search_rest) × g c))
               (in-hole KLate (search_rest × g c))
               "search-base-fused/continue-left-fail"])])
    (context-closure search-base-fused-branch-local/base search-base-lang QShell)))

(define search-base-fused-red
  (union-reduction-relations
   search-base-fused-pre-red
   search-base-fused-branch-local/under-QShell))

(define (step-once prog)
  (step-once/deterministic search-base-fused-red prog))
