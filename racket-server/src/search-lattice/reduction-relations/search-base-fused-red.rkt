#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-fused-pre-red.rkt"
         "./private/step-utils.rkt")

(provide search-late-red
         step-once)

(check-redundancy #t)

;; Fused-only policy steps, still wrapped by the outer committed shell.
(define search-late-branch-local/under-ShellCtx
  (let ([search-late-branch-local/base
         (reduction-relation
          search-lang
          #:domain cfg
          [--> (in-hole LateCtx ((settled_1 <-+ search_rest) × g c))
               (in-hole LateCtx ((settled_1 × g c) <-+ (search_rest × g c)))
               "distribute-over-conj"])])
    (context-closure search-late-branch-local/base search-lang ShellCtx)))

(define search-late-red
  (union-reduction-relations
   search-late-pre-red
   search-late-branch-local/under-ShellCtx))

(define (step-once prog)
  (step-once/deterministic search-late-red prog))
