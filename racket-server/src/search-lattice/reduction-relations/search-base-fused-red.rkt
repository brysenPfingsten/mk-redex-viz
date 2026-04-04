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
          [--> (in-hole LateCtx (((in-hole FreshCtx (⊤ σ_new)) <-+ search_rest) × g c))
               (in-hole LateCtx ((in-hole FreshCtx (g σ_new)) <-+ (search_rest × g c)))
               "continue-left-answer"]
          [--> (in-hole LateCtx (((in-hole FreshCtx (empty-tree)) <-+ search_rest) × g c))
               (in-hole LateCtx (search_rest × g c))
               "continue-left-fail"])])
    (context-closure search-late-branch-local/base search-lang ShellCtx)))

(define search-late-red
  (union-reduction-relations
   search-late-pre-red
   search-late-branch-local/under-ShellCtx))

(define (step-once prog)
  (step-once/deterministic search-late-red prog))
