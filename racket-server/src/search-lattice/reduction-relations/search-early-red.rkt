#lang racket

(require redex/reduction-semantics
         "../languages/search-lang.rkt"
         "./search-early-pre-red.rkt"
         "./private/step-utils.rkt")

(provide search-early-red
         step-once)

(check-redundancy #t)

;; Seq-only policy step, still wrapped by the outer committed shell.
(define search-early-branch-local/under-ShellCtx
  (let ([search-early-branch-local/base
         (reduction-relation
          search-lang
          #:domain cfg
          [--> (in-hole BranchCtx ((search_1 <-+ search_2) × g c))
               (in-hole BranchCtx ((search_1 × g c) <-+ (search_2 × g c)))
               "distribute-over-conj"])])
    (context-closure search-early-branch-local/base search-lang ShellCtx)))

(define search-early-red
  (union-reduction-relations
   search-early-pre-red
   search-early-branch-local/under-ShellCtx))

(define (step-once prog)
  (step-once/deterministic search-early-red prog))
