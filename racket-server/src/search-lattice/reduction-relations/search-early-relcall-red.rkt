#lang racket

(require redex/reduction-semantics
         "../languages/search-relcall-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         "./search-early-red.rkt")

(provide search-early-relcall-expand/raw
         search-early-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation search-early-red search-relcall-lang)
  search-relcall-lang)

(define search-early-relcall-expand/raw
  (reduction-relation
   search-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole BranchCtx (in-hole LocalCtx ((r t ... tag) σ)))))
        (Γ (in-hole ShellCtx (in-hole BranchCtx (in-hole LocalCtx (g_new σ)))))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "expand-relcall"]))

(define search-early-relcall-red
  (union-reduction-relations
   under-Gamma
   search-early-relcall-expand/raw))

(define (step-once prog)
  (step-once/deterministic search-early-relcall-red prog))
