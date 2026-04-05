#lang racket

(require redex/reduction-semantics
         "../languages/search-relcall-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         "./search-late-red.rkt")

(provide search-late-relcall-expand/raw
         search-late-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation search-late-red search-relcall-lang)
  search-relcall-lang)

(define search-late-relcall-expand/raw
  (reduction-relation
   search-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (in-hole LocalCtx ((r t ... tag) σ)))))
        (Γ (in-hole ShellCtx (in-hole LateCtx (in-hole LocalCtx (g_new σ)))))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "expand-relcall"]))

(define search-late-relcall-red
  (union-reduction-relations
   under-Gamma
   search-late-relcall-expand/raw))

(define (step-once prog)
  (step-once/deterministic search-late-relcall-red prog))
