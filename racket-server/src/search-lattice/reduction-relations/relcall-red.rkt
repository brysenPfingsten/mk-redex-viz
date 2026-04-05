#lang racket

(require redex/reduction-semantics
         "../languages/relcall-lang.rkt"
         (only-in "./delay-red.rkt"
                  delay-red)
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt")

(provide relcall-expand/raw
         relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation delay-red relcall-lang)
  relcall-lang)

(define relcall-expand/raw
  (reduction-relation
   relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole LocalCtx ((r t ... tag) σ))))
        (Γ (in-hole ShellCtx (in-hole LocalCtx (g_new σ))))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "expand-relcall"]))

(define relcall-red
  (union-reduction-relations
   under-Gamma
   relcall-expand/raw))

(define (step-once prog)
  (step-once/deterministic relcall-red prog))
