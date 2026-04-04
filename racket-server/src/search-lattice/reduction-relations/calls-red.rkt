#lang racket

(require redex/reduction-semantics
         "../languages/calls-lang.rkt"
         (only-in "./delay-red.rkt"
                  delay-red)
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt")

(provide calls-expand/raw
         calls-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-calls lifted-delay-red
  (extend-reduction-relation delay-red calls-lang)
  calls-lang)

(define calls-expand/raw
  (reduction-relation
   calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KLocal ((r t ... tag) σ))))
        (Γ (in-hole QShell (in-hole KLocal (g_new σ))))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "expand-relcall"]))

(define calls-red
  (union-reduction-relations
   lifted-delay-red
   calls-expand/raw))

(define (step-once prog)
  (step-once/deterministic calls-red prog))
