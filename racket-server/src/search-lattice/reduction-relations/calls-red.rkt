#lang racket

(require redex/reduction-semantics
         "../languages/calls-lang.rkt"
         "./delay-red.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt")

(provide calls-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-calls lifted-delay-red
  (extend-reduction-relation delay-red calls-lang)
  calls-lang)

(define calls-extra
  (reduction-relation
   calls-lang
   #:domain config
   [--> (Γ ((in-hole K ((r t ... tag) σ)) as_1))
        (Γ ((in-hole K (g_new σ)) as_1))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "calls/expand"]))

(define calls-red
  (union-reduction-relations
   lifted-delay-red
   calls-extra))

(define (step-once prog)
  (step-once/deterministic calls-red prog))
