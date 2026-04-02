#lang racket

(require redex/reduction-semantics
         "../languages/search-base-fused-calls-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide search-base-fused-calls-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-calls lifted-search-base-fused-red
  (extend-reduction-relation search-base-fused-red search-base-fused-calls-lang)
  search-base-fused-calls-lang)

(define calls-expand/raw
  (reduction-relation
   search-base-fused-calls-lang
   #:domain config
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K ((r t ... tag) σ)))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (g_new σ)))))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "search-base-fused-calls/expand"]))

(define search-base-fused-calls-red
  (union-reduction-relations
   lifted-search-base-fused-red
   calls-expand/raw))

(define (step-once prog)
  (step-once/deterministic search-base-fused-calls-red prog))
