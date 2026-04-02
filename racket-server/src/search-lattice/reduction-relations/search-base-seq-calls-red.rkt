#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-calls-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-base-seq-calls-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-calls lifted-search-base-seq-red
  (extend-reduction-relation search-base-seq-red search-base-seq-calls-lang)
  search-base-seq-calls-lang)

(define calls-expand/raw
  (reduction-relation
   search-base-seq-calls-lang
   #:domain config
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K ((r t ... tag) σ)))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (g_new σ)))))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "search-base-seq-calls/expand"]))

(define search-base-seq-calls-red
  (union-reduction-relations
   lifted-search-base-seq-red
   calls-expand/raw))

(define (step-once prog)
  (step-once/deterministic search-base-seq-calls-red prog))
