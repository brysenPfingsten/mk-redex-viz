#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-base-seq-calls-expand/raw
         search-base-seq-calls-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-calls lifted-search-base-seq-red
  (extend-reduction-relation search-base-seq-red search-base-calls-lang)
  search-base-calls-lang)

(define search-base-seq-calls-expand/raw
  (reduction-relation
   search-base-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KBranch (in-hole KLocal ((r t ... tag) σ)))))
        (Γ (in-hole QShell (in-hole KBranch (in-hole KLocal (g_new σ)))))
        (where g_new
               ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
        "search-base-seq-calls/expand"]))

(define search-base-seq-calls-red
  (union-reduction-relations
   lifted-search-base-seq-red
   search-base-seq-calls-expand/raw))

(define (step-once prog)
  (step-once/deterministic search-base-seq-calls-red prog))
