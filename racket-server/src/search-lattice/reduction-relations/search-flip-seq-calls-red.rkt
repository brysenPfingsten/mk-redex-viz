#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-seq-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-flip-seq-calls-extra
         search-flip-seq-calls-red
         step-once)

(check-redundancy #t)

(define search-flip-seq-calls-extra
  (reduction-relation
   search-base-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KBranch ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole QShell
                      (in-hole KBranch
                               (delay (search_2
                                       <-+
                                       (in-hole QFresh runnable-search_1))))))
        "search-flip-seq-calls/delay-swap-left"]))

(define search-flip-seq-calls-red
  (union-reduction-relations
   search-base-seq-calls-red
   search-flip-seq-calls-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-seq-calls-red prog))
