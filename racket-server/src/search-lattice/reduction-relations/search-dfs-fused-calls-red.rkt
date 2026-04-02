#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-fused-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-fused-calls-extra
         search-dfs-fused-calls-red
         step-once)

(check-redundancy #t)

(define search-dfs-fused-calls-extra
  (reduction-relation
   search-base-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KLate ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole QShell
                      (in-hole KLate
                               (delay ((in-hole QFresh runnable-search_1)
                                       <-+
                                       search_2)))))
        "search-dfs-fused-calls/delay-through-left"]))

(define search-dfs-fused-calls-red
  (union-reduction-relations
   search-base-fused-calls-red
   search-dfs-fused-calls-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-fused-calls-red prog))
