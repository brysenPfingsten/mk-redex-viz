#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-seq-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-seq-calls-extra
         search-dfs-seq-calls-red
         step-once)

(check-redundancy #t)

(define search-dfs-seq-calls-extra
  (reduction-relation
   search-base-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KBranch ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole QShell
                      (in-hole KBranch
                               (delay ((in-hole QFresh runnable-search_1)
                                       <-+
                                       search_2)))))
        "search-dfs-seq-calls/delay-through-left"]))

(define search-dfs-seq-calls-red
  (union-reduction-relations
   search-base-seq-calls-red
   search-dfs-seq-calls-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-seq-calls-red prog))
