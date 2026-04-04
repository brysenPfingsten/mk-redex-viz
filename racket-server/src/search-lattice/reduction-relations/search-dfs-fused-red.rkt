#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide search-dfs-fused-red
         step-once)

(check-redundancy #t)

(define search-dfs-fused-extra
  (let ([search-dfs-fused-extra/base
         (reduction-relation
          search-base-lang
          #:domain cfg
          [--> (in-hole KLate ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))
               (in-hole KLate
                        (delay ((in-hole QFresh runnable-search_1) <-+ search_2)))
               "delay-through-left"])])
    (context-closure search-dfs-fused-extra/base search-base-lang QShell)))

(define search-dfs-fused-red
  (union-reduction-relations
   search-base-fused-red
   search-dfs-fused-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-fused-red prog))
