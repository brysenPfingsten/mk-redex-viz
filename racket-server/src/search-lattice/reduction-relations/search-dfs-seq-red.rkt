#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-dfs-seq-red
         step-once)

(check-redundancy #t)

(define search-dfs-seq-extra
  (let ([search-dfs-seq-extra/base
         (reduction-relation
          search-base-lang
          #:domain cfg
          [--> (in-hole KBranch ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))
               (in-hole KBranch
                        (delay ((in-hole QFresh runnable-search_1) <-+ search_2)))
               "search-dfs-seq/delay-through-left"])])
    (context-closure search-dfs-seq-extra/base search-base-lang QShell)))

(define search-dfs-seq-red
  (union-reduction-relations
   search-base-seq-red
   search-dfs-seq-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-seq-red prog))
