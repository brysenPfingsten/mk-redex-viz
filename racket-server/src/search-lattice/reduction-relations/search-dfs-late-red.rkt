#lang racket

(require redex/reduction-semantics
         "../languages/search-lang.rkt"
         "./private/step-utils.rkt"
         "./search-late-red.rkt")

(provide search-dfs-late-extra
         search-dfs-late-red
         step-once)

(check-redundancy #t)

(define search-dfs-late-extra
  (let ([search-dfs-late-extra/base
         (reduction-relation
          search-lang
          #:domain cfg
          [--> (in-hole LateCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))
               (in-hole LateCtx
                        (delay ((in-hole FreshCtx runnable-search_1) <-+ search_2)))
               "delay-through-left"])])
    (context-closure search-dfs-late-extra/base search-lang ShellCtx)))

(define search-dfs-late-red
  (union-reduction-relations
   search-late-red
   search-dfs-late-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-late-red prog))
