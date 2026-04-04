#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-dfs-early-red
         step-once)

(check-redundancy #t)

(define search-dfs-early-extra
  (let ([search-dfs-early-extra/base
         (reduction-relation
          search-lang
          #:domain cfg
          [--> (in-hole BranchCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))
               (in-hole BranchCtx
                        (delay ((in-hole FreshCtx runnable-search_1) <-+ search_2)))
               "delay-through-left"])])
    (context-closure search-dfs-early-extra/base search-lang ShellCtx)))

(define search-dfs-early-red
  (union-reduction-relations
   search-early-red
   search-dfs-early-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-early-red prog))
