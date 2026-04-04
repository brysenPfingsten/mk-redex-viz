#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         (only-in "./search-dfs-seq-red.rkt"
                  search-dfs-early-extra)
         "./search-base-seq-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-early-relcall-extra
         search-dfs-early-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall search-dfs-early-relcall-extra
  (extend-reduction-relation search-dfs-early-extra search-relcall-lang)
  search-relcall-lang)

(define search-dfs-early-relcall-red
  (union-reduction-relations
   search-early-relcall-red
   search-dfs-early-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-early-relcall-red prog))
