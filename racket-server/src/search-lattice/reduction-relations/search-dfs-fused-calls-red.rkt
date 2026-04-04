#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         (only-in "./search-dfs-fused-red.rkt"
                  search-dfs-late-extra)
         "./search-base-fused-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-late-relcall-extra
         search-dfs-late-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall search-dfs-late-relcall-extra
  (extend-reduction-relation search-dfs-late-extra search-relcall-lang)
  search-relcall-lang)

(define search-dfs-late-relcall-red
  (union-reduction-relations
   search-late-relcall-red
   search-dfs-late-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-late-relcall-red prog))
