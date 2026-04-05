#lang racket

(require redex/reduction-semantics
         "../languages/search-relcall-lang.rkt"
         (only-in "./search-dfs-late-red.rkt"
                  search-dfs-late-extra)
         "./search-late-relcall-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-late-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation search-dfs-late-extra search-relcall-lang)
  search-relcall-lang)

(define search-dfs-late-relcall-red
  (union-reduction-relations
   search-late-relcall-red
   under-Gamma))

(define (step-once prog)
  (step-once/deterministic search-dfs-late-relcall-red prog))
