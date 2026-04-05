#lang racket

(require redex/reduction-semantics
         "../languages/search-relcall-lang.rkt"
         (only-in "./search-dfs-early-red.rkt"
                  search-dfs-early-extra)
         "./search-early-relcall-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-early-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation search-dfs-early-extra search-relcall-lang)
  search-relcall-lang)

(define search-dfs-early-relcall-red
  (union-reduction-relations
   search-early-relcall-red
   under-Gamma))

(define (step-once prog)
  (step-once/deterministic search-dfs-early-relcall-red prog))
