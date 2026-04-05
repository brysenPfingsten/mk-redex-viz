#lang racket

(require redex/reduction-semantics
         "../languages/search-relcall-lang.rkt"
         (only-in "./search-flip-late-red.rkt"
                  search-flip-late-extra)
         "./search-late-relcall-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-flip-late-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation search-flip-late-extra search-relcall-lang)
  search-relcall-lang)

(define search-flip-late-relcall-red
  (union-reduction-relations
   search-late-relcall-red
   under-Gamma))

(define (step-once prog)
  (step-once/deterministic search-flip-late-relcall-red prog))
