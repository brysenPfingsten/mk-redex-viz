#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         (only-in "./search-flip-fused-red.rkt"
                  search-flip-late-extra)
         "./search-base-fused-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-flip-late-relcall-extra
         search-flip-late-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall search-flip-late-relcall-extra
  (extend-reduction-relation search-flip-late-extra search-relcall-lang)
  search-relcall-lang)

(define search-flip-late-relcall-red
  (union-reduction-relations
   search-late-relcall-red
   search-flip-late-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-late-relcall-red prog))
