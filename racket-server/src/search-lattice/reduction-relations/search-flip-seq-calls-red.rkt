#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         (only-in "./search-flip-seq-red.rkt"
                  search-flip-early-extra)
         "./search-base-seq-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-flip-early-relcall-extra
         search-flip-early-relcall-red
         step-once)

(check-redundancy #t)

(define-lift-search-to-relcall search-flip-early-relcall-extra
  (extend-reduction-relation search-flip-early-extra search-relcall-lang)
  search-relcall-lang)

(define search-flip-early-relcall-red
  (union-reduction-relations
   search-early-relcall-red
   search-flip-early-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-early-relcall-red prog))
