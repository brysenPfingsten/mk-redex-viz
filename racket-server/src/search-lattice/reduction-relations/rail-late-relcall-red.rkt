#lang racket

(require redex/reduction-semantics
         "../languages/rail-relcall-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         (prefix-in rail: "./rail-late-red.rkt")
         "./search-late-relcall-red.rkt")

(provide rail-late-relcall-red
         step-once)

(check-redundancy #t)

(define lifted-search-late-relcall-red
  (extend-reduction-relation search-late-relcall-red rail-relcall-lang))

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation rail:under-ShellCtx rail-relcall-lang)
  rail-relcall-lang)

(define rail-late-relcall-red
  (union-reduction-relations lifted-search-late-relcall-red under-Gamma))

(define (step-once prog)
  (step-once/deterministic rail-late-relcall-red prog))
