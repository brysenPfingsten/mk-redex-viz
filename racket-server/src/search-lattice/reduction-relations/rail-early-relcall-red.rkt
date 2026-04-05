#lang racket

(require redex/reduction-semantics
         "../languages/rail-relcall-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         (prefix-in rail: "./rail-early-red.rkt")
         "./search-early-relcall-red.rkt")

(provide rail-early-relcall-red
         step-once)

(check-redundancy #t)

(define lifted-search-early-relcall-red
  (extend-reduction-relation search-early-relcall-red rail-relcall-lang))

(define-lift-search-to-relcall under-Gamma
  (extend-reduction-relation rail:under-ShellCtx rail-relcall-lang)
  rail-relcall-lang)

(define rail-early-relcall-red
  (union-reduction-relations lifted-search-early-relcall-red under-Gamma))

(define (step-once prog)
  (step-once/deterministic rail-early-relcall-red prog))
