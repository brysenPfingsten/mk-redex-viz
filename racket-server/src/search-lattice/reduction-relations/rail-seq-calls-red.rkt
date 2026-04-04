#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         (only-in "./rail-seq-red.rkt"
                  rail-early-frontier/base
                  rail-early-local/under-ShellCtx)
         "./search-base-seq-calls-red.rkt")

(provide rail-early-relcall-local/under-ShellCtx
         rail-early-relcall-frontier/base
         rail-early-relcall-red
         step-once)

(check-redundancy #t)

(define lifted-search-early-relcall-red
  (extend-reduction-relation search-early-relcall-red rail-relcall-lang))

(define-lift-search-to-relcall rail-early-relcall-local/under-ShellCtx
  (extend-reduction-relation rail-early-local/under-ShellCtx rail-relcall-lang)
  rail-relcall-lang)

(define-lift-search-to-relcall rail-early-relcall-frontier/base
  (extend-reduction-relation rail-early-frontier/base rail-relcall-lang)
  rail-relcall-lang)

(define rail-early-relcall-red
  (union-reduction-relations
   lifted-search-early-relcall-red
   rail-early-relcall-local/under-ShellCtx
   rail-early-relcall-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-early-relcall-red prog))
