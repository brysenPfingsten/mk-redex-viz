#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         (only-in "./rail-fused-red.rkt"
                  rail-late-frontier/base
                  rail-late-local/under-ShellCtx)
         "./search-base-fused-calls-red.rkt")

(provide rail-late-relcall-local/under-ShellCtx
         rail-late-relcall-frontier/base
         rail-late-relcall-red
         step-once)

(check-redundancy #t)

(define lifted-search-late-relcall-red
  (extend-reduction-relation
   search-late-relcall-red
   rail-relcall-lang))

(define-lift-search-to-relcall rail-late-relcall-local/under-ShellCtx
  (extend-reduction-relation rail-late-local/under-ShellCtx rail-relcall-lang)
  rail-relcall-lang)

(define-lift-search-to-relcall rail-late-relcall-frontier/base
  (extend-reduction-relation rail-late-frontier/base rail-relcall-lang)
  rail-relcall-lang)

(define rail-late-relcall-red
  (union-reduction-relations
   lifted-search-late-relcall-red
   rail-late-relcall-local/under-ShellCtx
   rail-late-relcall-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-late-relcall-red prog))
