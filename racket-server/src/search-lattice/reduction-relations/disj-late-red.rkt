#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         (prefix-in disj: "./disj-base-red.rkt")
         "./private/step-utils.rkt")

(provide disj-late-red
         step-once)

(check-redundancy #t)

(define disj-late-shared-local/under-ShellCtx
  (context-closure
   (context-closure disj:local/base disj-lang LateCtx)
   disj-lang
   ShellCtx))

(define disj-late-local/under-ShellCtx
  (let ([disj-late-local/base
         (reduction-relation
          disj-lang
          #:domain cfg
          [--> (in-hole LateCtx ((settled_1 <-+ search_rest) × g c))
               (in-hole LateCtx ((settled_1 × g c) <-+ (search_rest × g c)))
               "distribute-over-conj"])])
    (context-closure disj-late-local/base disj-lang ShellCtx)))

(define disj-late-red
  (union-reduction-relations
   disj:shell/base
   disj-late-shared-local/under-ShellCtx
   disj-late-local/under-ShellCtx))

(define (step-once prog)
  (step-once/deterministic disj-late-red prog))
