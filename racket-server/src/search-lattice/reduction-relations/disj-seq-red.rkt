#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         "./disj-base-red.rkt"
         "./private/step-utils.rkt")

(provide disj-early-red
         step-once)

(check-redundancy #t)

(define disj-early-shared-local/under-ShellCtx
  (context-closure
   (context-closure disj-local/base disj-lang BranchCtx)
   disj-lang
   ShellCtx))

(define disj-early-local/under-ShellCtx
  (let ([disj-early-local/base
         (reduction-relation
          disj-lang
          #:domain cfg
          [--> (in-hole BranchCtx ((search_1 <-+ search_2) × g c))
               (in-hole BranchCtx ((search_1 × g c) <-+ (search_2 × g c)))
               "distribute-over-conj"])])
    (context-closure disj-early-local/base disj-lang ShellCtx)))

(define disj-early-red
  (union-reduction-relations
   disj-shell/base
   disj-early-shared-local/under-ShellCtx
   disj-early-local/under-ShellCtx))

(define (step-once prog)
  (step-once/deterministic disj-early-red prog))
