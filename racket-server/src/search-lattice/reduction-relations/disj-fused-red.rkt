#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         "./disj-base-red.rkt"
         "./private/step-utils.rkt")

(provide disj-late-red
         step-once)

(check-redundancy #t)

(define disj-late-shared-local/under-ShellCtx
  (context-closure
   (context-closure disj-local/base disj-lang LateCtx)
   disj-lang
   ShellCtx))

(define disj-late-local/under-ShellCtx
  (let ([disj-late-local/base
         (reduction-relation
          disj-lang
          #:domain cfg
          [--> (in-hole LateCtx (((in-hole FreshCtx (⊤ σ_new)) <-+ search_rest) × g c))
               (in-hole LateCtx ((in-hole FreshCtx (g σ_new)) <-+ (search_rest × g c)))
               "continue-left-answer"]
          [--> (in-hole LateCtx (((in-hole FreshCtx (empty-tree)) <-+ search_rest) × g c))
               (in-hole LateCtx (search_rest × g c))
               "continue-left-fail"])])
    (context-closure disj-late-local/base disj-lang ShellCtx)))

(define disj-late-red
  (union-reduction-relations
   disj-shell/base
   disj-late-shared-local/under-ShellCtx
   disj-late-local/under-ShellCtx))

(define (step-once prog)
  (step-once/deterministic disj-late-red prog))
