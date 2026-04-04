#lang racket

(require redex/reduction-semantics
         "./search-base-lang.rkt")

(provide rail-lang)

(check-redundancy #t)

(define-extended-language rail-lang search-lang
  [runnable-root .... (search +-> search)]
  ;; Rail-specific active-path helper shared by both policies.
  [RailTailCtx ::= FreshCtx
             (RailTailCtx <-+ search)
             (search +-> RailTailCtx)]
  ;; Rail widens the inherited branch path through the right rail branch.
  ;; LateCtx is inherited unchanged and picks this up through its BranchCtx base.
  [BranchCtx ::= ....
               (search +-> BranchCtx)])
