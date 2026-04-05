#lang racket

(require redex/reduction-semantics
         "./search-lang.rkt")

(provide rail-lang)

(check-redundancy #t)

(define-extended-language rail-lang search-lang
  [runnable-root .... (search +-> search)]
  ;; Rail widens the inherited branch path through the right rail branch.
  ;; Local and frontier rail rules both reuse this extended branch context.
  [BranchCtx ::= ....
               (search +-> BranchCtx)])
