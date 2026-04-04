#lang racket

(require redex/reduction-semantics
         "./search-base-lang.rkt")

(provide rail-lang)

(check-redundancy #t)

(define-extended-language rail-lang search-base-lang
  [runnable-root .... (search +-> search)]
  ;; Rail-specific active-path helper shared by both policies.
  [KTail ::= QFresh
             (KTail <-+ search)
             (search +-> KTail)]
  ;; Rail widens the inherited branch path through the right rail branch.
  ;; KLate is inherited unchanged and picks this up through its KBranch base.
  [KBranch ::= ....
               (search +-> KBranch)])
