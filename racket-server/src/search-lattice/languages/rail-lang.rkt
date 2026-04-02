#lang racket

(require redex/reduction-semantics
         "./search-base-lang.rkt")

(provide rail-lang)

(check-redundancy #t)

(define-extended-language rail-lang search-base-lang
  [runnable-root .... (search +-> search)]
  ;; Rail-specific active-path helper shared by both policies.
  [KTail ::= KLocal
             (KTail <-+ search)
             (search +-> KTail)]
  ;; Extend the shared late-strength helper through the rail right branch.
  [KLate ::= ....
             (search +-> KLate)])
