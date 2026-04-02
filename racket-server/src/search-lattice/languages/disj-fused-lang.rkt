#lang racket

(require redex/reduction-semantics
         "./disj-lang.rkt")

(provide disj-fused-lang)

(check-redundancy #t)

(define-extended-language disj-fused-lang disj-lang
  [K ::= hole
        (K × g c)]
  [KCorePath ::= hole
                 (Freshened c tag KCorePath)
                 (KCorePath <-+ f)]
  [KScopePath ::= hole
                  (Freshened c tag KScopePath)
                  (KScopePath <-+ f)])
