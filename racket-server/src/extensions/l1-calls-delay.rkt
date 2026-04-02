#lang racket

(require redex/reduction-semantics
         "../core-definitions.rkt")

(check-redundancy #t)

(provide L0
         L1)

;; L0 is the base Core syntax.
(define-extended-language L0 Core)

;; L1 adds relation calls and delay/proceed administrative nodes.
(define-extended-language L1 L0
  [g .... (r t ... tag)]
  [pr ((r t ... tag) σ)
      (g σ)]
  [s .... (delay s)
     (proceed pr)])
