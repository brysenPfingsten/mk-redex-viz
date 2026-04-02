#lang racket

(require redex/reduction-semantics
         "./l0.rkt")

(check-redundancy #t)

(provide L2)

;; L2 delta:
;; - goal form: disjunction
;; - search-tree form: left disjunction
;; - context: Kdisj
(define-extended-language L2 L0
  [g .... (g ∨ g tag)]
  [s .... (s <-+ s)]
  ;; Kdisj: descend through the active disjunction spine.
  [Kdisj ::= hole
             (Kdisj <-+ s)])
