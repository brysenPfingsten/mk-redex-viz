#lang racket

(require redex/reduction-semantics
         "./delay-lang.rkt"
         "./disj-lang.rkt")

(provide search-lang)

(check-redundancy #t)

;; Runtime join of delay and neutral disjunction.
;; This is the primary L3 join node for the L0-L3 semilattice.
(define-union-language search-lang
  delay-lang
  disj-lang)
