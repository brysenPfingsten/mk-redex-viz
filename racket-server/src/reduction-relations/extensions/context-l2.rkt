#lang racket

(require redex/reduction-semantics
         "../../extensions/l2-left-disjunction.rkt")

(check-redundancy #t)

(provide L2
         L2/K)

(define-extended-language L2/K
  L2
  ;; General strategic context used by disjunction extension rules.
  [K ::= hole
         (K × g c)
         (K <-+ s)]
  ;; Core reduction context: conjunction only (no disjunction descent).
  [Kcore ::= hole
             (Kcore × g c)]
  ;; Left-disjunction scheduler context.
  [Kleft ::= hole
             (Kleft <-+ s)])
