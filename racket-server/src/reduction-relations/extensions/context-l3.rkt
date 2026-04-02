#lang racket

(require redex/reduction-semantics
         "../../extensions/l3-union-base.rkt")

(check-redundancy #t)

(provide L3
         L3/K-base
         L3/K)

;; Shared L3 context backbone used by all L3/L4 relation deltas.
(define-extended-language L3/K-base
  L3
  ;; General strategic context used by call/disjunction extension rules.
  [K ::= hole
         (K × g c)
         (K <-+ s)]
  ;; Core reduction context: conjunction only (no disjunction or delay descent).
  [Kcore ::= hole
             (Kcore × g c)]
  ;; Left-disjunction scheduler context.
  [Kleft ::= hole
             (Kleft <-+ s)]
  ;; Scheduler context: disjunction traversal plus answer-stream tails.
  [Ksched ::= hole
              (Ksched <-+ s)])

;; Adds only the delay-invocation fence on top of the shared backbone.
(define-extended-language L3/K
  L3/K-base
  ;; Delay invocation context: top-level or under answer-stream tails only.
  [Kdelay ::= hole])
