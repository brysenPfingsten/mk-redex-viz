#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./rail-fused-lang.rkt")

(provide rail-fused-calls-lang)

(check-redundancy #t)

(define-union-language rail-fused-calls/join
  calls-lang
  rail-fused-lang)

(define-extended-language rail-fused-calls-lang
  rail-fused-calls/join)
