#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./rail-lang.rkt")

(provide rail-calls-lang)

(check-redundancy #t)

(define-union-language rail-calls-lang
  calls-lang
  rail-lang)
