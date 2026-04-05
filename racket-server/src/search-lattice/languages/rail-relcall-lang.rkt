#lang racket

(require redex/reduction-semantics
         "./relcall-lang.rkt"
         "./rail-lang.rkt")

(provide rail-relcall-lang)

(check-redundancy #t)

(define-union-language rail-relcall-lang
  relcall-lang
  rail-lang)
