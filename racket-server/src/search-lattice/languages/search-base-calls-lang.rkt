#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./search-base-lang.rkt")

(provide search-relcall-lang)

(check-redundancy #t)

(define-union-language search-relcall-lang
  relcall-lang
  search-lang)
