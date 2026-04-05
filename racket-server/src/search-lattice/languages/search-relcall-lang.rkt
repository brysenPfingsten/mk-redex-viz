#lang racket

(require redex/reduction-semantics
         "./relcall-lang.rkt"
         "./search-lang.rkt")

(provide search-relcall-lang)

(check-redundancy #t)

(define-union-language search-relcall-lang
  relcall-lang
  search-lang)
