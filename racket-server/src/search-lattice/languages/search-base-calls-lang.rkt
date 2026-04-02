#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./search-base-lang.rkt")

(provide search-base-calls-lang)

(check-redundancy #t)

(define-union-language search-base-calls-lang
  calls-lang
  search-base-lang)
