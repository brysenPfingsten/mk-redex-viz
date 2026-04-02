#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./disj-fused-lang.rkt")

(provide search-base-fused-calls-lang)

(check-redundancy #t)

(define-union-language search-base-fused-calls/join
  calls-lang
  disj-fused-lang)

(define-extended-language search-base-fused-calls-lang
  search-base-fused-calls/join)
