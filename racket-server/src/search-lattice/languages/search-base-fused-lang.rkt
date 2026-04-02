#lang racket

(require redex/reduction-semantics
         "./delay-lang.rkt"
         "./disj-fused-lang.rkt")

(provide search-base-fused-lang)

(check-redundancy #t)

(define-union-language search-base-fused/join
  delay-lang
  disj-fused-lang)

(define-extended-language search-base-fused-lang
  search-base-fused/join)
