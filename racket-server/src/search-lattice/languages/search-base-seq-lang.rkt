#lang racket

(require redex/reduction-semantics
         "./delay-lang.rkt"
         "./disj-seq-lang.rkt")

(provide search-base-seq-lang)

(check-redundancy #t)

(define-union-language search-base-seq/join
  delay-lang
  disj-seq-lang)

(define-extended-language search-base-seq-lang
  search-base-seq/join)
