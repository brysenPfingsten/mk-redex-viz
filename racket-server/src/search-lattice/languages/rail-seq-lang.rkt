#lang racket

(require redex/reduction-semantics
         "./search-base-seq-lang.rkt")

(provide rail-seq-lang)

(check-redundancy #t)

(define-extended-language rail-seq-lang search-base-seq-lang
  [s .... (s +-> s)]
  [KDisj .... (s +-> KDisj)])
