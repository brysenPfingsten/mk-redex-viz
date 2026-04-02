#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./disj-seq-lang.rkt")

(provide search-base-seq-calls-lang)

(check-redundancy #t)

(define-union-language search-base-seq-calls/join
  calls-lang
  disj-seq-lang)

(define-extended-language search-base-seq-calls-lang
  search-base-seq-calls/join
  [K ::= hole
        (K × g c)]
  [KCorePath ::= hole
                 (Freshened c tag KCorePath)
                 (KCorePath <-+ f)]
  [KScopePath ::= hole
                  (Freshened c tag KScopePath)
                  (KScopePath <-+ f)])
