#lang racket

(require redex/reduction-semantics
         "./disj-lang.rkt")

(provide disj-seq-lang)

(check-redundancy #t)

(define-extended-language disj-seq-lang disj-lang
  [KDisj ::= hole
             (KDisj <-+ s)])
