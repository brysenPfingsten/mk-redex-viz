#lang racket

(require redex/reduction-semantics)

(provide define-lift-search-to-relcall)

(define-syntax-rule (define-lift-search-to-relcall name rel lang)
  (define name
    (context-closure rel lang (Γ hole))))
