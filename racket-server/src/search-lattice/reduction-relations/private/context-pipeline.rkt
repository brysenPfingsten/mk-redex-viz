#lang racket

(require redex/reduction-semantics)

(provide define-lift-search-to-calls)

(define-syntax-rule (define-lift-search-to-calls name rel lang)
  (define name
    (context-closure rel lang (Γ hole))))
