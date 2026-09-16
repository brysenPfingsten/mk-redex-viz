#lang racket

(require redex/reduction-semantics
         "feature-schema.rkt"
         (for-syntax racket/base syntax/parse))
(provide define-ownerless-strict-language)

;; Native E/N syntax only. Source equations and allocation operations are
;; separately supplied by the presentation that consumes this grammar.
(define-syntax (define-ownerless-strict-language stx)
  (syntax-parse stx
    [(_ language:id parent:id supply-production
        (~optional (~seq #:feature feature:id) #:defaults ([feature #'search])))
     #:with (feature-goal ...)
     (feature-goal-productions (syntax-e #'feature) #'language)
     #:with (feature-value ...) (feature-extra-productions (syntax-e #'feature) 'SV #f #'language)
     #:with (feature-computation ...) (feature-extra-productions (syntax-e #'feature) 'c #f #'language)
     #:with (feature-context ...) (feature-extra-productions (syntax-e #'feature) 'E #f #'language)
     #:with (feature-observation ...) (feature-extra-productions (syntax-e #'feature) 'O #f #'language)
     #:with (feature-frontier ...) (feature-extra-productions (syntax-e #'feature) 'F #f #'language)
     #:with (feature-observer ...) (feature-extra-productions (syntax-e #'feature) 'o #f #'language)
     #:with (feature-observer-context ...) (feature-extra-productions (syntax-e #'feature) 'C #f #'language)
     #'(define-extended-language language parent
           [g .... feature-goal ...]
           [a eq (t != t tag) (succeed tag) (fail tag)]
           [supply supply-production]
           [SV (Empty supply) (One σ) feature-value ...]
           [c SV (eval g σ) (bind c g) feature-computation ...]
           [E hole (bind E g) feature-context ...]
           [O (Done supply) (Solo σ) feature-observation ...]
           [F (Done supply) (Solo σ) feature-frontier ...]
           [o F (render c) (commit c) (advance o) (collect o) feature-observer ...]
           [q c o]
           [C E (render E) (commit E) (advance C) (collect C)
              feature-observer-context ...])]))
