#lang racket

(require redex/reduction-semantics)

(provide define-search-cfg/one-stage
         define-search-cfg/two-stage
         define-calls-cfg/one-stage
         define-calls-cfg/two-stage
         define-lift-search-to-calls)

(define-syntax-rule (define-search-cfg/one-stage cfg-name rel lang ctx)
  (define cfg-name
    (context-closure
     (context-closure rel lang ctx)
     lang
     (hole as))))

(define-syntax-rule (define-search-cfg/two-stage cfg-name rel lang ctx1 ctx2)
  (define cfg-name
    (context-closure
     (context-closure
      (context-closure rel lang ctx1)
      lang
      ctx2)
     lang
     (hole as))))

(define-syntax-rule (define-calls-cfg/one-stage cfg-name rel lang ctx)
  (define cfg-name
    (context-closure
     (context-closure
      (context-closure rel lang ctx)
      lang
      (hole as))
     lang
     (Γ hole))))

(define-syntax-rule (define-calls-cfg/two-stage cfg-name rel lang ctx1 ctx2)
  (define cfg-name
    (context-closure
     (context-closure
      (context-closure
       (context-closure rel lang ctx1)
       lang
       ctx2)
      lang
      (hole as))
     lang
     (Γ hole))))

(define-syntax-rule (define-lift-search-to-calls name rel lang)
  (define name
    (context-closure rel lang (Γ hole))))
