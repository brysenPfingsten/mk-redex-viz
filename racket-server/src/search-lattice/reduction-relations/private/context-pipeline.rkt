#lang racket

(require redex/reduction-semantics
         "./core-common.rkt")

(provide define-search-frontier/one-stage
         define-search-frontier/one-stage/no-collector
         define-search-frontier/two-stage
         define-search-frontier/three-stage
         define-search-frontier/two-stage/no-collector
         define-search-frontier/three-stage/no-collector
         define-calls-frontier/one-stage
         define-calls-frontier/two-stage
         define-calls-frontier/three-stage
         define-lift-search-to-calls)

(define-syntax-rule (define-search-frontier/one-stage name rel lang ctx)
  (define name
    (union-reduction-relations
     (context-closure
      (context-closure rel lang ctx)
      lang
      Q)
     (make-core-collector lang))))

(define-syntax-rule (define-search-frontier/one-stage/no-collector name rel lang ctx)
  (define name
    (context-closure
     (context-closure rel lang ctx)
     lang
     Q)))

(define-syntax-rule (define-search-frontier/two-stage name rel lang ctx1 ctx2)
  (define name
    (union-reduction-relations
     (context-closure
      (context-closure
       (context-closure rel lang ctx1)
       lang
       ctx2)
      lang
      Q)
     (make-core-collector lang))))

(define-syntax-rule (define-search-frontier/three-stage name rel lang ctx1 ctx2 ctx3)
  (define name
    (union-reduction-relations
     (context-closure
      (context-closure
       (context-closure
        (context-closure rel lang ctx1)
        lang
        ctx2)
       lang
       ctx3)
      lang
      Q)
     (make-core-collector lang))))

(define-syntax-rule (define-search-frontier/two-stage/no-collector name rel lang ctx1 ctx2)
  (define name
    (context-closure
     (context-closure
      (context-closure rel lang ctx1)
      lang
      ctx2)
     lang
     Q)))

(define-syntax-rule (define-search-frontier/three-stage/no-collector name rel lang ctx1 ctx2 ctx3)
  (define name
    (context-closure
     (context-closure
      (context-closure
       (context-closure rel lang ctx1)
       lang
       ctx2)
      lang
      ctx3)
     lang
     Q)))

(define-syntax-rule (define-calls-frontier/one-stage name rel lang ctx)
  (define name
    (context-closure
     (context-closure
      (context-closure rel lang ctx)
      lang
      Q)
     lang
     (Γ hole))))

(define-syntax-rule (define-calls-frontier/two-stage name rel lang ctx1 ctx2)
  (define name
    (context-closure
     (context-closure
      (context-closure
       (context-closure rel lang ctx1)
       lang
       ctx2)
      lang
      Q)
     lang
     (Γ hole))))

(define-syntax-rule (define-calls-frontier/three-stage name rel lang ctx1 ctx2 ctx3)
  (define name
    (context-closure
     (context-closure
      (context-closure
       (context-closure
        (context-closure rel lang ctx1)
        lang
        ctx2)
       lang
       ctx3)
      lang
      Q)
     lang
     (Γ hole))))

(define-syntax-rule (define-lift-search-to-calls name rel lang)
  (define name
    (context-closure rel lang (Γ hole))))
