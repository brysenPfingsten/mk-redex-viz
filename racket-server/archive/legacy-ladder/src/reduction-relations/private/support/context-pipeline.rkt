#lang racket

(require redex/reduction-semantics)

(provide define-cfg/one-stage
         define-cfg/two-stage)

;; Build cfg closure as core -> context -> whole-config.
(define-syntax-rule (define-cfg/one-stage cfg-name rel lang ctx)
  (define cfg-name
    (context-closure
     (context-closure rel lang ctx)
     lang
     (Γ hole as))))

;; Build cfg closure as core -> stage1 -> stage2 -> whole-config.
(define-syntax-rule (define-cfg/two-stage cfg-name rel lang ctx1 ctx2)
  (define cfg-name
    (context-closure
     (context-closure
      (context-closure rel lang ctx1)
      lang
      ctx2)
     lang
     (Γ hole as))))
