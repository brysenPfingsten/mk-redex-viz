#lang racket

(require redex/reduction-semantics
         "./core-lang.rkt")

(provide canonical-core-lang)

(check-redundancy #t)

(define-extended-language canonical-core-lang core-lang
  [r (variable-prefix r:)]
  [d (x_!_ ...)]
  [Γ ((r d g) ...)]
  [w (empty-tree)
     (g σ)
     (w × g c)
     (⊤ σ)]
  [config (Γ w)]
  [end-config (Γ (empty-tree))]

  #:binding-forms
  (config #:refers-to (shadow r ...)
          ((r (x ...) g #:refers-to (shadow x ...)) ...)
          #:refers-to (shadow r ...)))
