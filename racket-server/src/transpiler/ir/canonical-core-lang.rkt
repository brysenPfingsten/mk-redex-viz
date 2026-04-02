#lang racket

(require redex/reduction-semantics
         "../../search-lattice/languages/core-lang.rkt")

(provide canonical-core-lang)

(check-redundancy #t)

(define-extended-language canonical-core-lang core-lang
  [r (variable-prefix r:)]
  [d (x_!_ ...)]
  [Gamma ((r d g) ...)]
  [w (empty-tree)
     (g σ)
     (w × g c)
     (⊤ σ)]
  [config (Gamma w)]
  [end-config (Gamma (empty-tree))]

  #:binding-forms
  (config #:refers-to (shadow r ...)
          ((r (x ...) g #:refers-to (shadow x ...)) ...)
          #:refers-to (shadow r ...)))
