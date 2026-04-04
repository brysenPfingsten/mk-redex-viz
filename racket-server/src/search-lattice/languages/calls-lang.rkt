#lang racket

(require redex/reduction-semantics
         "./delay-lang.rkt")

(provide relcall-lang)

(check-redundancy #t)

(define-extended-language relcall-lang delay-lang
  [r (variable-prefix r:)]
  [g ....
     (r t ... tag)]
  [Γ ((r d g) ...)]
  [config (Γ cfg)]

  #:binding-forms
  (config #:refers-to (shadow r ...)
          ((r (x ...) g #:refers-to (shadow x ...)) ...)
          #:refers-to (shadow r ...)))
