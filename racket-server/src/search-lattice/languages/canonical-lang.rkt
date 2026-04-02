#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./disj-lang.rkt")

(provide canonical-lang)

(check-redundancy #t)

(define-union-language canonical/join
  calls-lang
  disj-lang)

(define-extended-language canonical-lang canonical/join
  [config (Γ s as)]
  [end-config (Γ (empty-tree) as)]

  #:binding-forms
  (config #:refers-to (shadow r ...)
          ((r (x ...) g #:refers-to (shadow x ...)) ...)
          #:refers-to (shadow r ...)))
