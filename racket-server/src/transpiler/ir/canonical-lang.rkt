#lang racket

(require redex/reduction-semantics
         "./canonical-core-lang.rkt")

(provide canonical-lang)

(check-redundancy #t)

(define-extended-language canonical-lang canonical-core-lang
  [g ....
     (g ∨ g tag)
     (suspend g tag)
     (r t ... tag)]
  [w ....
     (delay w)
     (w <-+ w)]

  #:binding-forms
  (config #:refers-to (shadow r ...)
          ((r (x ...) g #:refers-to (shadow x ...)) ...)
          #:refers-to (shadow r ...)))
