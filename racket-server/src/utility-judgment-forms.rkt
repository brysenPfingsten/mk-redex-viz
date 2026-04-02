#lang racket
(require redex)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(provide same-length?)

(require "definitions.rkt")

(define-judgment-form
  Core
  #:contract (same-length? (t ...) (x ...))
  #:mode (same-length? I I)

  [------------"empty list same length"
   (same-length? () ())]

  [(same-length? (t ...) (x ...))
   ------------"cons list same length"
   (same-length? (t_1 t ...) (x_1 x ...))]

  )
