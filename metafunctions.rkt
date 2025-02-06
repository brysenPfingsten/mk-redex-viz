#lang racket
(require redex)
(require "definitions.rkt")

(define-metafunction L
  unify-pair : any -> any
  [(unify-pair (c t)) (== c t)])

(define-metafunction L
  reify : sub -> t
  [(reify ((c t) ...))
   ,(eval (term (car (run* (q) ,@(unify-pair (c t) ...)))))])