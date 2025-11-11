#lang racket
(require rackunit
         redex
         "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(define prog (term (((∃
    (x:q)
    ((x:q =? (sym "bear") "u2") ∧ (x:q != (sym "bear") "u3") "c1")
    "f0")
   (state () () 0 () "s"))
  ())))

(check-true (term (closed-program? ,prog)))


(check-equal? (apply-reduction-relation*
                red
                prog)
              '((() ())))

(define prog2 '(((∃
    (x:q)
    ((x:q != (sym "bear") "u2") ∧ (x:q =? (sym "bear") "u3") "c1")
    "f0")
   (state () () 0 () "s"))
  ()))

(check-true (term (closed-program? ,prog2)))
(check-equal? (apply-reduction-relation*
                red
                prog2)
              '((() ())))
