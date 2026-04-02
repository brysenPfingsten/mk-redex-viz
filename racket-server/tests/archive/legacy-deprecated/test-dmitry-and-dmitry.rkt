#lang racket

(require redex
         redex/reduction-semantics
         rackunit)

(require "../../../src/reduction-relations/archive/legacy-deprecated/dmitry-and-dmitry.rkt")

(module+ test
  (define eq-prog
    (term ((((sym "a") =? (sym "a") (sym "u"))
            (state () 0 () (sym "s")))
           ())))

  (define next
    (apply-reduction-relation/tag-with-names dmitry-and-dmitry eq-prog))

  (test-true "dmitry relation can take one step on simple equality program" (pair? next))
  (test-true "dmitry stepping remains deterministic on this input"
             (or (null? next) (null? (cdr next)))))

(module+ main
  (stepper
   dmitry-and-dmitry
   (term ((((sym "a") =? (sym "a") (sym "u"))
           (state () 0 () (sym "s")))
          ()))))
