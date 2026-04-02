#lang racket

(require redex
         redex/reduction-semantics
         rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(define same-call-prog
  (term
   (((r:same (sym "cat") (sym "cat") (sym "r0"))
     (state () 0 () (sym "s")))
    ((r:same (x:x x:y) (x:x =? x:y (sym "u1")))))))

(module+ test
  ;; Keep raco-test path non-interactive.
  (test-match L g (term ⊤))
  (test-match L g (term ((sym "cat") =? (sym "cat") (sym "u"))))
  (test-true
   "visual smoke: relation call program is in-domain and can step"
   (not (null?
         (apply-reduction-relation
          red
          same-call-prog)))))

(module+ main
  (require redex/gui)
  ;; Interactive visual inspection entrypoint.
  (stepper red same-call-prog))
