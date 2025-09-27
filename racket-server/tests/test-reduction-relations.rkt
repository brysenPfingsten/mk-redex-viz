#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(define two-relations-delay-body
  (term
	(prog
	  ((r:+ () (∃ () ⊤ (sym "oZ")))
	   (r:X () (∃ () ⊤ (sym "HYvONcWjZNW"))))
	  (delay ()))))

(let ([pn (apply-reduction-relation/tag-with-names red two-relations-delay-body)])
  (check-true (or (null? pn) (null? (cdr pn)))))

