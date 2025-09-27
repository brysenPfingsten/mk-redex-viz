#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(define test-term
  (term
	(prog
	  ((r:+ () (∃ () ⊤ (sym "oZ"))) (r:X () (∃ () ⊤ (sym "HYvONcWjZNW"))))
	  (delay ()))))

(apply-reduction-relation red test-term)


;; (traces red test-term)
;; (stepper red test-term)
