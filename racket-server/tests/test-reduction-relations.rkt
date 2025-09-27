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

;; (redex-match L (in-hole Ex (in-hole Ev s)) test-term)

(let ([pn (apply-reduction-relation/tag-with-names red test-term)])
  (check-true (or (null? pn) (null? (cdr pn)))))



;; (traces red test-term)
;; (stepper red test-term)
