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
  (test-true "Regression: decomposing rel environment when no relcall breaks unique decomposition"
    (or (null? pn) (null? (cdr pn)))))


(define relcall-body
  (term
	(prog
	  ((r:N (x:w) (∃ () (∃ () ⊤ (nat 1)) (sym ""))))
	  (proceed ((r:N (nat 4)) (state () 2 () (nat 7)))))))

(let ([pn (apply-reduction-relation/tag-with-names red relcall-body)])
  (test-true ""
    (or (null? pn) (null? (cdr pn)))))
