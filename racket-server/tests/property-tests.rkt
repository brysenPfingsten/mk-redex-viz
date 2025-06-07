#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(redex-check L
             p
             (implies (and (not (redex-match L prog-val (term p)))
                           (judgment-holds (closed-program?  p)))
                      (= (length (apply-reduction-relation red (term p))) 1))
             #:attempts 20000
             #:print? (λ (p) #t)
             #:keep-going? #true
             #:attempt-size (λ (i) 7))


