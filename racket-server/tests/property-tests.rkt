#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

 (redex-check L p
    (implies (judgment-holds (closed-program? p))
             (let ([outs (apply-reduction-relation red (term p))])
               (or (null? outs)
                   (and (null? (cdr outs))
                        (closed-program? (car outs))))))
    #:attempts 100000)
    ;;500000 when making changes
