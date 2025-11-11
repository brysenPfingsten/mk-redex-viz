#lang racket
(require rackunit
         rackunit/text-ui
         redex
         "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(define/provide-test-suite
 DISEQUALITY
 #:after (λ () (displayln "Finished running tests for disequality."))
 (test-case "A program with a contradicting unification and disequality."
   (define prog
     (term (((∃ (x:q) ((x:q =? (sym "bear") "u2") ∧ (x:q != (sym "bear") "u3") "c1") "f0")
             (state () () 0 () "s"))
            ())))
   (check-true (term (closed-program? ,prog)))
  (test-equal? "Program Steps to Failure" 
               (apply-reduction-relation* red prog) 
               '((() ())))))

(test-case "A program with a contradicting disequality and unification"
  (define prog2
    '(((∃ (x:q) ((x:q != (sym "bear") "u2") ∧ (x:q =? (sym "bear") "u3") "c1") "f0")
       (state () () 0 () "s"))
      ()))

  (check-true (term (closed-program? ,prog2)))
  (test-equal? "Program Steps to Failure" 
               (apply-reduction-relation* red prog2) 
               '((() ()))))

;; (run-tests DISEQUALITY)
