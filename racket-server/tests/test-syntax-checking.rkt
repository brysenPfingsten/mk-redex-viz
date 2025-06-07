#lang racket

(require "../src/definitions.rkt"
         "../src/syntax-checking.rkt"
         "../src/judgment-forms.rkt")
(require redex
         rackunit
         rackunit/text-ui)

(define WELL-FORMED-PROG (term (prog () ())))
(define BAD-FORMED-PROG (term (prog () ((r:goneo "r0") (state () 0 ())))))

(define-test-suite WELL-FORMED
  (test-case "Well Formed Program Returns Empty String"
    (check-true (redex-match? L p WELL-FORMED-PROG))
    (check-true (judgment-holds (closed-program? ,WELL-FORMED-PROG)))
    (check-equal? (check-well-formed WELL-FORMED-PROG) ""))

  (test-case "Non Well Formed Program Returns Error Message"
    (check-true (redex-match? L p BAD-FORMED-PROG))
    (check-false (judgment-holds (closed-program? ,BAD-FORMED-PROG)))
    (check-equal? (check-well-formed BAD-FORMED-PROG)
                  "Program is not well formed!")))

(define GOOD-SYNTAX-PROG 
"
(defrel (foo x) (== x 'bar))
(run* (q) (foo q))
"
)
(define BAD-SYNTAX-PROG
"
(defrel (foo x) (== x 'foo))
(run* (foo q))
"
)

(define LOOP-PROG
"
(defrel (loopo x) (loopo x))
(run* (q) (loopo q))
"
)

(define ARITY-MISMATCH
"
(defrel (foo x y)
  (== x 'x))
(run* (q r s t) (foo q r s t))
")

(define-test-suite SYNTAX-CHECKING
  (test-equal? "Syntactically Valid Program Returns Empty String"
               (check-syntax-capture-error GOOD-SYNTAX-PROG)
               "")
  (test-true "Syntacically Invalid Program Returns Nonempty String"
             (non-empty-string? (check-syntax-capture-error BAD-SYNTAX-PROG)))
;; "syntax-checker: run*: expected more terms starting with goal expression
;;  at: ()
;;  within: (run* (foo q))
;;  in: (run* (foo q))
;;  parsing context:
;;  while parsing (run* (<id> ...+) <goal> ...+)
;;  term: (run* (foo q))
;;  location: syntax-checker"
  (test-equal? "Non Terminating Program Does Not Evaluate Forever"
               (check-syntax-capture-error LOOP-PROG) "")

  (test-true "Arity Mismatch is Detected"
             (non-empty-string? (check-syntax-capture-error ARITY-MISMATCH))))

(define/provide-test-suite SYNTAX-CHECKER
    #:before (thunk (displayln "Running Rests For Syntax Checking..."))
    #:after  (thunk (displayln "Finished Running Tests For Syntax Checking."))
    WELL-FORMED
    SYNTAX-CHECKING)
