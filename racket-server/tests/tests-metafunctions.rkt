#lang racket
(require redex)
(require "../src/metafunctions.rkt"
         "../src/definitions.rkt")
(require rackunit)

(define PROG1
  '(prog () ((∃ (x:q x:r x:v) ("a" =? "a" "u0") "f0") (state () 0 ()))))

(define PROG2
  '(prog () ((∃ (x:q) ("a" =? "a" "u0") "f0") (state () 0 ()))))

(define PROG3
  '(prog () ((∃ () ("a" =? "a" "u0") "f0") (state () 0 ()))))

(define PROG4
  '(prog () (("a" =? "a" "u0") (state () 0 ()))))

(test-case "Program With Multiple Query Vars Works"
           (check-true (redex-match? L p PROG1))
           (check-equal? (num-query-vars PROG1) 3))

(test-case "Program With One Query Var Works"
           (check-true (redex-match? L p PROG2))
           (check-equal? (num-query-vars PROG2) 1))

(test-case "Program With No Query Vars Works"
           (check-true (redex-match? L p PROG3))
           (check-equal? (num-query-vars PROG3) 0))

(test-case "Non Fresh Program Throws Error"
           (check-true (redex-match? L p PROG4))
           (check-exn exn:fail? (thunk (num-query-vars PROG4))))