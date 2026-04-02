#lang racket
(require redex)
(require "../../../../src/archive/legacy-deprecated/legacy-stack/metafunctions.rkt"
         "../../../../src/archive/legacy-deprecated/legacy-stack/definitions.rkt")
(require rackunit rackunit/text-ui)

(provide METAFUNCTIONS)

(define PROG1
  '(((∃ (x:q x:r x:v) ("a" =? "a" "u0") "f0") (state () 0 () "s")) ()))

(define PROG2
  '(((∃ (x:q) ("a" =? "a" "u0") "f0") (state () 0 () "s")) ()))

(define PROG3
  '(((∃ () ("a" =? "a" "u0") "f0") (state () 0 () "s")) ()))

(define PROG4
  '((("a" =? "a" "u0") (state () 0 () "s")) ()))

(define-test-suite NUM-QUERY-VARS
  #:before (thunk (displayln "Running tests for num-query-vars..."))
  #:after (thunk (displayln "Finished running tests for num-query-vars."))

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
  )

(define-test-suite UNIFY-WALK-SANITY
  (test-case "unify equal constants preserves substitution"
    (check-equal? (term (unify "abc" "abc" ((2 "fish"))))
                  (term ((2 "fish")))))

  (test-case "walk resolves direct variable binding"
    (check-equal? (term (walk 0 ((1 "cat") (0 "dog"))))
                  (term "dog")))

  (test-case "walk resolves chained variable binding"
    (check-equal? (term (walk 0 ((1 "cat") (0 1))))
                  (term "cat"))))

(define/provide-test-suite METAFUNCTIONS
  #:before (thunk (displayln "Running tests for metafunctions..."))
  #:after (thunk (displayln "Finished running tests for metafunctions."))
  NUM-QUERY-VARS
  UNIFY-WALK-SANITY
  )

;(run-tests METAFUNCTIONS)
