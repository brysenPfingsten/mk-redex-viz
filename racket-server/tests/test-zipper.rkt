#lang racket

(require rackunit
         rackunit/text-ui
         "../src/zipper.rkt")

(define z1 (zipper '() 1 '() 0))
(define z2 (zipper '(1 2) 3 '(4 5) 2))
(define z3 (zipper '(1) 2 '(3) 3))
(define init-zip (make-empty-zipper))

(define/provide-test-suite ZIPPER
  #:before (thunk (displayln "Running tests for zipper..."))
  #:after (thunk (displayln "Finished running tests for zipper."))

  (test-case "zipper-reset returns an empty zipper"
    (define z^ (zipper-reset z1))
    (match-define (zipper prev curr next idx) z^)
    (check-equal? prev '())
    (check-false curr)
    (check-equal? next '())
    (check-equal? idx 0))

  (test-case "zipper-add pushes the current entry into history"
    (define z^ (zipper-add z1 100))
    (match-define (zipper prev curr next idx) z^)
    (check-equal? prev '(1))
    (check-equal? curr 100)
    (check-equal? next '())
    (check-equal? idx 1)
    (check-equal? z1 (zipper '() 1 '() 0)))

  (test-case "zipper-add seeds an empty zipper at index zero"
    (define z^ (zipper-add init-zip 100))
    (match-define (zipper prev curr next idx) z^)
    (check-equal? prev '())
    (check-equal? curr 100)
    (check-equal? next '())
    (check-equal? idx 0))

  (test-case "zipper-back walks to the previous entry without mutating input"
    (define-values (elem z^) (zipper-back z3))
    (match-define (zipper prev curr next idx) z^)
    (check-equal? elem 1)
    (check-equal? prev '())
    (check-equal? curr 1)
    (check-equal? next '(2 3))
    (check-equal? idx 2)
    (check-equal? z3 (zipper '(1) 2 '(3) 3)))

  (test-case "zipper-back on an empty history is a no-op"
    (define-values (elem z^) (zipper-back z1))
    (check-false elem)
    (check-equal? z^ z1))

  (test-case "zipper-forward on a cached future advances without mutating input"
    (define-values (elem z^) (zipper-forward z2))
    (match-define (zipper prev curr next idx) z^)
    (check-equal? elem 4)
    (check-equal? prev '(3 1 2))
    (check-equal? curr 4)
    (check-equal? next '(5))
    (check-equal? idx 3)
    (check-equal? z2 (zipper '(1 2) 3 '(4 5) 2)))

  (test-case "zipper-forward on an empty future is a no-op"
    (define-values (elem z^) (zipper-forward z1))
    (check-false elem)
    (check-equal? z^ z1)))

(module+ test
  (run-tests ZIPPER))
