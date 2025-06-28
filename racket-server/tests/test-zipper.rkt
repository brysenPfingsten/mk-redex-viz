#lang racket
(require rackunit
         rackunit/text-ui
         "../src/zipper.rkt")
(provide ZIPPER)

(define z1 'unititialized)
(define z2 'unititialized)
(define z3 'unititialized)
(define init-zip 'unititialized)

(define (initialize-all!)
  (set! z1 (zipper '() 1 '() 0))
  (set! z2 (zipper '(1 2) 3 '(4 5) 2))
  (set! z3 (zipper '(1) 2 '(3) 3))
  (set! init-zip (zipper '() #f '() 0)))

(define-test-suite ZIPPER
  #:before (thunk (displayln "Running tests for zipper..."))
  #:after (thunk (displayln "Finished running tests for zipper."))

  (initialize-all!)
  (test-case "zipper-init! Works"
             (zipper-init! z1)
             (check-true (empty? (zipper-prev z1)))
             (check-false (zipper-curr z1))
             (check-true (empty? (zipper-next z1)))
             (check-equal? (zipper-idx z1) 0))

  (initialize-all!)
  (test-case "Adding To Non-Empty Zipper Works"
             (zipper-add! z1 100)
             (check-equal? (zipper-prev z1) '(1))
             (check-equal? (zipper-curr z1) 100)
             (check-equal? (zipper-next z1) '())
             (check-equal? (zipper-idx z1) 1))

  (initialize-all!)
  (test-case "Adding To Empty Zipper Works"
             (zipper-add! init-zip 100)
             (check-equal? (zipper-prev init-zip) '())
             (check-equal? (zipper-curr init-zip) 100)
             (check-equal? (zipper-next init-zip) '())
             (check-equal? (zipper-idx init-zip) 0))

  (initialize-all!)
  (test-case "Going Back When Previous Has One Element Returns False Struct"
              (check-equal? (zipper-back! z3) (initial 1))
              (check-equal? (zipper-prev z3) '())
              (check-equal? (zipper-curr z3) 1)
              (check-equal? (zipper-next z3) '(2 3))
              (check-equal? (zipper-idx z3) 2))

  (initialize-all!)
  (test-case "Going Back When Previous Is Non-Empty Works"
             (check-equal? (zipper-back! z2) 1)
             (check-equal? (zipper-prev z2) '(2))
             (check-equal? (zipper-curr z2) 1)
             (check-equal? (zipper-next z2) '(3 4 5))
             (check-equal? (zipper-idx z2) 1))

  (initialize-all!)
  (test-case "Going Back When Previous Is Empty Returns False And No Mutations"
             (check-false (zipper-back! z1))
             (check-equal? (zipper-prev z1) '())
             (check-equal? (zipper-curr z1) 1)
             (check-equal? (zipper-next z1) '())
             (check-equal? (zipper-idx z1) 0))

  (initialize-all!)
  (test-case "Going Forward When Next Is Empty Returns False And No Mutations"
             (check-false (zipper-next! z1))
             (check-equal? (zipper-prev z1) '())
             (check-equal? (zipper-curr z1) 1)
             (check-equal? (zipper-next z1) '())
             (check-equal? (zipper-idx z1) 0))

  (initialize-all!)
  (test-case "Going Forward When Next Is Non-Empty Works"
             (check-equal? (zipper-next! z2) 4)
             (check-equal? (zipper-prev z2) '(3 1 2))
             (check-equal? (zipper-curr z2) 4)
             (check-equal? (zipper-next z2) '(5))
             (check-equal? (zipper-idx z2) 3))
  )

#; (run-tests ZIPPER)
