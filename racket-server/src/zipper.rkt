#lang racket

(provide (struct-out zipper)
         make-empty-zipper
         zipper-reset
         zipper-add
         zipper-forward
         zipper-back)

(struct zipper (prev curr next idx) #:transparent)

(define (make-empty-zipper)
  (zipper '() #f '() 0))

(define (zipper-reset _z)
  (make-empty-zipper))

(define (zipper-add z elem)
  (match z
    [(zipper prev curr _ idx) #:when (not (false? curr))
     (zipper (cons curr prev) elem '() (add1 idx))]
    [_ (zipper '() elem '() 0)]))

(define (zipper-forward z)
  (match z
    [(zipper prev curr (cons a d) idx)
     (values a (zipper (cons curr prev) a d (add1 idx)))]
    [_ (values #f z)]))

(define (zipper-back z)
  (match z
    [(zipper (cons a d) curr next idx)
     (values a (zipper d a (cons curr next) (sub1 idx)))]
    [_ (values #f z)]))
