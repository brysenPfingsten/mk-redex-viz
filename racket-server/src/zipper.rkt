#lang racket
(provide (all-defined-out))

(define-struct zipper 
               ([prev #:mutable] 
                [curr #:mutable]
                [next #:mutable]
                [idx  #:mutable])
               #:transparent)

;; zipper-init!: zipper -> void
;; Initializes the given zipper with empty prev and next,
;; false current, and 0 current
(define (zipper-init! z)
  (set-zipper-prev! z '())
  (set-zipper-curr! z #f)
  (set-zipper-next! z '())
  (set-zipper-idx!  z 0))

;; zipper-add!: zipper any -> void
;; Adds the given elem to the zipper and moves the current to the previous
(define (zipper-add! z elem)
  (match z
    [(zipper prev curr next idx) #:when (not (false? curr))
    (set-zipper-prev! z (cons curr prev))
    (set-zipper-curr! z elem)
    (set-zipper-idx!  z (add1 idx))]
    [_
     (set-zipper-curr! z elem)]))

;; zipper-next!: zipper -> any
;; Retrieves the next element if it exists and pushes the current to the previous
;; Returns: The element if next is non-empty, otherwise false
(define (zipper-next! z)
  (match z
    [(zipper prev curr (cons a d) idx)
     (set-zipper-prev! z (cons curr prev))
     (set-zipper-curr! z a)
     (set-zipper-next! z d)
     (set-zipper-idx!  z (add1 idx))
     a]
    [_ #f]))


(define-struct initial (elem) #:transparent)

;; zipper-back!: zipper ->  any
;; Retrieves the previous element if it exists and pushes the current to the next
;; Returns: The element if prev is non-empty, otherwise false
(define (zipper-back! z)
  (let ([go-back! (λ (a d curr next idx)
                      (set-zipper-next! z (cons curr next))
                      (set-zipper-curr! z a)
                      (set-zipper-prev! z d)
                      (set-zipper-idx!  z (sub1 idx)))])
  (match z
    [(zipper (list elem) curr next idx)
     (go-back! elem '() curr next idx)
     (initial elem)]
    [(zipper (cons a d) curr next idx)
     (go-back! a d curr next idx)
     a]
    [_ #f])))


