#lang racket

(require redex/reduction-semantics
         "./rcall-eager.rkt"
         "./rcall-lazy.rkt"
         "./rdisj-left.rkt"
         "./rbase-e.rkt"
         "./rbase-l.rkt"
         "./rflip-e.rkt"
         "./rflip-l.rkt"
         "./rrail-e.rkt"
         "./rrail-l.rkt")

(provide Rcall-eager
         Rcall-lazy
         Rdisj-left
         Rbase-e
         Rbase-l
         Rflip-e
         Rflip-l
         Rrail-e
         Rrail-l
         step-once/Rcall-eager
         step-once/Rcall-lazy
         step-once/Rdisj-left
         step-once/Rbase-e
         step-once/Rbase-l
         step-once/Rflip-e
         step-once/Rflip-l
         step-once/Rrail-e
         step-once/Rrail-l)

;; Step wrappers for GUI/driver usage.
(define (step-once/Rcall-eager prog)
  (apply-reduction-relation/tag-with-names Rcall-eager (term ,prog)))

(define (step-once/Rcall-lazy prog)
  (apply-reduction-relation/tag-with-names Rcall-lazy (term ,prog)))

(define (step-once/Rdisj-left prog)
  (apply-reduction-relation/tag-with-names Rdisj-left (term ,prog)))

(define (step-once/Rbase-e prog)
  (apply-reduction-relation/tag-with-names Rbase-e (term ,prog)))

(define (step-once/Rbase-l prog)
  (apply-reduction-relation/tag-with-names Rbase-l (term ,prog)))

(define (step-once/Rflip-e prog)
  (apply-reduction-relation/tag-with-names Rflip-e (term ,prog)))

(define (step-once/Rflip-l prog)
  (apply-reduction-relation/tag-with-names Rflip-l (term ,prog)))

(define (step-once/Rrail-e prog)
  (apply-reduction-relation/tag-with-names Rrail-e (term ,prog)))

(define (step-once/Rrail-l prog)
  (apply-reduction-relation/tag-with-names Rrail-l (term ,prog)))
