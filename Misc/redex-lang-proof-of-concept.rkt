#lang racket
(require redex)

#|

"All problems in computer science can be solved by another
 level of indirection"
--- Butler Lampson

|#

(define-language L1
  [A ::= ((X_!_ ...) ...)]
  [X ::= number])


(define-language L2
  [A ::= (L ...)]
  [L ::= (X_!_ ...)]
  [X ::= number])

(redex-match L1 A (term ((1 2 3) (1 2 3)))) ;; intended to be allowed, but fails grammar---bad
(redex-match L2 A (term ((1 2 3) (1 2 3)))) ;; intended to be allowed, and passes grammar---good
(redex-match L1 A (term ((1 1) (1 2 3))))   ;; intended to be excluded, and fails grammar---good
(redex-match L2 A (term ((1 1) (1 2 3))))   ;; intended to be excluded, and fails grammar---good

(define-language L3
  [A (R ...)]
  [R (r x_!_ ...)]
  [r (variable-prefix r:)]
  [x (variable-prefix x:)])

(redex-match L3 A (term ((r:test x:a x:b)))) ;; succeed good
(redex-match L3 A (term ((r:test x:a x:a)))) ;; fail good
(redex-match L3 A (term ((r:test x:a) (r:test1 x:a)))) ;; succeed good
(redex-match L3 A (term ((r:test x:a) (r:test x:a)))) ;; succeed good
(redex-match L3 A (term ((r:test x:a) (r:test x:b)))) ;; succeed bad



