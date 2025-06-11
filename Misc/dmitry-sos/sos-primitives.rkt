#lang racket
(require rackunit rackunit/text-ui)
(provide (except-out (all-defined-out) walk occurs? extend))

;; A subst is a ((var val) ...).
(define (walk t s)
  (let ((pr (assv t s)))
	(if pr (walk (second pr) s) t)))

;; Assumes v is a var and t comes in walked viz s.
(define (occurs? v t s)
  (match t
	[(cons t1 t2)
     (or (occurs? v (walk t1 s) s) (occurs? v (walk t2 s) s))]
	[else (eqv? v t)]))

(define (extend v t s)
  (if (occurs? v t s)
	  #f
	  (cons (list v t) s)))

(define (unify t1 t2 s)
  (define (unify-help t1 t2 s)
	(match (list t1 t2)
	  [(list t t) s]
	  [(list v t) #:when (number? v)
	   (extend v t s)]
	  [(list t v) #:when (number? v)
	   (extend v t s)]
	  [(list (cons t1a t1b) (cons t2a t2b))
	   (let ((s^ (unify t1a t2a s)))
		 (and s^ (unify t1b t2b s^)))]
	  [(list _ _) #f]))
  (unify-help (walk t1 s) (walk t2 s) s))

(define (subst*-term vars vals term)
  (cond
	[(cons? term)
     (cons (subst*-term vars vals (car term))
		   (subst*-term vars vals (cdr term)))]
	[(index-of vars term)
	 => (lambda (i) (list-ref vals i))]
	[else term]))

(define (subst-term var val t)
  (subst*-term (list var) (list val) t))

;; [Listof Vars] [Listof Vals] [Listof Terms] -> Listof Terms
;; Simultaneously substitute instances of vars for vals recursively over each term in terms.
(define (subst*-terms vars vals lst)
  (map (lambda (term) (subst*-term vars vals term)) lst))

(define (subst-terms var val lst)
  (subst*-terms (list var) (list val) lst))

(define-test-suite MK-HELPERS
  (test-case "Non-Numeric Term Returns Itself"
			 (check-equal? (walk 'dog '()) 'dog)
			 (check-equal? (walk "dog" '()) "dog")
			 (check-equal? (walk #t '()) #t)
			 (check-equal? (walk (cons 0 1) '()) (cons 0 1)))

  (test-case "Numeric Term Walks To Correct Value"
			 (check-equal? (walk 0 `((0 dog))) 'dog)
			 (check-equal? (walk 0 `((0 1) (1 2) (2 dog))) 'dog)
			 (check-equal? (walk 0 `((0 (1 2)) (1 "bear") (2 "bird"))) (list 1 2)))

  (test-case "Numeric Term Not Bound Returns Itself"
			 (check-equal? (walk 0 '()) 0)
			 (check-equal? (walk 5 '((0 1) (1 2) (2 3))) 5))

  (test-true "Logic Var Occurs In Itself"
			 (occurs? 0 0 '()))

  (test-false "Logic Var Does Not Appear In Ground Atomic Term"
			  (occurs? 0 "not here" '()))

  (test-true "Logic Var Appears Nested In A List"
			 (occurs? 0 (cons (cons 1 2) (cons (cons 0 5) 12)) '()))

  (test-true "Logic Var Appears In A Walked Term"
			 (occurs? 0 0 '((1 0))))

  (test-false "Logic Var Does Not Appear In A List"
			  (occurs? 0 (cons 1 2) '()))

  (test-case "Substitution Is Extended When Occurs Check Passes"
			 (check-false (occurs? 0 "dog" '()))
			 (check-equal? (extend 0 "dog" '()) '((0 "dog"))))

  (test-case "Extending Substitution Fails When Occurs Check Fails"
			 (check-true (occurs? 0 0 '()))
			 (check-false (extend 0 0 '())))

  (test-case "Unifying Two of the Same Terms Returns The Original Substitution"
			 (check-equal? (unify 0 0 '()) '())
			 (check-equal? (unify "dog" "dog" '()) '()))

  (test-case "Unifying A Logic Var And A Term Extends The Substitution"
			 (check-equal? (unify 0 "dog" '()) '((0 "dog")))
			 (check-equal? (unify "dog" 0 '()) '((0 "dog")))
			 (check-equal? (unify 0 1 '((1 "cat"))) '((0 "cat") (1 "cat"))))

  (test-case "Unifying Pairs Works"
			 (check-equal? (unify (cons 0 "bear") (cons "eagle" 1) '())
						   '((1 "bear") (0 "eagle")))
			 (check-equal? (unify (cons 0 1) (cons 2 3) '((0 "dog") (1 "cat") (2 "dog") (3 "cat")))
						   '((0 "dog") (1 "cat") (2 "dog") (3 "cat"))))

  (test-case "Unifying Two Different Terms Fails"
			 (check-false (unify "dog" "cat" '()))
			 (check-false (unify 0 1 '((0 "dog") (1 #t)))))

  (test-case "Nested Walk Resolves Correctly"
			   (check-equal? (walk 0 '((3 "dog") (2 3) (1 2) (0 1))) "dog")
			   (check-equal? (walk 0 '((0 (1 2)) (3 "cat") (1 (3 4)))) '(1 2)))

  (test-false "Occurs Check Fails For Disjoint Terms"
              (occurs? 0 (cons 1 2) '((1 "dog") (2 "cat"))))

  (test-true "Occurs Check Passes For Deeply Nested Logic Var"
             (occurs? 0 '(1 (2 (3 (4 0)))) '()))

  (test-false "Extend Fails Due To Occurs Check In Nested Structure"
              (extend 0 (cons 1 (cons 2 0)) '()))

  (test-case "Extend Works With Nested Structure When Occurs Check Passes"
             (check-equal? (extend 0 (cons 1 (cons 2 3)) '()) '((0 (1 2 . 3)))))

  (test-case "Unify Nested Pairs"
             (check-equal? (unify '(0 . (1 . 2)) '("cat" . ("dog" . "bird")) '())
                           '((2 "bird") (1 "dog") (0 "cat"))))

  (test-false "Unify Fails With Contradictory Nested Terms"
              (unify '(0 . "dog") '("cat" . 0) '()))

  (test-case "subst-term Replaces Correctly"
             (check-equal? (subst-term 'a "dog" '(a . 1)) '("dog" . 1))
             (check-equal? (subst-term 'a "cat" '(0 . (a . 2))) '(0 . ("cat" . 2))))

  (test-case "subst*-terms Simultaneously Replaces Multiple Terms"
             (check-equal? (subst*-terms '(a b) '((b . "dog") (a . "cat")) '(a b a))
                           '((b . "dog") (a . "cat") (b . "dog"))))

  (test-case "subst*-terms Simultaneously Replaces Multiple Terms"
             (check-equal? (subst*-terms '(a b) '((b . "dog") (a . "cat")) '(b))
                           '((a . "cat"))))

  )
