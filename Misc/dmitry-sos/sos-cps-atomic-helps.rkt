#lang racket
(require rackunit rackunit/text-ui)
(require racket/trace)
(require "sos-primitives.rkt")

(struct trip (tree s n) #:transparent)
(struct mt () #:transparent)
(struct rel (name args goal)  #:transparent)
(struct == (t1 t2) #:transparent)
(struct gor (g1 g2) #:transparent)
(struct tor (t1 t2) #:transparent)
(struct gand (g1 g2) #:transparent)
(struct tand (t1 t2) #:transparent)
(struct fresh (x g) #:transparent)
(struct relcall (name terms) #:transparent)

(define (substitute fv rv g)
  (subst*-goal (list fv) (list rv) g))

(define (subst*-goal vars vals goal)
  (match goal
	[(== t1 t2)
	 (== (subst*-term vars vals t1) (subst*-term vars vals t2))]

	[(gor g1 g2)
     (gor (subst*-goal vars vals g1) (subst*-goal vars vals g2))]

	[(gand g1 g2)
	 (gand (subst*-goal vars vals g1) (subst*-goal vars vals g2))]

	[(fresh x g)
     (cond
	   [(index-of vars x)
		=> (lambda (i)
			 (match-let-values ([(before-args (cons a after-args)) (split-at vars i)]
						        [(before-terms (cons t after-terms)) (split-at vals i)])
			   (fresh x (subst*-goal (append before-args after-args) (append before-terms after-terms) g))))]
		[else (fresh x (subst*-goal vars vals g))])]

	[(relcall name terms)
     (relcall name (subst*-terms vars vals terms))]))

(define (invoke name terms)
  (match-let ([`(,args . ,goal) (lookup-rel name rels)])
	(subst*-goal args terms goal)))

(define sameo (rel 'sameo '(x y) (== 'x 'y)))
(define exampleo (rel 'foo '(y x) (gor (== 'y "fish")
									   (== 'x "horse"))))
(define rels (list sameo exampleo))

(define (lookup-rel name env)
  (match env
	[(cons (rel rname args goal) rest)
	 #:when (eqv? name rname)
     `(,args . ,goal)]
	[(cons _ rest)
	 (lookup-rel name rest)]
	[_ (error "Relation not found")]))

(define (dmitry/k t k)
  (match t
	[(trip (== t1 t2) s n)
	 (cond
	   [(unify t1 t2 s)
		=> (lambda (s^)
			 (displayln "UnifySuccess")
			 (k (list (mt) `(,s^ ,n))))]
	   [else (displayln "UnifyFail")
			 (k (list (mt) #f))])]

	[(trip (gor g1 g2) s n)
	 (displayln "Disj")
	 (k (list (tor (trip g1 s n) (trip g2 s n)) #f))]

	[(trip (gand g1 g2) s n)
	 (displayln "Conj")
	 (k (list (tand (trip g1 s n) g2) #f))]

	[(trip (fresh x g) s n)
	 (displayln "Fresh")
     (k (list (trip (substitute x n g) s (add1 n)) #f))]

	[(trip (relcall name ts) s n)
	 (displayln "Invoke")
	 (k (list (trip (invoke name ts) s n) #f))]

	[(tor t1 t2)
	 (dmitry/k t1
			   (λ (t1^)
				 (match t1^
				   [(list (mt) #f)
					(displayln "DisjStop")
					(k `(,t2 #f))]

				   [(list (mt) b)
					(displayln "DisjStopAns")
					(k `(,t2 ,b))]

				   [(list s #f)
					(displayln "DisjStep")
					(k `(,(tor t2 s) #f))]

				   [(list s b)
					(displayln "DisjStepAns")
					(k `(,(tor t2 s) ,b))])))]

	[(tand t g)
	 (dmitry/k t
			   (λ (t^)
				 (match t^
				   [(list (mt) #f)
					(displayln "ConjStop")
					(k `(,(mt) #f))]

				   [(list (mt) `(,subst ,n))
					(displayln "ConjStopAns")
					(k `(,(trip g subst n) #f))]

				   [(list s #f)
					(displayln "ConjStep")
					(k `(,(tand s g) #f))]

				   [(list t `(,subst ,n))
					(displayln "ConjStepAns")
					(k `(,(tor (trip g subst n) (tand t g)) #f))])))]
	))

(define end/k (lambda (x) x))

(define (dmitry-capture-output prog)
  (let ([out (open-output-string)])
	(parameterize ([current-output-port out])
	  (define res (dmitry/k prog end/k))
	  (values res (get-output-string out)))))

(define-test-suite DMITRY

  (test-case "subst*-term/goal w/subst"
			 (check-equal?
			   (subst*-goal (list 'y) (list '0) (== "cat" 'y))
               (== "cat" 0)))

  (test-case "UnifySuccess"
			 (define-values (res out)
			   (dmitry-capture-output (trip (== 0 "abc") '() 1)))
			 (check-equal? res (list (mt) '(((0 "abc")) 1)))
			 (check-equal? out "UnifySuccess\n"))

  (test-case "UnifyFail"
			 (define-values (res out)
			   (dmitry-capture-output (trip (== 'dog 'cat) '() 0)))
			 (check-equal? res (list (mt) #f))
			 (check-equal? out "UnifyFail\n"))

  (test-case "Disj"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (gor (== 'left 'left)
						   (== 'right 'right))
					  '() 0)))
			 (check-equal? res
						   (list (tor (trip (== 'left 'left) '() 0)
									  (trip (== 'right 'right) '() 0))
								 #f))
			 (check-equal? out "Disj\n"))

  (test-case "Conj"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (gand (== 'left 'left)
							(== 'right 'right))
					  '() 0)))
			 (check-equal? res
						   (list (tand (trip (== 'left 'left) '() 0)
									   (== 'right 'right))
								 #f))
			 (check-equal? out "Conj\n"))

  (test-case "Fresh - Over Unification"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (fresh 'x (== 'x "abc"))
					  '() 0)))
			 (check-equal? res (list (trip (== 0 "abc") '() 1) #f))
			 (check-equal? out "Fresh\n"))

  (test-case "Fresh - Nested Same Var"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (fresh 'x (fresh 'x (== 'x "abc")))
					  '() 0)))
			 (check-equal? res (list (trip (fresh 'x (== 'x "abc")) '() 1) #f))
			 (check-equal? out "Fresh\n"))

  (test-case "Fresh - Nested Different Vars"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (fresh 'x (fresh 'y (== 'x 'y)))
					  '() 0)))
			 (check-equal? res (list (trip (fresh 'y (== 0 'y)) '() 1) #f))
			 (check-equal? out "Fresh\n"))

  (test-case "Fresh - Over Conj"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (fresh 'x (gand (== 'x "dog")
									  (== "cat" 'x)))
					  '() 0)))
			 (check-equal?
			  res
			  (list (trip (gand (== 0 "dog")
								(== "cat" 0))
						  '() 1)
					#f))
			 (check-equal? out "Fresh\n"))

  (test-case "Fresh - Over Disj"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (fresh 'x (gor (== 'x "dog")
									 (== "cat" 'x)))
					  '() 0)))
			 (check-equal?
			  res
			  (list (trip (gor (== 0 "dog")
							   (== "cat" 0))
						  '() 1)
					#f))
			 (check-equal? out "Fresh\n"))

  (test-case "Fresh - Over Relcall"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (fresh 'x (relcall 'sameo '(x "dog")))
					  '() 0)))
			 (check-equal?
			  res
			  (list (trip (relcall 'sameo '(0 "dog"))
						  '() 1)
					#f))
			 (check-equal? out "Fresh\n"))

  (test-case "Invoke - Relation Exists"
			 (define-values (res out)
			   (dmitry-capture-output
				(trip (relcall 'sameo '("dog" "cat"))
					  '() 0)))
			 (check-equal? res (list (trip (== "dog" "cat") '() 0) #f))
			 (check-equal? out "Invoke\n"))

  (test-case "Invoke - Relation Does Not Exist"
			 (check-exn exn:fail?
						(lambda ()
						  (dmitry-capture-output
						   (trip (relcall 'goneo '()) '() 0)))))

  (test-case "DisjStop"
			 (define-values (res out)
			   (dmitry-capture-output
				(tor (trip (== "abc" "def") '() 0)
					 (trip (fresh 'x (relcall 'sameo (list 'x "dog"))) '() 0))))
			 (check-equal? res
						   (list (trip (fresh 'x (relcall 'sameo (list 'x "dog"))) '() 0) #f))
			 (check-equal? out "UnifyFail\nDisjStop\n"))

  (test-case "DisjStopAns"
			 (define-values (res out)
			   (dmitry-capture-output
				(tor (trip (== "abc" "abc") '() 0)
					 (trip (fresh 'x (relcall 'sameo (list 'x "dog"))) '() 0))))
			 (check-equal? res
						   (list (trip (fresh 'x (relcall 'sameo (list 'x "dog"))) '() 0) '(() 0)))
			 (check-equal? out "UnifySuccess\nDisjStopAns\n"))

  (test-case "DisjStopAns - Nested Tor"
			 (define-values (res out)
			   (dmitry-capture-output
				(tor (tor (trip (== "abc" "abc") '() 0)
						  (trip (== "def" "def") '() 0))
					 (trip (== "ghi" "ghi") '() 0))))
			 (check-equal? res
						   (list (tor (trip (== "ghi" "ghi") '() 0)
									  (trip (== "def" "def") '() 0))
								 '(() 0)))
			 (check-equal? out "UnifySuccess\nDisjStopAns\nDisjStepAns\n"))

  (test-case "DisjStep"
			 (define-values (res out)
			   (dmitry-capture-output
				(tor (tor (trip (fresh 'x (== 'x "abc")) '() 0)
						  (trip (== "def" "def") '() 0))
					 (trip (== "ghi" "ghi") '() 0))))
			 (check-equal? res
						   (list (tor (trip (== "ghi" "ghi") '() 0)
									  (tor (trip (== "def" "def") '() 0)
										   (trip (== 0 "abc") '() 1)))
								 #f))
			 (check-equal? out "Fresh\nDisjStep\nDisjStep\n"))

  (test-case "DisjStepAns"
			 (define-values (res out)
			   (dmitry-capture-output
				(tor (tor (trip (== "abc" "abc") '() 0)
						  (trip (== "def" "def") '() 0))
					 (trip (fresh 'x (== 'x "dog")) '() 0))))
			 (check-equal? res
						   (list (tor (trip (fresh 'x (== 'x "dog")) '() 0)
									  (trip (== "def" "def") '() 0))
								 '(() 0)))
			 (check-equal? out "UnifySuccess\nDisjStopAns\nDisjStepAns\n"))

  (test-case "ConjStop"
			 (define-values (res out)
			   (dmitry-capture-output
				(tand (trip (== "abc" "def") '() 0)
					  (== "ghi" "ghi"))))
			 (check-equal? res (list (mt) #f))
			 (check-equal? out "UnifyFail\nConjStop\n"))

  (test-case "ConjStopAns"
			 (define-values (res out)
			   (dmitry-capture-output
				(tand (trip (== "abc" "abc") '() 0)
					  (== "def" "def"))))
			 (check-equal? res
						   (list (trip (== "def" "def") '() 0) #f))
			 (check-equal? out "UnifySuccess\nConjStopAns\n"))

  (test-case "ConjStep"
			 (define-values (res out)
			   (dmitry-capture-output
				(tand (trip (relcall 'sameo '("dog" "dog")) '() 0)
					  (== "abc" "abc"))))
			 (check-equal? res
						   (list (tand (trip (== "dog" "dog") '() 0)
									   (== "abc" "abc"))
								 #f))
			 (check-equal? out "Invoke\nConjStep\n"))

  (test-case "ConjStepAns"
			 (define-values (res out)
			   (dmitry-capture-output
				(tand (tor (trip (== "abc" "abc") '() 0)
						   (trip (== "def" "def") '() 0))
					  (== "ghi" "ghi"))))
			 (check-equal? res
						   (list (tor (trip (== "ghi" "ghi") '() 0)
									  (tand (trip (== "def" "def") '() 0)
											(== "ghi" "ghi")))
								 #f))
			 (check-equal? out "UnifySuccess\nDisjStopAns\nConjStepAns\n"))
  )
