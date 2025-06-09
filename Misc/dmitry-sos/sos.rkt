#lang racket
(require rackunit rackunit/text-ui)
(require racket/trace)

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


(define (walk t s)
  (if (not (number? t))
      t
      (match s
        [(cons (list v t^) _) 
         #:when (eq? t v)
         (walk t^ s)]
        [(cons _ s^) (walk t s^)]
        [_ t])))


(define (occurs? v t s)
  (match t 
    [(cons t1 t2) (or (occurs? v (walk t1 s) s)
                      (occurs? v (walk t2 s) s))]
    [t (eq? v (walk t s))]))


(define (extend v t s)
  (if (occurs? v t s)
      #f
      (cons (list v t) s)))


(define (unify t1 t2 s)
  (let* ([t1^ (walk t1 s)]
         [t2^ (walk t2 s)])
    (unify-help t1^ t2^ s)))


(define (unify-help t1 t2 s)
  (match (list t1 t2)
    [(list t t) s]
    [(list v t) #:when (number? v)
                (extend v t s)]
    [(list t v) #:when (number? v)
                (extend v t s)]
    [(list (cons t1a t1b) (cons t2a t2b))
     (define s^ (unify t1a t2a s))
     (unify t1b t2b s^)]
    [(list _ _)  #f]))


(define (subst-term var val term)
  (cond 
    [(cons? term)
     (cons (subst-term var val (car term))
           (subst-term var val (cdr term)))]
    [(equal? term var) val]
    [else term]))


(define (substitute fresh-var replace-var goal)
  (match goal
    [(== t1 t2)
     (== (subst-term fresh-var replace-var t1)
         (subst-term fresh-var replace-var t2))]
    [(gor g1 g2)
     (gor (substitute fresh-var replace-var g1)
          (substitute fresh-var replace-var g2))]
    [(gand g1 g2)
     (gand (substitute fresh-var replace-var g1)
           (substitute fresh-var replace-var g2))]
    [(fresh x g)
     (cond
       [(eqv? x fresh-var) goal]
       [else (fresh x (substitute fresh-var replace-var g))])]
    [(relcall name terms)
     (relcall name (map (lambda (t) (subst-term fresh-var replace-var t)) terms))]))

(define sameo (rel 'sameo '(x y) (== 'x 'y)))
(define exampleo (rel 'foo '(y x) (gor (== 'y "fish")
                                       (== 'x "horse"))))
(define rels (list sameo exampleo))

(define (lookup-rel name env)
  (match env
    [(cons (rel rname args goal) rest)
     #:when (eqv? name rname)
     (values args goal)]
    [(cons _ rest) (lookup-rel name rest)]
    [_ (error "Relation not found")]))

(define (invoke name terms)
  (define-values (args goal) (lookup-rel name rels))
  (for/fold ([g goal])
            ([arg args]
             [term terms])
    (substitute arg term g)))

(define dmitry 
  (lambda (t) 
    (match t
       
      [(trip (== t1 t2) s n)
       (define res (unify t1 t2 s))
       (cond
         [res (displayln "UnifySuccess")
              (list (mt) `(,res ,n))]
         [else (displayln "UnifyFail")
               (list (mt) #f)])]

      [(trip (gor g1 g2) s n)
       (displayln "Disj")
       (list (tor (trip g1 s n) (trip g2 s n)) #f)]

      [(trip (gand g1 g2) s n)
       (displayln "Conj")
       (list (tand (trip g1 s n) g2) #f)]

      [(trip (fresh x g) s n)
       (displayln "Fresh")
       (list (trip (substitute x n g) s (add1 n)) #f)]

      [(trip (relcall name ts) s n)
       (displayln "Invoke")
       (list (trip (invoke name ts) s n) #f)]

      [(tor t1 t2)
       (define res (dmitry t1))
       (match res
         [(list (mt) #f)
          (displayln "DisjStop")
          `(,t2 #f)]

         [(list (mt) b)
          (displayln "DisjStopAns")
          `(,t2 ,b)]

         [(list s #f)
          (displayln "DisjStep")
          `(,(tor t2 s) #f)]

         [(list s b)
          (displayln "DisjStepAns")
          `(,(tor t2 s) ,b)])]

      [(tand t g)
       (define res (dmitry t))
       (match res
         [(list (mt) #f)
          (displayln "ConjStop")
          `(,(mt) #f)]
           
         [(list (mt) `(,subst ,n))
          (displayln "ConjStopAns")
          `(,(trip g subst n) #f)]

         [(list s #f)
          (displayln "ConjStep")
          `(,(tand s g) #f)]

         [`(,t^ (,subst ,n))
          (displayln "ConjStepAns")
          `(,(tor (trip g subst n) (tand t^ g)) #f)])]
      ))) 

(define (mash-dmitry prog)
  (displayln prog)
  (let ([next (dmitry prog)])
    (match next
      [`(,(mt) #f) next]
      [(list tree _) (mash-dmitry tree)])))

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
             (occurs? 0 1 '((1 0))))

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
  )

(define (dmitry-capture-output prog)
  (let ([out (open-output-string)])
    (parameterize ([current-output-port out])
      (define res (dmitry prog))
      (values res (get-output-string out)))))

(define-test-suite DMITRY
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
                (tand (tor (trip (== "abc" "abc") '(state) 0)
                           (trip (== "def" "def") '() 0))
                      (== "ghi" "ghi"))))
             (check-equal? res
                           (list (tor (trip (== "ghi" "ghi") '(state) 0)
                                      (tand (trip (== "def" "def") '() 0)
                                            (== "ghi" "ghi")))
                                 #f))
             (check-equal? out "UnifySuccess\nDisjStopAns\nConjStepAns\n"))
  )

