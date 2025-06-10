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

(struct init:k (f*))

(define (apply-k k v)
  (match k
    [(subst-term:car var* val* term* k*)
     (subst-term/k var* val* (cdr term*) (subst-term:cdr v k*))]

    [(subst-term:cdr a* k*)
     (apply-k k* (cons a* v))]

    [(subst-terms:car var* val* lst* k*)
     (subst-terms/k var* val* (cdr lst*) (subst-terms:cdr v k*))]

    [(subst-terms:cdr a* k*)
     (apply-k k* (cons a* v))]

    [(substitute:==/t1 fresh-var* replace-var* t2* k*)
     (subst-term/k fresh-var* replace-var* t2*
                   (substitute:==/t2 v k*))]

    [(substitute:==/t2 t1* k*)
     (apply-k k* (== t1* v))]

    [(substitute:gor/g1 fresh-var* replace-var* g2* k*)
     (substitute/k fresh-var* replace-var* g2*
                   (substitute:gor/g2 v k*))]

     [(substitute:gor/g2 g1* k*)
      (apply-k k* (gor g1* v))]

    [(substitute:gand/g1 fresh-var* replace-var* g2* k*)
     (substitute/k fresh-var* replace-var* g2*
                   (substitute:gand/g2 v k*))]

     [(substitute:gand/g2 g1* k*)
      (apply-k k* (gand g1* v))]

     [(substitute:fresh x* k*)
      (apply-k k* (fresh x* v))]

     [(substitute:relcall name* k*)
      (apply-k k* (relcall name* v))]

     [(foldr:k args* terms* k*)
      (foldr/k args* terms* v k*)]

     [(invoke:k terms* k*)
      (define args (car v))
      (define goal (cdr v))
      (foldr/k args terms* goal k*)]

     [(dmitry:fresh s* n* k*)
      (apply-k k* (list (trip v s* (add1 n*)) #f))]

     [(dmitry:invoke s* n* k*)
      (apply-k k* (list (trip v s* n*) #f))]

     [(dmitry:tor t2* k*)
      (match v
        [(list (mt) #f)
        (displayln "DisjStop")
        (apply-k k* `(,t2* #f))]

        [(list (mt) b)
        (displayln "DisjStopAns")
        (apply-k k* `(,t2* ,b))]

        [(list s #f)
        (displayln "DisjStep")
        (apply-k k* `(,(tor t2* s) #f))]
        [(list s b)
        (displayln "DisjStepAns")
        (apply-k k* `(,(tor t2* s) ,b))])]

     [(dmitry:tand g* k*)
        (match v
          [(list (mt) #f)
          (displayln "ConjStop")
          (apply-k k* `(,(mt) #f))]
  
          [(list (mt) `(,subst ,n))
          (displayln "ConjStopAns")
          (apply-k k* `(,(trip g* subst n) #f))]

          [(list s #f)
          (displayln "ConjStep")
          (apply-k k* `(,(tand s g*) #f))]

          [(list t `(,subst ,n))
          (displayln "ConjStepAns")
          (apply-k k* `(,(tor (trip g* subst n) (tand t g*)) #f))])]
     
	[(init:k f*) (f* v)]))

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


(struct subst-term:car (var* val* term* k*))
(struct subst-term:cdr (a* k*))

(define (subst-term/k var val term k)
  (cond
    [(cons? term)
     (subst-terms/k var val (car term) (subst-term:car var val term k))]
    [(equal? term var) (apply-k k val)]
    [else (apply-k k term)]))

(struct subst-terms:car (var* val* lst* k*))
(struct subst-terms:cdr (a* k*))

(define (subst-terms/k var val lst k)
  (if (null? lst)
      (apply-k k '())
      (subst-term/k var val (car lst) (subst-terms:car var val lst k))))

(struct substitute:==/t1 (fresh-var* replace-var* t2* k*))
(struct substitute:==/t2 (t1* k*))
(struct substitute:gor/g1 (fresh-var* replace-var* g2* k*))
(struct substitute:gor/g2 (g1* k*))
(struct substitute:gand/g1 (fresh-var* replace-var* g2* k*))
(struct substitute:gand/g2 (g1* k*))
(struct substitute:fresh (x* k*))
(struct substitute:relcall (name* k*))

(define (substitute/k fresh-var replace-var goal k)
  (match goal
    [(== t1 t2)
     (subst-term/k fresh-var replace-var t1
                   (substitute:==/t1 fresh-var replace-var t2 k))]

    [(gor g1 g2)
     (substitute/k fresh-var replace-var g1
                   (substitute:gor/g1 fresh-var replace-var g2 k))]

    [(gand g1 g2)
     (substitute/k fresh-var replace-var g1
                   (substitute:gand/g1 fresh-var replace-var g2 k))]

    [(fresh x g)
     (if (eqv? x fresh-var)
         (apply-k k goal)
         (substitute/k fresh-var replace-var g
                       (substitute:fresh x k)))]

    [(relcall name terms)
     (subst-terms/k fresh-var replace-var terms
                    (substitute:relcall name k))]))


(define sameo (rel 'sameo '(x y) (== 'x 'y)))
(define exampleo (rel 'foo '(y x) (gor (== 'y "fish")
                                       (== 'x "horse"))))
(define rels (list sameo exampleo))

(define (lookup-rel/k name env k)
  (match env
    [(cons (rel rname args goal) rest)
     #:when (eqv? name rname)
     (apply-k k `(,args . ,goal))]

    [(cons _ rest)
     (lookup-rel/k name rest k)]

    [_ (error "Relation not found")]))

(struct foldr:k (args* terms* k*))

(define (foldr/k args terms goal k)
  (if (null? args)
      (apply-k k goal)
      (substitute/k (car args) (car terms) goal
                    (foldr:k (cdr args) (cdr terms) k))))

(struct invoke:k (terms* k*))

(define (invoke/k name terms k)
  (lookup-rel/k name rels
                (invoke:k terms k)))

(struct dmitry:fresh (s* n* k*))
(struct dmitry:invoke (s* n* k*))
(struct dmitry:tor (t2* k*))
(struct dmitry:tand (g* k*))

(define (dmitry/k t k)
  (match t
    [(trip (== t1 t2) s n)
     (let ((s^ (unify t1 t2 s)))
                (cond
                  [s^ (displayln "UnifySuccess")
                      (apply-k k (list (mt) `(,s^ ,n)))]
                  [else (displayln "UnifyFail")
                        (apply-k k (list (mt) #f))]))]

    [(trip (gor g1 g2) s n)
     (displayln "Disj")
     (apply-k k (list (tor (trip g1 s n) (trip g2 s n)) #f))]

    [(trip (gand g1 g2) s n)
     (displayln "Conj")
     (apply-k k (list (tand (trip g1 s n) g2) #f))]

    [(trip (fresh x g) s n)
     (displayln "Fresh")
     (substitute/k x n g
                   (dmitry:fresh s n k))]

    [(trip (relcall name ts) s n)
     (displayln "Invoke")
     (invoke/k name ts
               (dmitry:invoke s n k))]

    [(tor t1 t2)
     (dmitry/k t1
               (dmitry:tor t2 k))]
    [(tand t g)
     (dmitry/k t
               (dmitry:tand g k))]
    ))
                   

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
      (define res (dmitry/k prog (init:k identity)))
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

(module+ test
  (run-test DMITRY))
