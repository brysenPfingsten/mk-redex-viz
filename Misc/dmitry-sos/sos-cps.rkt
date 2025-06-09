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

(define (walk/k t s k)
  (if (not (number? t))
      (k t)
      (match s
        [(cons (list v t^) _) 
         #:when (eq? t v)
         (walk/k t^ s k)]
        [(cons _ s^) (walk/k t s^ k)]
        [_ (k t)])))


(define (occurs?/k v t s k)
  (match t 
    [(cons t1 t2) 
     (walk/k t1 s
             (λ (t1^)
               (walk/k t2 s
                       (λ (t2^)
                         (occurs?/k v t1^ s
                                    (λ (b1)
                                      (occurs?/k v t2^ s
                                                 (λ (b2)
                                                   (k (or b1 b2))))))))))]
    [t (walk/k t s
               (λ (t^)
                 (k (eq? v t^))))]))


(define (extend/k v t s k)
  (occurs?/k v t s
             (λ (b)
               (if b
                   (k #f)
                   (k (cons (list v t) s))))))


(define (unify/k t1 t2 s k)
  (walk/k t1 s
          (λ (t1^)
            (walk/k t2 s
                    (λ (t2^)
                      (unify-help/k t1^ t2^ s k))))))


(define (unify-help/k t1 t2 s k)
  (match (list t1 t2)
    [(list t t) (k s)]
    [(list v t) #:when (number? v)
                (extend/k v t s k)]
    [(list t v) #:when (number? v)
                (extend/k v t s k)]
    [(list (cons t1a t1b) (cons t2a t2b))
     (unify/k t1a t2a s
              (λ (s^)
                (unify/k t1b t2b s^ k)))]
    [(list _ _)  (k #f)]))


(define (subst-term/k var val term k)
  (cond 
    [(cons? term)
     (subst-term/k var val (car term)
                   (λ (a)
                     (subst-term/k var val (cdr term)
                                   (λ (d)
                                     (k (cons a d))))))]
    [(equal? term var) (k val)]
    [else (k term)]))


(define (subst-terms/k var val lst k)
  (if (null? lst)
      (k '())
      (subst-term/k var val (car lst)
                    (λ (a^)
                      (subst-terms/k var val (cdr lst)
                                     (λ (d^)
                                       (k (cons a^ d^))))))))


(define (substitute/k fresh-var replace-var goal k)
  (match goal
    [(== t1 t2)
     (subst-term/k fresh-var replace-var t1
                   (λ (t1^)
                     (subst-term/k fresh-var replace-var t2
                                   (λ (t2^)
                                     (k (== t1^ t2^))))))]

    [(gor g1 g2)
     (substitute/k fresh-var replace-var g1
                   (λ (g1^)
                     (substitute/k fresh-var replace-var g2
                                   (λ (g2^)
                                     (k (gor g1^ g2^))))))]

    [(gand g1 g2)
     (substitute/k fresh-var replace-var g1
                   (λ (g1^)
                     (substitute/k fresh-var replace-var g2
                                   (λ (g2^)
                                     (k (gand g1^ g2^))))))]

    [(fresh x g)
     (if (eqv? x fresh-var)
         (k goal)
         (substitute/k fresh-var replace-var g
                       (λ (g^)
                         (k (fresh x g^)))))]

    [(relcall name terms)
     (subst-terms/k fresh-var replace-var terms
                    (λ (terms^)
                      (k (relcall name terms^))))]))


(define sameo (rel 'sameo '(x y) (== 'x 'y)))
(define exampleo (rel 'foo '(y x) (gor (== 'y "fish")
                                       (== 'x "horse"))))
(define rels (list sameo exampleo))

(define (lookup-rel/k name env k)
  (match env
    [(cons (rel rname args goal) rest)
     #:when (eqv? name rname)
     (k `(,args . ,goal))]

    [(cons _ rest)
     (lookup-rel/k name rest k)]

    [_ (error "Relation not found")]))

(define (foldr/k args terms goal k)
  (if (null? args)
      (k goal)
      (substitute/k (car args) (car terms) goal
                    (λ (goal^)
                      (foldr/k (cdr args) (cdr terms) goal^ k)))))

(define (invoke/k name terms k)
  (lookup-rel/k name rels
                (λ (result)
                  (define args (car result))
                  (define goal (cdr result))
                  (foldr/k args terms goal k))))


(define (dmitry/k t k)
  (match t
    [(trip (== t1 t2) s n)
     (unify/k t1 t2 s 
              (λ (s^)
                (cond
                  [s^ (displayln "UnifySuccess")
                      (k (list (mt) `(,s^ ,n)))]
                  [else (displayln "UnifyFail")
                        (k (list (mt) #f))])))]

    [(trip (gor g1 g2) s n)
     (displayln "Disj")
     (k (list (tor (trip g1 s n) (trip g2 s n)) #f))]

    [(trip (gand g1 g2) s n)
     (displayln "Conj")
     (k (list (tand (trip g1 s n) g2) #f))]

    [(trip (fresh x g) s n)
     (displayln "Fresh")
     (substitute/k x n g
                   (λ (g^)
                     (k (list (trip g^ s (add1 n)) #f))))]

    [(trip (relcall name ts) s n)
     (displayln "Invoke")
     (invoke/k name ts
               (λ (g^)
                 (k (list (trip g^ s n) #f))))]

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

(define-test-suite MK-HELPERS
  (test-case "Non-Numeric Term Returns Itself"
             (check-equal? (walk/k 'dog '() end/k) 'dog)
             (check-equal? (walk/k "dog" '() end/k) "dog")
             (check-equal? (walk/k #t '() end/k) #t)
             (check-equal? (walk/k (cons 0 1) '() end/k) (cons 0 1)))

  (test-case "Numeric Term Walks To Correct Value"
             (check-equal? (walk/k 0 `((0 dog)) end/k) 'dog)
             (check-equal? (walk/k 0 `((0 1) (1 2) (2 dog)) end/k) 'dog)
             (check-equal? (walk/k 0 `((0 (1 2)) (1 "bear") (2 "bird")) end/k) (list 1 2)))

  (test-case "Numeric Term Not Bound Returns Itself"
             (check-equal? (walk/k 0 '() end/k) 0)
             (check-equal? (walk/k 5 '((0 1) (1 2) (2 3)) end/k) 5))

  (test-true "Logic Var Occurs In Itself"
             (occurs?/k 0 0 '() end/k))

  (test-false "Logic Var Does Not Appear In Ground Atomic Term"
              (occurs?/k 0 "not here" '() end/k))

  (test-true "Logic Var Appears Nested In A List"
             (occurs?/k 0 (cons (cons 1 2) (cons (cons 0 5) 12)) '() end/k))

  (test-true "Logic Var Appears In A Walked Term"
             (occurs?/k 0 1 '((1 0)) end/k))

  (test-false "Logic Var Does Not Appear In A List"
              (occurs?/k 0 (cons 1 2) '() end/k))

  (test-case "Substitution Is Extended When Occurs Check Passes"
             (check-false (occurs?/k 0 "dog" '() end/k))
             (check-equal? (extend/k 0 "dog" '() end/k) '((0 "dog"))))

  (test-case "Extending Substitution Fails When Occurs Check Fails"
             (check-true (occurs?/k 0 0 '() end/k))
             (check-false (extend/k 0 0 '() end/k)))

  (test-case "Unifying Two of the Same Terms Returns The Original Substitution"
             (check-equal? (unify/k 0 0 '() end/k) '())
             (check-equal? (unify/k "dog" "dog" '() end/k) '()))

  (test-case "Unifying A Logic Var And A Term Extends The Substitution"
             (check-equal? (unify/k 0 "dog" '() end/k) '((0 "dog")))
             (check-equal? (unify/k "dog" 0 '() end/k) '((0 "dog")))
             (check-equal? (unify/k 0 1 '((1 "cat")) end/k) '((0 "cat") (1 "cat"))))

  (test-case "Unifying Pairs Works"
             (check-equal? (unify/k (cons 0 "bear") (cons "eagle" 1) '() end/k)
                           '((1 "bear") (0 "eagle")))
             (check-equal? (unify/k (cons 0 1) (cons 2 3) '((0 "dog") (1 "cat") (2 "dog") (3 "cat")) end/k)
                           '((0 "dog") (1 "cat") (2 "dog") (3 "cat"))))

  (test-case "Unifying Two Different Terms Fails"
             (check-false (unify/k "dog" "cat" '() end/k))
             (check-false (unify/k 0 1 '((0 "dog") (1 #t)) end/k)))
  )

(define (dmitry-capture-output prog)
  (let ([out (open-output-string)])
    (parameterize ([current-output-port out])
      (define res (dmitry/k prog end/k))
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
