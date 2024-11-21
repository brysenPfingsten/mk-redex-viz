#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)
(require redex-etc)

(require "definitions.rkt" "judgment-forms.rkt" "reduction-relations.rkt")

(module+ test

  ;; FAILS
  ;; "bald symbols are terms"
  ;; (redex-match? L t (term cat))

  (test-true
   "lists are terms"
   (redex-match? L t (term (cons "cat" (cons #t empty)))))
  
  (test-true
   "bald numbers are terms"
   (redex-match? L t (term 5)))

  (test-true
   "booleans are terms"
   (redex-match? L t (term #t)))

  (test-true
   "strings are terms"
   (redex-match? L t (term "cat")))

  (test-true
   "a fresh call over an equation is a goal"
   (redex-match? L g (term (∃ x:x (x:x =? "abc")))))

  (test-true
   "an empty list is a substitution"
   (redex-match? L sub (term ())))

  (test-true
   "A `state` tag w/empty subst and 0 is a σ"
   (redex-match? L σ (term (state () 0))))

  (test-true
   "A goal w/a state is a search tree"
   (redex-match? L s (term ((∃ x:x (x:x =? "abc")) (state () 0)))))

  (test-true
   "A goal w/a state is a query expression"
   (redex-match? L e (term ((∃ x:x (x:x =? "abc")) (state () 0)))))

  (test-true
   "A program w/a single relation and an empty search tree is a program"
   (redex-match? L p (term (prog ((r:add x:x (x:x =? "cat"))) ()))))

  (test-true
   "matching a small unify program with symbol constants"
   (redex-match? L p (term (prog () (("cat" =? "cat") (state () 0))))))

  (test-true
   "matching a small unify program with symbol constants and non-empty subst"
   (redex-match?
    L
    p
    (term (prog () (("abc" =? "abc") (state ((2 "fish")) 0))))))

  (test-true
   "matches a program w a relation"
   (redex-match?
    L
    p
    (term
     (prog ((r:add x:x (x:x =? "cat"))) (⊤ (state () 0))))))

  (test-true
   "matching a full program with a relation call"
   (redex-match?
    L
    p
    (term
     (prog ((r:add x:x (x:x =? "cat"))) ((r:add "dog") (state () 0))))))

  (test-true
   "Small relation lookup matches reduction pattern"
   (redex-match?
    L
    (prog ((r_0 x_0 g_0) ... (r_1 x_1 g_1) (r_2 x_2 g_2) ...) (in-hole Ev (in-hole Es ((r_1 t) σ))))
    (term (prog ((r:foo x:x ("abc" =? "abc"))) ((r:foo "abc") (state () 0))))))

  (redex-match?
   L
   p
   (term
    (prog
     ((r:add x:x (∃ x:a
                    (∃ x:d
                       ((x:x =? (x:a : x:d))
                        ∧ (((x:a =? "z")
                            ∧ (x:d =? ("s" : "z")))
                           ∨ (∃ x:a2
                                (∃ x:d2
                                   (((x:a : x:d) =? (("s" : x:a2) : ("s" : x:d2)))
                                    ∧ (r:add (x:a2 : x:d2)))))))))))
     ((∃ x:y (r:add (("s" : ("s" : ("s" : "z"))) : x:y))) (state () 0)))))


  (test-results))

(module+ test

  (test-equal
   (term (unify "abc" "abc" ((2 "fish"))))
   (term ((2 "fish"))))

  (test-equal
   (term (unify 0 0 ()))
   (term ()))

  (test-equal
   (term (unify 0 "cat" ()))
   (term ((0 "cat"))))

  (test-equal
   (term (unify "cat" 0 ()))
   (term ((0 "cat"))))

  (test-equal
   (term (walk 0 ((1 "cat") (0 "dog"))))
   (term "dog"))

  (test-equal
   (term (walk 0 ((1 "cat") (0 1))))
   (term "cat"))

  (test-equal
   (term
    (unify (walk (1 : 2) ((0 2) (1 "z")))
           (walk (("s" : 3) : ("s" : 4)) ((0 2) (1 "z")))
           ((0 2) (1 "z"))))
   (term #f))

  (test-results))

(module+ test
  (test-->>
   red
   (term (prog () (("abc" =? "abc") (state () 0))))
   (term (prog () (⊤ (state () 0)))))

  (test-->>
   red
   (term (prog () ((∃ x:x ⊤) (state () 0))))
   (term (prog () (⊤ (state () 1)))))

  (test-->>
   red
   (term (prog ((r:foo x:x ("abc" =? "abc"))) ((r:foo "abc") (state () 0))))
   (term (prog ((r:foo x:x ("abc" =? "abc"))) (⊤ (state () 0)))))

  (test-->>
   red
   (term (prog ((r:foo x:x x:y (x:y =? x:x))) ((r:foo "abc" "abc") (state () 0))))
   (term (prog ((r:foo x:x x:y (x:y =? x:x))) (⊤ (state () 0)))))

  (test-->>
   red
   (term
    (prog
     ()
     ((⊥ (state ((3 "x")) 0))
      +
      (("abc" =? "abc")
       (state ((3 "x")) 0)))))
   (term (prog () (⊤ (state ((3 "x")) 0)))))

  (test-->>
   red
   #:equiv alpha-equivalent?
   (term (prog ((r:foo x:x ("abc" =? "abc"))) ((r:foo "abc") (state () 0))))
   (term (prog ((r:foo x:x ("abc" =? "abc"))) (⊤ (state () 0)))))

  (test-->>
   red
   #:equiv alpha-equivalent?
   (term (prog () (("abc" =? "abc") (state ((2 "fish")) 0))))
   (term (prog () (⊤ (state ((2 "fish")) 0)))))

  ;; Tests substitution doesn't subst constants


  (test-->>
   red
   #:equiv alpha-equivalent?
   (term
    (prog ((r:add x:x (x:x =? "cat"))) ((r:add "dog") (state () 0))))
   (term
    (prog ((r:add x:x (x:x =? "cat"))) ())))

  (test-->>
   red
   #:equiv alpha-equivalent?
   (term (prog () ((⊤ (state ((3 "fish")) 0))
                   +
                   ((⊤ (state ((3 "fish")) 0))
                    +
                    ((⊤ (state ((3 "fish")) 0))
                     +
                     ((("nine" =? "nine") (state ((3 "fish")) 0))
                      +
                      (("ghi" =? "ghi") (state ((3 "fish")) 0))))))))

   (term (prog () ((⊤ (state ((3 "fish")) 0))
                   +
                   ((⊤ (state ((3 "fish")) 0))
                    +
                    ((⊤ (state ((3 "fish")) 0))
                     +
                     ((⊤ (state ((3 "fish")) 0))
                      +
                      (⊤ (state ((3 "fish")) 0)))))))))

  (test-->>
   red
   (term
    (prog () ((delay (("abc" =? "abc") (state ((3 "fish")) 0))) + (delay (("def" =? "def") (state ((4 "fish")) 0))))))
   (term (prog () ((⊤ (state ((3 "fish")) 0)) + (⊤ (state ((4 "fish")) 0))))))

  (test-->>
   red
   (term (prog () (("six" =? "abc") (state ((3 "fish")) 0))))
   (term (prog () ())))

  (test-->>
   red
   (term (prog () ((⊥ #f) + (⊤ (state ((3 "boba-tea")) 0)))))
   (term (prog () (⊤ (state ((3 "boba-tea")) 0)))))

  (test-->>
   red
   (term (prog () ((⊤ (state ((3 "fish")) 0)) + (⊥ #f))))
   (term (prog () ((⊤ (state ((3 "fish")) 0)) + ()))))

  (test-->>
   red
   (term (prog () ((⊤ (state ((3 "fish")) 0)) + (⊥ #f))))
   (term (prog () ((⊤ (state ((3 "fish")) 0)) + ()))))

  (test-->>
   red
   (term (prog () (((delay (("abc" =? "abc") (state ((3 "fish")) 0)))
                    + (delay (("def" =? "def") (state ((4 "fish")) 0))))
                   + (("nine" =? "nine") (state ((9 "fish")) 0)))))
   (term (prog () ((⊤ (state ((9 "fish")) 0))
                   + ((⊤ (state ((3 "fish")) 0))
                      + (⊤ (state ((4 "fish")) 0)))))))


  ;; This asymmetry mirrors prolog's behavior re: choice oints.
  #|
  ?- 7 = 7 ; 6 = 7.
  true
  ;  false.
  ?- 6 = 7; 7 = 7.
  true.
  ?-
  |#
  (test-->>
   red
   (term (prog () ((("six" =? "abc") ∨ ("abc" =? "abc")) (state ((3 "hat")) 0))))
   (term (prog () (⊤ (state ((3 "hat")) 0)))))

  (test-->>
   red
   (term (prog () ((("abc" =? "abc") ∨ ("six" =? "abc")) (state ((3 "gerbil")) 0))))
   (term (prog () ((⊤ (state ((3 "gerbil")) 0)) + ()))))

  (test-->>
   red
   (term (prog () (((((⊤
                       ∧ ("abc" =? "abc"))
                      ∨ (("def" =? "def")
                         ∧ ("nine" =? "nine")))
                     ∧ ((⊤
                         ∧ ("abc" =? "abc"))
                        ∨ (("def" =? "def")
                           ∧ ("nine" =? "nine"))))
                    ∨ (((⊤
                         ∧ ("abc" =? "abc"))
                        ∨ (("def" =? "def")
                           ∧ ("nine" =? "nine")))
                       ∧ ((⊤
                           ∧ ("abc" =? "abc"))
                          ∨ (("def" =? "def")
                             ∧ ("nine" =? "nine")))))
                   (state ((3 "fish")) 0))))
   (term
    (prog () ((⊤ (state ((3 "fish")) 0))
              +
              ((⊤ (state ((3 "fish")) 0))
               +
               ((⊤ (state ((3 "fish")) 0))
                +
                ((⊤ (state ((3 "fish")) 0))
                 +
                 ((⊤ (state ((3 "fish")) 0))
                  +
                  ((⊤ (state ((3 "fish")) 0))
                   +
                   ((⊤ (state ((3 "fish")) 0))
                    +
                    (⊤
                     (state
                      ((3 "fish"))
                      0))))))))))))

  (test-->>
   red
   (term
    (prog ((r:add x:x (∃ x:a
                         (∃ x:d
                            ((x:x =? (x:a : x:d))
                             ∧ (((x:a =? "z")
                                 ∧ (x:d =? ("s" : "z")))
                                ∨ (∃ x:a2
                                     (∃ x:d2
                                        (((x:a : x:d) =? (("s" : x:a2) : ("s" : x:d2)))
                                         ∧ (r:add (x:a2 : x:d2)))))))))))
          ((∃ x:y (x:y =? x:y))
           (state () 0))))
   (term (prog ((r:add x:x (∃ x:a
                              (∃ x:d
                                 ((x:x =? (x:a : x:d))
                                  ∧ (((x:a =? "z")
                                      ∧ (x:d =? ("s" : "z")))
                                     ∨ (∃ x:a2
                                          (∃ x:d2
                                             (((x:a : x:d) =? (("s" : x:a2) : ("s" : x:d2)))
                                              ∧ (r:add (x:a2 : x:d2)))))))))))
               (⊤ (state () 1)))))


  (test-->>
   red
   (term
    (prog ((r:add x:x (∃ x:a (x:a =? x:x))))
          ((∃ x:y (r:add (("s" : "z") : x:y)))
           (state () 0))))
   (term
    (prog ((r:add x:x (∃ x:a (x:a =? x:x))))
          (⊤ (state ((1 (("s" : "z") : 0))) 2)))))

  (test-->>
   red
   #:equiv alpha-equivalent?
   (term
    (prog
     ((r:add x:x (∃ x:a
                    (∃ x:d
                       (x:x =? (x:a : x:d))))))
     ((∃ x:y (r:add (("s" : ("s" : ("s" : "z"))) : x:y)))
      (state () 0))))
   (term (prog
          ((r:add x:x (∃ x:a (∃ x:d (x:x =? (x:a : x:d))))))
          (⊤ (state ((0 2) (1 ("s" : ("s" : ("s" : "z"))))) 3)))))

  (test-->>
   red
   #:equiv alpha-equivalent?
   (term
    (prog
     ((r:add x:x
             (∃ x:a
                (∃ x:d ((x:x =? (x:a : x:d))
                        ∧
                        (((x:a =? "z")
                          ∧ (x:d =? ("s" : "z")))
                         ∨ (∃ x:a2
                              (∃ x:d2
                                 (((x:a : x:d) =? (("s" : x:a2) : ("s" : x:d2)))
                                  ∧ (r:add (x:a2 : x:d2)))))))))))
     ((∃ x:y (r:add (("s" : ("s" : ("s" : "z"))) : x:y))) (state () 0))))
   (term
    (prog
     ((r:add x:x
             (∃ x:a
                (∃ x:d ((x:x =? (x:a : x:d))
                        ∧
                        (((x:a =? "z")
                          ∧ (x:d =? ("s" : "z")))
                         ∨ (∃ x:a2
                              (∃ x:d2
                                 (((x:a : x:d) =? (("s" : x:a2) : ("s" : x:d2)))
                                  ∧ (r:add (x:a2 : x:d2)))))))))))
     ((⊤
       (state
        ((14 ("s" : "z"))
         (12 14)
         (13 "z")
         (10 ("s" : 12))
         (11 "z")
         (8 10)
         (9 ("s" : "z"))
         (6 ("s" : 8))
         (7 ("s" : "z"))
         (4 6)
         (5 ("s" : ("s" : "z")))
         (2 ("s" : 4))
         (3 ("s" : ("s" : "z")))
         (0 2)
         (1 ("s" : ("s" : ("s" : "z")))))
        15))
      +
      ()))))

  #;(test-->>
     red
     (term (prog ((r:appendo x:l x:s x:out
                             (((x:l =? empty) ∧ (x:s =? x:out))
                              ∨
                              (∃ x:a x:d x:res
                                 (((x:a : x:d) =? x:l)
                                  ∧
                                  (((x:a : x:res) =? x:out)
                                   ∧
                                   (r:appendo x:d x:s x:res)))))))
                 ((∃ x:q (r:appendo (( : 2) : empty) ((3 : 4) : empty) x:q))
                  (state () 0))
                 )))
  )

(module+ traces

  (test-->>
   red
   #:equiv alpha-equivalent?
   (term (prog ((r:foo x:x ("abc" =? "abc"))) ((r:foo "six") (state () 0))))
   (term (prog ((r:foo x:x ("abc" =? "abc"))) (⊤ (state () 0)))))

  ;; This is a state mid-run
  (test-->>
   red
   #:equiv alpha-equivalent?
   (term (prog () (((∃ x:x ("abc" =? "abc")) (state () 0)) + (⊤ (state () 0)))))
   (term (prog () ((⊤ (state () 1)) + (⊤ (state () 0))))))

  ;; This is a state mid-run
  (test-->>
   red
   #:equiv alpha-equivalent?
   (term (prog () (((∃ x:x ("abc" =? "abc")) ∨ ⊤) (state () 0))))
   (term (prog () ((⊤ (state () 1)) + (⊤ (state () 0))))))

  (test-->>
   red
   #:equiv alpha-equivalent?
   (term
    (prog
     ((r:add x:x
             (∃ x:a
                (∃ x:d ((x:x =? (x:a : x:d))
                        ∧
                        (((x:a =? "z")
                          ∧ (x:d =? ("s" : "z")))
                         ∨ (∃ x:a2
                              (∃ x:d2
                                 (((x:a : x:d) =? (("s" : x:a2) : ("s" : x:d2)))
                                  ∧ (r:add (x:a2 : x:d2)))))))))))
     ((∃ x:y (r:add ("z" : x:y))) (state () 0))))
   (term
    (prog
     ()
     ((⊤ (state ((3 "x")) 0))
      +
      ((⊤ (state ((3 "x")) 0))
       +
       ((⊤ (state ((3 "x")) 0))
        +
        ((⊤ (state ((3 "x")) 0))
         +
         (⊤ (state ((3 "x")) 0)))))))))

  (traces
   red
   (term
    (prog ()
          (((((⊤
               ∧ ("abc" =? "abc"))
              ∨ (("def" =? "def")
                 ∧ ("nine" =? "nine")))
             ∧ ((⊥
                 ∨ ("abc" =? "abc"))
                ∨ (("def" =? "def")
                   ∧ ("nine" =? "nine"))))
            ∨ (((⊥
                 ∨ ("abc" =? "abc"))
                ∨ (("def" =? "def")
                   ∧ ("nine" =? "nine")))
               ∧ ((⊤
                   ∧ ("abc" =? "abc"))
                  ∨ (("def" =? "def")
                     ∧ ("nine" =? "nine")))))
           (state ((3 "x")) 0)))))

  (traces
   red
   (term
    (prog
     ()
     ((((⊤ (state ((3 "x")) 0))
        +
        (("def" =? "def")
         (state ((3 "x")) 0)))
       ×
       (("abc" =? "abc")
        ∨
        ("nine" =? "nine")))
      +
      (("ghi" =? "ghi")
       (state ((3 "x")) 0))))))

  (test-results))

(test-true
 "A program w/a no relations "
 (redex-match? L p (term (prog ()
                               (("abc" =? "abc") (state () 0))))))

(redex-match? L p (term (prog ()
                              ((⊤ ∨ ⊥) (state () 0)))))

#;(traces red (term (prog ()
                          ((((⊤ ∨ ⊥) ∧ ((⊥ ∨ ⊤) ∧ (⊥ ∨ ⊤))) ∨
                                                            ((⊥ ∨ ⊤) ∨ ((⊤ ∧ ⊤) ∨ (⊤ ∨ ⊥))))
                           (state () 0)))))

#|

⊤           ; Trivial success
     ⊥           ; Trivial failure
     (t =? t)    ; Syntactic equality
     (g ∨ g)     ; Disjunction
     (g ∧ g)     ; Conjuction
  
(((⊤ ∨ ⊥) ∧ ((⊥ ∨ ⊤) ∧ (⊥ ∨ ⊤))) ∨
((⊥ ∨ ⊤) ∨ ((⊤ ∧ ⊤) ∨ ((⊤ ∨ ⊥)))
)
run* (q) (disj

              
    
              
              (disj (disj fail succeed)
                    (disj (conj succeed succeed)
                          (disj succeed fail)))))
 |#

#|
(traces red (term (prog ((r:appendo x:l x:s x:out
                           (((x:l =? empty) ∧ (x:s =? x:out))
                            ∨
                            (∃ x:a x:d x:res
                               (((x:a : x:d) =? x:l)
                                ∧
                                (((x:a : x:res) =? x:out)
                                 ∧
                                 (r:appendo x:d x:s x:res)))))))
               ((∃ x:q (r:appendo ("cat" : ("dog" : empty)) ("bear" : ("lion" : empty)) x:q))
                (state () 0))
  )))

--->

'((prog
   ((r:appendo«10385»
     x:l«10386»
     x:s«10387»
     x:out«10388»
     (((x:l«10386» =? empty) ∧ (x:s«10387» =? x:out«10388»))
      ∨
      (∃ x:a x:d x:res (((x:a : x:d) =? x:l«10386») ∧ (((x:a : x:res) =? x:out«10388») ∧ (r:appendo«10385» x:d x:s«10387» x:res)))))))
   ((⊤ (state ((6 ("bear" : ("lion" : empty))) (3 (4 : 6)) (5 empty) (4 "dog") (0 (1 : 3)) (2 ("dog" : empty)) (1 "cat")) 7)) + ())))



(judgment-holds
   (closed-goal?
    ("abc" =? "abc") (r:hello)))

 (judgment-holds
   (closed-program?
    (prog ((r:hello x:1 ("abc" =? "abc"))) ((r:hello x:a) (state () 0)))))

Failed w/ undefined relations
 (redex-check L
               p
               (> 1 (length (apply-reduction-relation red (term p))))
               #:attempts 1000
               #:keep-going? #t
               #:attempt-size (λ (x) 100))

(judgment-holds
   (closed-tree?
    ((∃ x:q (r:appendo ("cat" : ("dog" : empty)) ("bear" : ("lion" : empty)) x:q))
                (state () 0)) (r:appendo)))

(judgment-holds
   (closed-goal?
    (∃ x:q (r:appendo ("cat" : ("dog" : empty)) ("bear" : ("lion" : empty)) x:q))
                 (r:appendo)))
(traces red (term (prog ((r:poso x:n (∃ x:a x:d (x:n =? (x:a : x:d)))))
                          ((∃ x:q (r:poso x:q)) (state () 0)))))

|#

(redex-check L
               p
                 (implies (and (not (redex-match L prog-val (term p)))
                             (judgment-holds (closed-program?  p)))
                        (= (length (apply-reduction-relation red (term p))) 1))
               #:attempts 1000
               #:print? (λ (p) #t)
               #:keep-going? #true)

#;(redex-check L
             s
             (judgment-holds
              (closed-tree? s ())))