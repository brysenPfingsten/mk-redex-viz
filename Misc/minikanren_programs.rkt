#lang racket
(require minikanren)

(defrel (appendo l s out)
  (conde
   [(== l '()) (== s out)]
   [(fresh (a d res)
           (== `(,a . ,d) l)
           (== `(,a . ,res) out)
           (appendo d s res))]))

(run* (q) (fresh (l s)
                 (appendo l s '(dog cat bear))
                 (== `(,l ,s) q)))

#|
'((() (dog cat bear))
  ((dog) (cat bear))
  ((dog cat) (bear))
  ((dog cat bear) ()))


'((⊤ (state ((1 ("dog" : ("cat" : ("bear" : empty))))
             (0 empty)) 2))
  +
  ((⊤ (state ((1 ("cat" : ("bear" : empty)))
              (3 empty)
              (4 ("cat" : ("bear" : empty)))
              (2 "dog")
              (0 (2 : 3))) 5))
   +
   ((⊤ (state ((1 ("bear" : empty))
               (6 empty)
               (7 ("bear" : empty))
               (5 "cat")
               (3 (5 : 6))
               (4 ("cat" : ("bear" : empty)))
               (2 "dog")
               (0 (2 : 3))) 8))
    +
    ((⊤ (state ((1 empty)
                (9 empty)
                (10 empty)
                (8 "bear")
                (6 (8 : 9))
                (7 ("bear" : empty))
                (5 "cat")
                (3 (5 : 6))
                (4 ("cat" : ("bear" : empty)))
                (2 "dog")
                (0 (2 : 3))) 11)) + ()))))

|#
