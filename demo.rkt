#lang racket
(require minikanren)
(require (rename-in redex/reduction-semantics
                    (fresh f)))
(require redex/gui)
(require rackunit)
(check-redundancy #t)
(require redex-etc)
(require "definitions.rkt" "reduction-relations.rkt")

(defrel (add x y)
  (conde
    [(== x "z") (== y `("s" . "z"))]
    [(fresh (x^ y^)
       (== x `("s" . ,x^))
       (== y `("s" . ,y^))
       (add x^ y^))]))

(run* (q) (add '("s" "s" "s" . "z") q))
(run* (q) (add q '("s" "s" "s" . "z")))

(stepper
 red
 (term (prog
        ((r:add (x:x x:y)
                ((∃ (x:x^ x:y^) ((x:x =? ("s" : x:x^))
                                 ∧
                                 ((x:y =? ("s" : x:y^))
                                  ∧
                                  (r:add x:x^ x:y^))))
                 ∨
                 ((x:x =? "z")
                  ∧
                  (x:y =? ("s" : x:x))))))
        ((∃ (x:q x:n) (r:add x:n x:q)) (state () 0)))))

#;(stepper
 red
 (term (prog
        ((r:add (x:x x:y)
                (((x:x =? "z")
                  ∧
                  (x:y =? ("s" : x:x)))
                 ∨
                 (∃ (x:x^ x:y^) ((x:x =? ("s" : x:x^))
                                 ∧
                                 ((x:y =? ("s" : x:y^))
                                  ∧
                                  (r:add x:x^ x:y^)))))))
        ((∃ (x:q) (r:add x:q ("s" : ("s" : ("s" : "z"))))) (state () 0)))))

(stepper
 red
 (term (prog
        ((r:poso (x:n)
                 (∃ (x:a x:d)
                    ((x:a : x:d) =? x:n)))
         (r:dfao (x:l x:state)
                 (((x:l =? empty) ∧ (x:state =? "q1"))
                  ∨
                  (∃ (x:a x:d x:next-state)
                     ((x:l =? (x:a : x:d))
                      ∧
                      ((((x:a =? "z")
                         ∧
                         ((r:poso x:d)
                          ∧
                          (((x:state =? "q1")
                            ∧
                            (x:next-state =? "q1"))
                           ∨
                           (((x:state =? "q2")
                             ∧
                             (x:next-state =? "q3"))
                            ∧
                            ((x:state =? "q3")
                             ∧
                             (x:next-state =? "q2"))))))
                        ∨
                        ((x:a =? "s")
                         ∧
                         (((x:state =? "q1")
                           ∧
                           (x:next-state =? "q2"))
                          ∨
                          (((x:state =? "q2")
                            ∧
                            (x:next-state =? "q1"))
                           ∧
                           ((x:state =? "q3")
                            ∧
                            (x:next-state =? "q3"))))))
                       ∧
                       (r:dfao x:d x:next-state)))))))
        ((∃ (x:q) (r:dfao x:q "q1")) (state () 0)))))



(apply-reduction-relation* red (term (prog () ((("ghi" =? "ghi") (state () 2)) +-> ((("def" =? "def") (state () 1)) +-> (("abc" =? "abc") (state () 0)))))))

(apply-reduction-relation* red (term (prog () (((("abc" =? "abc") (state () 0)) <-+ (("def" =? "def") (state () 1))) <-+ (("ghi" =? "ghi") (state () 2))))))

(apply-reduction-relation* red (term (prog () (((("def" =? "def") (state () 1)) +-> (("abc" =? "abc") (state () 0))) <-+ (("ghi" =? "ghi") (state () 2))))))

(apply-reduction-relation* red (term (prog () (((("def" =? "def") (state () 1)) <-+ (("ghi" =? "ghi") (state () 2))) +-> (("abc" =? "abc") (state () 0)))))) 
       