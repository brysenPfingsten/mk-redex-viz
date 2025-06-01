#lang racket

(require redex
         redex/reduction-semantics
         redex/gui)

(require "../dmitry-and-dmitry.rkt")

(stepper
 dmitry-and-dmitry
 (term (prog ((r:appendo (x:l x:s x:out)
                         (((x:l =? empty "u2") ∧ (x:out =? x:s "u3") "c1")
                          ∨
                          (∃ (x:a)
                             (∃ (x:d)
                                (∃ (x:res)
                                   (((x:l =? (x:a : x:d) "u7") ∧ (x:out =? (x:a : x:res) "u8") "c6")
                                    ∧
                                    (r:appendo x:d x:s x:res "r9")
                                    "c5")
                                   "f4") "f5") "f6")
                                "d0")))
             ((∃ (x:q)
                 (r:appendo ((sym "dog") : ((sym "cat") : ((sym "bear") : ((sym "lion") : empty))))
                             ((sym "bear") : empty)
                             x:q
                             "r21")
                 "f20")
              (state () 0 ())))))
