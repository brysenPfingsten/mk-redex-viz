#lang racket
(require redex)
(require "../src/metafunctions.rkt"
         "../src/definitions.rkt"
         "../src/reduction-relations/reduction-relations.rkt")
(require rackunit rackunit/text-ui)

(stepper red
  '(((∃
    (x:q)
    (r:reverseo
     ((sym "dog") : ((sym "cat") : ((sym "bear") : ((sym "lion") : empty))))
     x:q
     "r21")
    "f20")
   (state () 0 () "s"))
   ((r:appendo
    (x:l x:s x:out)
    (((x:l =? empty "u8") ∧ (x:out =? x:s "u9") "c7")
     ∨
     (∃
      (x:a x:d x:res)
      (((x:l =? (x:a : x:d) "u4") ∧ (x:out =? (x:a : x:res) "u5") "c3")
       ∧
       (r:appendo x:d x:s x:res "r6")
       "c2")
      "f1")
     "d0"))
   (r:reverseo
    (x:ls x:out)
    (((x:ls =? empty "u18") ∧ (x:out =? empty "u19") "c17")
     ∨
     (∃
      (x:a x:d x:res)
      (((x:ls =? (x:a : x:d) "u14") ∧ (r:reverseo x:d x:res "r15") "c13")
       ∧
       (r:appendo x:res (x:a : empty) x:out "r16")
       "c12")
      "f11")
     "d10")))
  ))

(redex-match? L (in-hole Ex ((g_1 ∧ g_2 o) σ)) (car '(((((((sym "dog")
      :
      ((sym "cat")
       :
       ((sym "bear")
        :
        ((sym "lion")
         :
         empty))))
     =?
     empty
     "u18")
    ∧
    (0 =? empty "u19")
    "c17")
   (state () 1 () "s"))
  <-+
  ((∃
    (x:a x:d x:res)
    (((((sym "dog")
        :
        ((sym "cat")
         :
         ((sym "bear")
          :
          ((sym "lion")
           :
           empty))))
       =?
       (x:a : x:d)
       "u14")
      ∧
      (r:reverseo«379»
       x:d
       x:res
       "r15")
      "c13")
     ∧
     (r:appendo«378»
      x:res
      (x:a : empty)
      0
      "r16")
     "c12")
    "f11")
   (state () 1 () "g17019")))
 ((r:appendo«378»
   (x:l«380»
    x:s«381»
    x:out«382»)
   (((x:l«380» =? empty "u8")
     ∧
     (x:out«382»
      =?
      x:s«381»
      "u9")
     "c7")
    ∨
    (∃
     (x:a x:d x:res)
     (((x:l«380»
        =?
        (x:a : x:d)
        "u4")
       ∧
       (x:out«382»
        =?
        (x:a : x:res)
        "u5")
       "c3")
      ∧
      (r:appendo«378»
       x:d
       x:s«381»
       x:res
       "r6")
      "c2")
     "f1")
    "d0"))
  (r:reverseo«379»
   (x:ls«383» x:out«384»)
   (((x:ls«383»
      =?
      empty
      "u18")
     ∧
     (x:out«384»
      =?
      empty
      "u19")
     "c17")
    ∨
    (∃
     (x:a x:d x:res)
     (((x:ls«383»
        =?
        (x:a : x:d)
        "u14")
       ∧
       (r:reverseo«379»
        x:d
        x:res
        "r15")
       "c13")
      ∧
      (r:appendo«378»
       x:res
       (x:a : empty)
       x:out«384»
       "r16")
      "c12")
     "f11")
    "d10"))))))


(apply-reduction-relation red-tree (car '(((((((sym "dog")
      :
      ((sym "cat")
       :
       ((sym "bear")
        :
        ((sym "lion")
         :
         empty))))
     =?
     empty
     "u18")
    ∧
    (0 =? empty "u19")
    "c17")
   (state () 1 () "s"))
  <-+
  ((∃
    (x:a x:d x:res)
    (((((sym "dog")
        :
        ((sym "cat")
         :
         ((sym "bear")
          :
          ((sym "lion")
           :
           empty))))
       =?
       (x:a : x:d)
       "u14")
      ∧
      (r:reverseo«379»
       x:d
       x:res
       "r15")
      "c13")
     ∧
     (r:appendo«378»
      x:res
      (x:a : empty)
      0
      "r16")
     "c12")
    "f11")
   (state () 1 () "g17019")))
 ((r:appendo«378»
   (x:l«380»
    x:s«381»
    x:out«382»)
   (((x:l«380» =? empty "u8")
     ∧
     (x:out«382»
      =?
      x:s«381»
      "u9")
     "c7")
    ∨
    (∃
     (x:a x:d x:res)
     (((x:l«380»
        =?
        (x:a : x:d)
        "u4")
       ∧
       (x:out«382»
        =?
        (x:a : x:res)
        "u5")
       "c3")
      ∧
      (r:appendo«378»
       x:d
       x:s«381»
       x:res
       "r6")
      "c2")
     "f1")
    "d0"))
  (r:reverseo«379»
   (x:ls«383» x:out«384»)
   (((x:ls«383»
      =?
      empty
      "u18")
     ∧
     (x:out«384»
      =?
      empty
      "u19")
     "c17")
    ∨
    (∃
     (x:a x:d x:res)
     (((x:ls«383»
        =?
        (x:a : x:d)
        "u14")
       ∧
       (r:reverseo«379»
        x:d
        x:res
        "r15")
       "c13")
      ∧
      (r:appendo«378»
       x:res
       (x:a : empty)
       x:out«384»
       "r16")
      "c12")
     "f11")
    "d10"))))))
