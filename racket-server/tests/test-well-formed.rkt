#lang racket
(require rackunit
         redex
         "../src/judgment-forms.rkt"
         "../src/definitions.rkt")


(define reverso-body '((∃
    (x:q)
    (r:reverseo
     ((sym "dog") : ((sym "cat") : ((sym "bear") : ((sym "lion") : empty))))
     x:q
     "r21")
    "f20")
   (state () 0 () "s")))
(define reverso-env
  '((r:appendo
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
     "d10"))))
(define reverso (term (,reverso-body ,reverso-env)))

(test-case "Reverso is well formed"
  (check-true (redex-match? L s reverso-body))
  (check-true (redex-match? L Γ reverso-env))
  (check-true (redex-match? L p reverso))
  (check-true (judgment-holds (closed-program? ,reverso)))
  )

(test-case "Relation Call Correct Arity Is Closed"
  (judgment-holds (closed-program? (((r:testo "a" "b" "dog") (state () 0 () "s")) ((r:testo (x:a x:b) (x:a =? x:b "u"))))))
  )
(test-case "Relation Call Inorrect Arity Is Not Closed"
  (judgment-holds (closed-program? (((r:testo "a" "dog") (state () 0 () "s")) ((r:testo (x:a x:b) (x:a =? x:b "u"))))))
  )
