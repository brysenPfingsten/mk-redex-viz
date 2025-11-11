#lang racket
(require rackunit
         rackunit/text-ui
         redex
         "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(define/provide-test-suite
 DISEQUALITY
 #:after (λ () (displayln "Finished running tests for disequality."))
 (test-case "A program with a contradicting unification and disequality."
   (define prog
     (term (((∃ (x:q) ((x:q =? (sym "bear") "u2") ∧ (x:q != (sym "bear") "u3") "c1") "f0")
             (state () () 0 () "s"))
            ())))
   (check-true (term (closed-program? ,prog)))
  (test-equal? "Program Steps to Failure" 
               (apply-reduction-relation* red prog) 
               '((() ())))))

(test-case "A program with a contradicting disequality and unification"
  (define prog2
    '(((∃ (x:q) ((x:q != (sym "bear") "u2") ∧ (x:q =? (sym "bear") "u3") "c1") "f0")
       (state () () 0 () "s"))
      ()))

  (check-true (term (closed-program? ,prog2)))
  (test-equal? "Program Steps to Failure" 
               (apply-reduction-relation* red prog2) 
               '((() ()))))

;; (run-tests DISEQUALITY)

(define MEMBERO '(((∃
    (x:q)
    (r:membero
     ((nat 1) : ((nat 2) : empty))
     (nat 2)
     x:q
     "r11")
    "f10")
   (state () () 0 () "s"))
  ((r:membero
    (x:l x:x x:out)
    (∃
     (x:a x:d)
     (((x:a : x:d) =? x:l "u2")
      ∧
      (((x:a =? x:x "u8") ∧ (x:l =? x:out "u9") "c7")
       ∨
       ((x:a != x:x "u5") ∧ (r:membero x:d x:x x:out "r6") "c4")
       "d3")
      "c1")
     "f0")))))

(define TEMP '(((1 =? (nat 2) "u8")
    (state
     ((2 ((nat 2) : empty))
      (1 (nat 1)))
     ()
     3
     (((1 : 2)
       =?
       ((nat 1)
        :
        ((nat 2) : empty))
       "u2"))
     "s"))
   ×
   (((nat 1)
     :
     ((nat 2) : empty))
    =?
    0
    "u9")))
(redex-match L (in-hole Ex (g σ)) TEMP)

;; (stepper red MEMBERO)
;; (define TEMP '(((1 =? (nat 2) "u8")
;;     (state
;;      ((2 ((nat 2) : empty))
;;       (1 (nat 1)))
;;      ()
;;      3
;;      (((1 : 2)
;;        =?
;;        ((nat 1)
;;         :
;;         ((nat 2) : empty))
;;        "u2"))
;;      "s"))
;;    ×
;;    (((nat 1)
;;      :
;;      ((nat 2) : empty))
;;     =?
;;     0
;;     "u9")))
