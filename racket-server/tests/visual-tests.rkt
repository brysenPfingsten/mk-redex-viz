#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(test-match L g (term ⊤))
(test-match L g (term (x:x =? ("s" : x:y) (nat 5))))


;; Add
(stepper
 red
 (term (((∃ (x:q) (r:add ("s" : ("s" : "z")) x:q)) (state () 0))
        ((r:add (x:x x:y)
                ((∃ (x:x^ x:y^)
                    ((x:x =? ("s" : x:x^)) ∧ ((x:y =? ("s" : x:y^)) ∧ (r:add x:x^ x:y^))))
                 ∨
                 ((x:x =? "z") ∧ (x:y =? ("s" : "z")))))))))

;; Conjs and Disjs
(traces
 red
 (term
  ((((((⊤
		∧ ("abc" =? "abc"))
	   ∨ (("def" =? "def")
		  ∧ ("nine" =? "nine")))
	  ∧ ((("abc" =? "def")
		  ∨ ("abc" =? "abc"))
		 ∨ (("def" =? "def")
			∧ ("nine" =? "nine"))))
	 ∨ (((("abc" =? "def")
		  ∨ ("abc" =? "abc"))
		 ∨ (("def" =? "def")
			∧ ("nine" =? "nine")))
		∧ ((("abc" =? "def")
			∧ ("abc" =? "abc"))
		   ∨ (("def" =? "def")
			  ∧ ("nine" =? "nine")))))
	 (state ((3 "x")) 0))
    ())))


;; Misc
(traces
 red
 (term
  (((((⊤ (state ((3 "x")) 0))
      <-+
      (("def" =? "def")
       (state ((3 "x")) 0)))
     ×
     (("abc" =? "abc")
      ∨
      ("nine" =? "nine")))
    <-+
    (("ghi" =? "ghi")
     (state ((3 "x")) 0)))
   ())))

(traces red (term (((((⊤ ∨ ("abc" =? "def")) ∧ ((("abc" =? "def") ∨ ⊤) ∧ (("abc" =? "def") ∨ ⊤)))
					 ∨
					 ((("abc" =? "def") ∨ ⊤) ∨ ((⊤ ∧ ⊤) ∨ (⊤ ∨ ("abc" =? "def")))))
					(state () 0))
				   ())))

;; Appendo
(traces red (term (((∃ (x:q) (r:appendo ("cat" : ("dog" : empty)) ("bear" : ("lion" : empty)) x:q))
					(state () 0))
				   ((r:appendo (x:l x:s x:out)
					  (((x:l =? empty) ∧ (x:s =? x:out))
					   ∨
					   (∃ (x:a x:d x:res)
						  (((x:a : x:d) =? x:l)
						   ∧
						   (((x:a : x:res) =? x:out)
							∧
							(r:appendo x:d x:s x:res)))))))
  )))

#;((∃ x:l x:s (r:appendo x:l x:s ("dog" : ("cat" : ("bear" : empty))))) (state () 0))

;; Poso
(traces red (term (((∃ (x:q) (r:poso x:q)) (state () 0))
                   ((r:poso (x:n) (∃ (x:a x:d) (x:n =? (x:a : x:d))))))))

;; Same-lengtho
(traces red (term (((r:same-lengtho ("ghi" : ("jkl" : empty)) ("abc" : ("def" : empty))) (state ()  0))
                   ((r:same-lengtho (x:l1 x:l2)
					  (((x:l1 =? empty) ∧ (x:l2 =? empty))
					   ∨
					   (∃ (x:car1 x:cdr1 x:car2 x:cdr2)
						  ((x:l1 =? (x:car1 : x:cdr1))
						   ∧
						   ((x:l2 =? (x:car2 : x:cdr2))
							∧
							(r:same-lengtho x:cdr1 x:cdr2))))))))))

#;((∃ x:h1 x:h2 (r:same-lengtho x:h1 x:h2)) (state ()  0))
#;((∃ x:h1 (r:same-lengtho x:h1 ("dog" : ("cat" : empty)))) (state ()  0))


;; Cdro
(traces red (term (((r:cdro ("test" : empty) empty) (state () 0))
                   ((r:cdro (x:l x:d) (∃ (x:a) (x:l =? (x:a : x:d))))))))
