#lang racket

(require redex
         redex/reduction-semantics
         rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(module+ test
  ;; Language-shape sanity for current legacy syntax.
  (test-true "numbers are terms" (redex-match? L t (term 5)))
  (test-true "booleans are terms" (redex-match? L t (term #t)))
  (test-true "strings are terms" (redex-match? L t (term "cat")))

  (test-true
   "fresh over tagged equation is a goal"
   (redex-match? L g (term (∃ (x:x) (x:x =? (sym "abc") (sym "u1")) (sym "f0")))))

  (test-true
   "state uses current 4-field shape"
   (redex-match? L σ (term (state () 0 () (sym "s")))))

  (test-true
   "goal/state is a search tree"
   (redex-match? L s
                 (term (((sym "abc") =? (sym "abc") (sym "u"))
                        (state () 0 () (sym "s"))))))

  (test-true
   "program is (e Γ), not (prog Γ e)"
   (redex-match? L p
                 (term ((((sym "abc") =? (sym "abc") (sym "u"))
                         (state () 0 () (sym "s")))
                        ()))))

  ;; Judgment sanity.
  (test-true
   "simple equality program is closed"
   (judgment-holds
    (closed-program?
     ((((sym "abc") =? (sym "abc") (sym "u"))
       (state () 0 () (sym "s")))
      ()))))

  ;; Metafunction sanity.
  (test-equal (term (unify "abc" "abc" ((2 "fish"))))
              (term ((2 "fish"))))
  (test-equal (term (walk 0 ((1 "cat") (0 "dog"))))
              (term "dog"))
  (test-equal (term (walk 0 ((1 "cat") (0 1))))
              (term "cat"))

  ;; Keep unit-tests focused on language/judgment/metafunction sanity.
  ;; Reduction behavior coverage lives in dedicated reduction/property suites.

  (test-results))
