#lang racket

(require "./shared/kernel.rkt")

(provide example-labels
         example-program
         parse-source-program
         instantiate-program)

(define programs
  (list
   (list "simple unify"
         '(program (x:q)
                   (x:q =? (sym "cat") (label "eq"))))
   (list "disequality success"
         '(program (x:q)
                   ((x:q != (sym "cat") (label "neq"))
                    ∧
                    (x:q =? (sym "dog") (label "eq"))
                    (label "and"))))
   (list "nested fresh"
         '(program (x:q)
                   (∃ (x:x)
                      (∃ (x:y)
                         (((x:x =? (sym "cat") (label "eq-x"))
                           ∧
                           (x:y =? (sym "dog") (label "eq-y"))
                           (label "and-xy"))
                          ∧
                          (x:q =? (x:x : x:y) (label "eq-q"))
                          (label "and-q"))
                         (label "fy"))
                      (label "fx"))))
   (list "pure disjunction"
         '(program (x:q)
                   ((x:q =? (sym "left") (label "left"))
                    ∨
                    (x:q =? (sym "right") (label "right"))
                    (label "split"))))
   (list "scoped conjunction carry"
         '(program (x:q)
                   (∃ (x:x)
                      (((x:x =? (sym "cat") (label "eq-x"))
                        ∧
                        (x:q =? x:x (label "eq-q"))
                        (label "and"))
                       ∨
                       (x:q =? (sym "dog") (label "dog"))
                       (label "split"))
                      (label "fx"))))
   (list "delay interleaving"
         '(program (x:q)
                   ((suspend (x:q =? (sym "later") (label "later"))
                             (label "delay"))
                    ∨
                    (x:q =? (sym "now") (label "now"))
                    (label "split"))))
   (list "fresh delay interaction"
         '(program (x:q)
                   (∃ (x:x)
                      (((suspend (x:x =? (sym "nap") (label "nap"))
                                 (label "delay"))
                        ∧
                        (x:q =? x:x (label "eq-q"))
                        (label "and"))
                       ∨
                       (x:q =? (sym "awake") (label "awake"))
                       (label "split"))
                      (label "fx"))))))

(define example-labels
  (map first programs))

(define (example-program label)
  (for/first ([entry (in-list programs)]
              #:do [(match-define (list entry-label program) entry)]
              #:when (equal? entry-label label))
    program))

(define (parse-source-program raw)
  (match raw
    [`(program ,query-x* ,goal)
     (values query-x* goal)]
    [_ (error 'parse-source-program
              "unsupported source program: ~e"
              raw)]))

(define (instantiate-program label)
  (define raw
    (or (example-program label)
        (error 'instantiate-program
               "unknown example label: ~a"
               label)))
  (define-values (query-x* goal)
    (parse-source-program raw))
  (define query-u*
    (fresh-u-list '() query-x*))
  (define query-sub
    (map list query-x* query-u*))
  (values query-u*
          (subst-goal goal query-sub)))
