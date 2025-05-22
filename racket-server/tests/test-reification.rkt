#lang racket

(require "../definitions.rkt" "../reification.rkt")
(require redex rackunit)
(require rackunit/text-ui)

(define L1 (term ("t" : ("e" : ("s" : ("t" : empty))))))
(define L2 (term ("t" : ("e" : ("s" : "t")))))
(define L3 (term ("string" : (empty : ((sym "symbol") : ((nat 1) : (#t : empty)))))))
(define STRING "miniKanren")
(define TRUE #t)
(define FALSE #f)
(define MT (term empty))
(define SYMBOL (term (sym "miniKanren")))
(define NAT (term (nat 1)))
(define LOGIC-VAR 5)

(define-test-suite
  TERM->MK
  (test-case "All Example Redex Terms Are Terms"
             (check-true (redex-match? L t L1))
             (check-true (redex-match? L t L2))
             (check-true (redex-match? L t L3))
             (check-true (redex-match? L t STRING))
             (check-true (redex-match? L t TRUE))
             (check-true (redex-match? L t FALSE))
             (check-true (redex-match? L t MT))
             (check-true (redex-match? L t SYMBOL))
             (check-true (redex-match? L t NAT))
             (check-true (redex-match? L t LOGIC-VAR)))

  (test-case "Primitives Are Converted Correctly"
             (check-equal? (term (term->mk ,STRING)) "miniKanren")
             (check-equal? (term (term->mk ,TRUE)) #t)
             (check-equal? (term (term->mk ,FALSE)) #f)
             (check-equal? (term (term->mk ,MT)) ''())
             (check-equal? (term (term->mk ,SYMBOL)) ''miniKanren)
             (check-equal? (term (term->mk ,NAT)) 1)
             (check-equal? (term (term->mk ,LOGIC-VAR)) '_5))

  (test-case "List Are Converted Correctly"
             (check-equal? (term (term->mk ,L1))
                           '(cons "t" (cons "e" (cons "s" (cons "t" '())))))
             (check-equal? (term (term->mk ,L2))
                           '(cons "t" (cons "e" (cons "s" (cons "t" '())))))
             (check-equal? (term (term->mk ,L3))
                           '(cons "string" (cons '() (cons 'symbol (cons 1 (cons #t '()))))))))

(define REIFIED '_.0)
(define SYMBOL1 'miniKanren)
(define NATURAL1 1)
(define MT1 '())
(define STRING1 "miniKanren")
(define BOOL1 #t)
(define BOOL2 #f)
(define L4 (list REIFIED SYMBOL1 NATURAL1 MT1 STRING1 BOOL1 BOOL2))

(define-test-suite
  MK->JSON
  (test-case "Reified Variables Are Properly Detected"
             (check-true (reified? '_.0))
             (check-true (reified? '_.1351)))

  (test-case "Non-Reified Variables Are Properly Detected"
             (check-false (reified? "String"))
             (check-false (reified? '_!a))
             (check-false (reified? #true)))

  (test-case "Primitives Are Converted To JSON Correctly"
             (check-equal? (mk->json REIFIED) "_.0")
             (check-equal? (mk->json SYMBOL1) (hasheq 'sym "miniKanren"))
             (check-equal? (mk->json NATURAL1) (hasheq 'num 1))
             (check-equal? (mk->json MT1) '())
             (check-equal? (mk->json STRING1) "miniKanren")
             (check-equal? (mk->json BOOL1) #t)
             (check-equal? (mk->json BOOL2) #f))

  (test-equal? "Lists Are Converted to JSON Correctly"
               (mk->json L4)
               (list "_.0" #hasheq((sym . "miniKanren")) #hasheq((num . 1)) '() "miniKanren" #t #f)))

(define SUB1
  `((0 1)
    (1 2)
    (2 3)))

(define SUB2
  '((0 (sym "hello"))
    (1 "hello")
    (2 (nat 43110))
    (3 #t)
    (4 #f)))

(define SUB3
  '((0 (1 : (2 : (3 : empty))))
    (1 (nat 1))
    (2 (nat 2))
    (3 (nat 3))))

(define QUERY-VARS '(q r s t u v))

(define-test-suite
  REIFICATION-HELPERS
  (test-case "Fresh Variables Are Generated Correctly"
             (check-equal? (generate-fresh-names 5) '(_1 _2 _3 _4))
             (check-equal? (generate-fresh-names 1) '())
             (check-equal? (generate-fresh-names 0) '()))

  (test-true "Query Variables Are All Symbols"
             (andmap symbol? (generate-query-vars 10)))
  (test-case "Query Variables Are Generated Randomly"
             (check-not-equal? (generate-query-vars 10)
                               (generate-query-vars 10)))
  (test-case "The Proper Number Of Query Variables Are Generated"
             (check-equal? (length (generate-query-vars 10)) 10)
             (check-equal? (length (generate-query-vars 1)) 1)
             (check-equal? (length (generate-query-vars 0)) 0))

  (test-case "First n Logic Vars Become Corresponding Query Var"
             (check-equal? (make-unify-clause QUERY-VARS 6 (list 0 5))
                           '(== q v))
             (check-equal? (make-unify-clause QUERY-VARS 6 (list 1 4))
                           '(== r u)))
  (test-equal? "Logic Vars >= n Are Underscored"
               (make-unify-clause '() 0 (list 0 1))
               '(== _0 _1))
  (test-case "Primitives Are Turned Into Equations Correctly"
             (check-equal? (make-unify-clause '() 0 (list 0 '(sym "symbol")))
                           '(== _0 'symbol))
             (check-equal? (make-unify-clause '() 0 (list 1 '(nat 99)))
                           '(== _1 99))
             (check-equal? (make-unify-clause '() 0 (list 2 "string"))
                           '(== _2 "string"))
             (check-equal? (make-unify-clause '() 0 (list 3 #t))
                           '(== _3 #t))
             (check-equal? (make-unify-clause '() 0 (list 3 'empty))
                           '(== _3 '())))
  (test-equal? "List Are Turned Into Equations Correctly"
               (make-unify-clause '(q) 1 (list 0 L3))
               '(== q (cons "string" (cons '() (cons 'symbol (cons 1 (cons #t '())))))))

  (test-case "Generated Namespace Contains run*, fresh, and =="
             (define mapped-syms (namespace-mapped-symbols (prepare-minikanren-namespace)))
             (check-not-false (member 'run* mapped-syms))
             (check-not-false (member 'fresh mapped-syms))
             (check-not-false (member '== mapped-syms)))

  (test-equal? "MK Lists Are Converted To JSON Properly"
               (process-reify-result (list "string" 'symbol 1 #t '()))
                             (list "string" (hasheq 'sym "symbol") (hasheq 'num 1) #t '()))
  (test-case "MK Terms Are Converted To JSON Properly"
             (check-equal? (process-reify-result '_.0) "_.0")
             (check-equal? (process-reify-result 'mk) (hasheq 'sym "mk"))))

(define/provide-test-suite
  REIFICATION
  #:before (thunk (display "Running Tests For Reification...\n"))
  #:after (thunk (display "Finished Tests For Reification.\n"))

  TERM->MK
  MK->JSON
  REIFICATION-HELPERS

  (test-equal? "One Query Var, All Fresh Bindings"
               (reify SUB1 4 1)
               "_.0")

  (test-case "Multiple Query Vars, All Fresh Bindings"
             (check-equal? (reify SUB1 4 4)
                           '("_.0" "_.0" "_.0" "_.0"))
             (check-equal? (reify SUB1 4 3)
                           '("_.0" "_.0" "_.0"))
             (check-equal? (reify SUB1 4 2)
                           '("_.0" "_.0")))

  (test-case "Primitives Are Reified Correctly"
             (check-true (redex-match? L sub SUB2))
             (check-equal? (reify SUB2 5 5)
                           (list #hasheq((sym . "hello")) "hello" #hasheq((num . 43110)) #t #f)))

  (test-case "Basic List Reification"
             (check-true (redex-match? L sub SUB3))
             (check-equal? (reify SUB3 4 1)
                           (list #hasheq((num . 1)) #hasheq((num . 2)) #hasheq((num . 3)))))
  )