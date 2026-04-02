#lang racket
(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in l4: "../src/extensions/l4-railroad-syntax.rkt")
         (prefix-in j: "../src/wf-variants.rkt")
         "../src/transpiler.rkt")

(define-test-suite ASSOCIATIVITY
  (test-case "Conjunctions Left Associate"
    (define PROG '((run* (q) (== 1 1) (== 2 2) (== 3 3))))
    (define-values (cfg _) (parse-prog/canonical PROG))
    (match cfg
      [`(,_ ((∃ ,_ ,goal ,_) ,_))
       (check-true (redex-match? l4:L4 g (term ,goal)))
       (check-true (redex-match? l4:L4 g (term ((g_1 ∧ g_2 tag_1) ∧ g_3 tag_2))))]
      [_ (fail "unexpected canonical cfg shape")]))

  (test-case "Disjunctions Right Associate"
    (define PROG '((run* (q)
                    (conde
                      [(conde
                        [(same q 'turtle)]
                        [(same q 'cat)]
                        [(== q 'dog)])]
                      [(same q 'fish)]))))
    (define-values (cfg _) (parse-prog/canonical PROG))
    (match cfg
      [`(,_ ((∃ ,_ ,goal ,_) ,_))
       (check-true (redex-match? l4:L4 g (term ,goal)))
       (check-true (redex-match? l4:L4 g (term ((g_1 ∨ (g_2 ∨ g_3 tag_1) tag_2) ∨ g_4 tag_3))))]
      [_ (fail "unexpected canonical cfg shape")])

    (define PROG1 '((run* (q)
                      (conde
                        ((conde
                          ((same q 'turtle))
	                      ((conde
	                          ((same q 'cat))
	                          ((== q 'dog))))))
                            ((same q 'fish))))))
    (define-values (cfg1 _1) (parse-prog/canonical PROG1))
    (match cfg1
      [`(,_ ((∃ ,_ ,goal ,_) ,_))
       (check-true (redex-match? l4:L4 g (term ((g_1 ∨ (g_2 ∨ g_3 tag_1) tag_2) ∨ g_4 tag_3))))]
      [_ (fail "unexpected canonical cfg shape")])

    (define PROG2 '((run* (q)
                    (conde
                      [(same q 'turtle)]
                      [(same q 'cat)]
                      [(== q 'dog)]
                      [(same q 'fish)]))))
    (define-values (cfg2 _2) (parse-prog/canonical PROG2))
    (match cfg2
      [`(,_ ((∃ ,_ ,goal ,_) ,_))
       (check-true (redex-match? l4:L4 g (term (g_1 ∨ (g_2 ∨ (g_3 ∨ g_4 tag_1) tag_2) tag_3))))]
      [_ (fail "unexpected canonical cfg shape")])
    ))

(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()
        (cons expr (read-all port)))))

(define (parse-src/canonical src)
  (parse-prog/canonical (read-all (open-input-string src))))

(define-test-suite CANONICAL-TRANSLATION
  (test-case
   "run*-only canonical translation is L4/config and wf"
   (define-values (cfg html)
     (parse-src/canonical "(run* (q) (== 'a 'a))"))
   (check-true (redex-match? l4:L4 config cfg))
   (check-true (j:wf-config/target? "L4/config" cfg))
   (check-true (string? html)))

  (test-case
   "defrel+run* canonical translation is L4/config and wf"
   (define-values (cfg html)
     (parse-src/canonical
      "(defrel (same x y) (== x y))
(run* (q) (same q 'cat))"))
   (check-true (redex-match? l4:L4 config cfg))
   (check-true (j:wf-config/target? "L4/config" cfg))
   (check-true (string? html)))

  (test-case
   "relation-call arity mismatch parses but is rejected by wf"
   (define-values (cfg _html)
     (parse-src/canonical
      "(defrel (same x y) (== x y))
(run* (q) (same q))"))
   (check-true (redex-match? l4:L4 config cfg))
   (check-false (j:wf-config/target? "L4/config" cfg)))

  )

(define/provide-test-suite TRANSPILER
  #:after (thunk (displayln "Finished running tests for transpiler."))

  ASSOCIATIVITY
  CANONICAL-TRANSLATION)

#;(run-tests TRANSPILER)
