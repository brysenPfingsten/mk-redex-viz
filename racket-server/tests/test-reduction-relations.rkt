#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit rackunit/text-ui)
(check-redundancy #t)

(require "../src/reduction-relations/reduction-relations.rkt")

(define (make-state [c 0] [o "s"])
  `(state () () ,c () ,o))

;; Check that applying rel on t gives expected rule name and result
(define-syntax check-rule
  (syntax-rules ()
    [(_ t expected-name expected-result rel)
     (let ([results (apply-reduction-relation/tag-with-names rel t)])
       (check-false (null? results) (format "Expected reduction for ~a" t))
       (check-equal? (caar results) expected-name)
       (check-equal? (cadar results) expected-result))]))


(define/provide-test-suite
  RED
  #:before (λ () (displayln "Running reduction relation tests ..."))
  #:after (λ () (displayln "Finished running reduction relation tests ..."))
  (test-case "Substitute Relation Body And Proceed"
    ;; Relation/param names get alpha-renamed, so check structure
    (let* ([state (make-state 0 "s1")]
           [prog `((proceed ((r:foo 0 "call") ,state))
                   ((r:foo (x:a) (x:a =? 0 "body"))))]
           [results (apply-reduction-relation/tag-with-names red prog)])
      (check-false (null? results))
      (check-equal? (caar results) "Substitute Relation Body And Proceed")
      ;; Check that the body was substituted (0 =? 0 "body") with state
      (let ([result (cadar results)])
        (check-equal? (caar result) '(0 =? 0 "body"))
        (check-equal? (cadar result) state))))

  (test-case "Invoke Delay"
    (let* ([state (make-state 0 "s1")]
           [inner `(⊤ ,state)]
           [prog `((delay ,inner) ())]
           [expected `(,inner ())])
      (check-rule prog "Invoke Delay" expected red)))

  (test-case "Distribute State Over Disjunction"
    ;; Result has gensym so we check structure manually
    ;; ((g_1 ∨ g_2 _) σ) => ((g_1 σ) <-+ (g_2 σ'))
    (let* ([state (make-state 1 "s1")]
           [results (apply-reduction-relation/tag-with-names
                     red-tree `(((0 =? 0 "g1") ∨ (1 =? 1 "g2") "disj") ,state))])
      (check-false (null? results))
      (check-equal? (caar results) "Distribute State Over Disjunction")
      (let ([result (cadar results)])
        ;; result is ((g_1 σ) <-+ (g_2 σ'))
        (check-equal? (car result) `((0 =? 0 "g1") ,state))
        (check-equal? (cadr result) '<-+)
        ;; Third element is (g_2 σ') where σ' has gensym origin
        (check-equal? (car (caddr result)) '(1 =? 1 "g2")))))

  (test-case "Distribute State Over Conjunction"
    ;; ((g_1 ∧ g_2 _) σ) => ((g_1 σ) × g_2)
    (let* ([state (make-state 1 "s1")]
           [expected `(((0 =? 0 "g1") ,state) × (1 =? 1 "g2"))])
      (check-rule
        `(((0 =? 0 "g1") ∧ (1 =? 1 "g2") "conj") ,state)
        "Distribute State Over Conjunction" expected red-tree)))

  (test-case "Distribute Left Disjunction Answer Over Conjunction"
    ;; (((⊤ σ) <-+ s) × g) => (((⊤ σ) × g) <-+ (s × g))
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [tree `((0 =? 0 "g") ,state)]
           [expected `((,answer × (1 =? 1 "g2")) <-+ (,tree × (1 =? 1 "g2")))])
      (check-rule
        `((,answer <-+ ,tree) × (1 =? 1 "g2"))
        "Distribute Left Disjunction Answer Over Conjunction" expected red-tree)))

  (test-case "Distribute Right Disjunction Answer Over Conjunction"
    ;; ((s +-> (⊤ σ)) × g) => ((s × g) +-> ((⊤ σ) × g))
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [tree `((0 =? 0 "g") ,state)]
           [expected `((,tree × (1 =? 1 "g2")) +-> (,answer × (1 =? 1 "g2")))])
      (check-rule
        `((,tree +-> ,answer) × (1 =? 1 "g2"))
        "Distribute Right Disjunction Answer Over Conjunction" expected red-tree)))

  (test-case "Reassociate Right-Left Disjunction"
    ;; s_2 +-> ((⊤ σ) <-+ s) => (⊤ σ) <-+ (s_2 +-> s)
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [s `((0 =? 0 "g") ,state)]
           [s2 `((1 =? 1 "g2") ,state)]
           [expected `(,answer <-+ (,s2 +-> ,s))])
      (check-rule
        `(,s2 +-> (,answer <-+ ,s))
        "Reassociate Right-Left Disjunction" expected red-tree)))

  (test-case "Reassociate Right-Right Disjunction"
    ;; s_2 +-> (s +-> (⊤ σ)) => (s_2 +-> s) +-> (⊤ σ)
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [s `((0 =? 0 "g") ,state)]
           [s2 `((1 =? 1 "g2") ,state)]
           [expected `((,s2 +-> ,s) +-> ,answer)])
      (check-rule
        `(,s2 +-> (,s +-> ,answer))
        "Reassociate Right-Right Disjunction" expected red-tree)))

  (test-case "Reassociate Left-Left Disjunction"
    ;; ((⊤ σ) <-+ s) <-+ s_2 => (⊤ σ) <-+ (s <-+ s_2)
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [s `((0 =? 0 "g") ,state)]
           [s2 `((1 =? 1 "g2") ,state)]
           [expected `(,answer <-+ (,s <-+ ,s2))])
      (check-rule
        `((,answer <-+ ,s) <-+ ,s2)
        "Reassociate Left-Left Disjunction" expected red-tree)))

  (test-case "Reassociate Left-Right Disjunction"
    ;; (s +-> (⊤ σ)) <-+ s_2 => (s <-+ s_2) +-> (⊤ σ)
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [s `((0 =? 0 "g") ,state)]
           [s2 `((1 =? 1 "g2") ,state)]
           [expected `((,s <-+ ,s2) +-> ,answer)])
      (check-rule
        `((,s +-> ,answer) <-+ ,s2)
        "Reassociate Left-Right Disjunction" expected red-tree)))

  (test-case "Bring Success State To Second Conjunct"
    ;; (⊤ σ) × g => (g σ)
    (let* ([state (make-state 0 "s1")]
           [expected `((0 =? 0 "g") ,state)])
      (check-rule
        `((⊤ ,state) × (0 =? 0 "g"))
        "Bring Success State To Second Conjunct" expected red-tree)))

  (test-case "Prune Failed Conjuncts"
    ;; () × g => ()
    (check-rule
      '(() × (0 =? 0 "g"))
      "Prune Failed Conjuncts" '() red-tree))

  (test-case "Prune Left Disjunction Failure"
    ;; () <-+ s => s
    (let* ([state (make-state 0 "s1")]
           [s `((0 =? 0 "g") ,state)])
      (check-rule
        `(() <-+ ,s)
        "Prune Left Disjunction Failure" s red-tree)))

  (test-case "Prune Right Disjunction Failure"
    ;; s +-> () => s
    (let* ([state (make-state 0 "s1")]
           [s `((0 =? 0 "g") ,state)])
      (check-rule
        `(,s +-> ())
        "Prune Right Disjunction Failure" s red-tree)))

  (test-case "Promote Left Answer"
    ;; (⊤ σ) <-+ s => (⊤ σ) + s (in evaluation context)
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [s `((0 =? 0 "g") ,state)]
           [expected `(,answer + ,s)])
      (check-rule
        `(,answer <-+ ,s)
        "Promote Left Answer" expected red-tree)))

  (test-case "Promote Right Answer"
    ;; s +-> (⊤ σ) => (⊤ σ) + s (in evaluation context)
    (let* ([state (make-state 0 "s1")]
           [answer `(⊤ ,state)]
           [s `((0 =? 0 "g") ,state)]
           [expected `(,answer + ,s)])
      (check-rule
        `(,s +-> ,answer)
        "Promote Right Answer" expected red-tree)))

  (test-case "Substitute Fresh Variables"
    ;; (∃ (x ...) g _) with state => substitute fresh var and bump counter
    (let* ([state (make-state 0 "s1")]
           ;; x:a gets replaced with logic var 0, counter bumps to 1
           [expected `((0 =? 0 "inner") (state () () 1 () "s1"))])
      (check-rule
        `((∃ (x:a) (x:a =? 0 "inner") "fresh") ,state)
        "Substitute Fresh Variables" expected red-tree)))

  (test-case "Relation Call And Add Delay"
    ;; (r_1 t ... o) σ => (delay (proceed ((r_1 t ... o) σ)))
    (let* ([state (make-state 0 "s1")]
           [expected `(delay (proceed ((r:foo 0 "call") ,state)))])
      (check-rule
        `((r:foo 0 "call") ,state)
        "Relation Call And Add Delay" expected red-tree)))

  (test-case "Unification Fails"
    ;; Unifying incompatible ground terms fails => ()
    (let ([state '(state () () 1 () "s1")])
      (check-rule
        `(((sym "a") =? (sym "b") "unify") ,state)
        "Unification Fails" '() red-tree)))

  (test-case "Unification Succeeds Disequalities Not Violated"
    ;; Unifying compatible terms succeeds => (⊤ state-with-trail)
    (let* ([state '(state () () 1 () "s1")]
           ;; Trail gets updated with the unification
           [expected '(⊤ (state () () 1 (((sym "a") =? (sym "a") "unify")) "s1"))])
      (check-rule
        `(((sym "a") =? (sym "a") "unify") ,state)
        "Unification Succeeds Disequalities Not Violated" expected red-tree)))

  (test-case "Unification Succeeds But Disequality Violated"
    ;; Unification succeeds but violates disequality => ()
    (let ([state '(state () (((sym "a") (sym "a"))) 1 () "s1")])
      (check-rule
        `(((sym "a") =? (sym "a") "unify") ,state)
        "Unification Succeeds But Disequality Violated" '() red-tree)))

  (test-case "Disequality Not Violated"
    ;; Adding disequality that is not violated => (⊤ state-with-diseq)
    (let* ([state '(state () () 1 () "s1")]
           [expected '(⊤ (state () (((sym "a") (sym "b"))) 1 () "s1"))])
      (check-rule
        `(((sym "a") != (sym "b") "diseq") ,state)
        "Disequality Not Violated" expected red-tree)))

  (test-case "Disequality Violated"
    ;; Adding disequality that is immediately violated => ()
    (let ([state '(state () () 1 () "s1")])
      (check-rule
        `(((sym "a") != (sym "a") "diseq") ,state)
        "Disequality Violated" '() red-tree)))

  (test-case "Propagate Delay Through Conjunction"
    ;; (delay s) × g => delay (s × g)
    (let* ([state (make-state 0 "s1")]
           [inner `((0 =? 0 "g") ,state)]
           [expected `(delay (,inner × (1 =? 1 "g2")))])
      (check-rule
        `((delay ,inner) × (1 =? 1 "g2"))
        "Propagate Delay Through Conjunction" expected red-tree)))

  (test-case "Propagate Delay Through Left Disjunction And Flip"
    ;; (delay s_1) <-+ s_2 => delay (s_1 +-> s_2)
    (let* ([state (make-state 0 "s1")]
           [s1 `((0 =? 0 "g") ,state)]
           [s2 `((1 =? 1 "g2") ,state)]
           [expected `(delay (,s1 +-> ,s2))])
      (check-rule
        `((delay ,s1) <-+ ,s2)
        "Propagate Delay Through Left Disjunction And Flip" expected red-tree)))

  (test-case "Propagate Delay Through Right Disjunction And Flip"
    ;; s_2 +-> (delay s_1) => delay (s_2 <-+ s_1)
    (let* ([state (make-state 0 "s1")]
           [s1 `((0 =? 0 "g") ,state)]
           [s2 `((1 =? 1 "g2") ,state)]
           [expected `(delay (,s2 <-+ ,s1))])
      (check-rule
        `(,s2 +-> (delay ,s1))
        "Propagate Delay Through Right Disjunction And Flip" expected red-tree))))

;; (run-tests RED)
