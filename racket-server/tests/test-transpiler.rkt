#lang racket
(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in canonical:
                    "../src/search-lattice/languages/canonical-lang.rkt")
         (prefix-in wf:
                    "../src/search-lattice/wf/all.rkt")
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt")

(define (parse-src/canonical src
                             #:source-mode [source-mode default-source-mode]
                             #:compile-profile [compile-profile #f])
  (parse-prog/canonical (read-all-sexprs (open-input-string src))
                        #:source-mode source-mode
                        #:compile-profile compile-profile))

(define (parse-src/ast src
                       #:source-mode [source-mode default-source-mode]
                       #:compile-profile [compile-profile #f])
  (parse-prog->ast (read-all-sexprs (open-input-string src))
                   #:source-mode source-mode
                   #:compile-profile compile-profile))

(define (query-goal-of cfg)
  (match cfg
    [`(,_ ((∃ ,_ ,goal ,_) ,_) ,_) goal]
    [_ (error 'query-goal-of "unexpected canonical cfg shape: ~e" cfg)]))

(define (relation-goal-of cfg rel-name)
  (match cfg
    [`(,rels ,_ ,_)
     (define maybe-goal
       (for/first ([rel (in-list rels)]
                   #:when (match rel
                            [`(,r ,_ ,_)
                             (equal? r rel-name)]
                            [_ #f]))
         (match rel
           [`(,_ ,_ ,goal) goal]
           [_ #f])))
     (or maybe-goal
         (error 'relation-goal-of
                "relation ~e not present in canonical cfg ~e"
                rel-name
                cfg))]
    [_ (error 'relation-goal-of "unexpected canonical cfg shape: ~e" cfg)]))

(define (goal-top-delay? goal)
  (match goal
    [`(suspend ,_ ,_) #t]
    [_ #f]))

(define (goal-contains-delay? goal [seen #f])
  (match goal
    [`(suspend ,g ,_)
     (goal-contains-delay? g #t)]
    [`(∃ ,_ ,g ,_)
     (goal-contains-delay? g seen)]
    [`(,g1 ∧ ,g2 ,_)
     (or (goal-contains-delay? g1 seen)
         (goal-contains-delay? g2 seen))]
    [`(,g1 ∨ ,g2 ,_)
     (or (goal-contains-delay? g1 seen)
         (goal-contains-delay? g2 seen))]
    [_ seen]))

(define (strip-suspends goal)
  (match goal
    [`(suspend ,g ,_) (strip-suspends g)]
    [`(∃ ,vars ,g ,tag) `(∃ ,vars ,(strip-suspends g) ,tag)]
    [`(,g1 ∧ ,g2 ,tag) `(,(strip-suspends g1) ∧ ,(strip-suspends g2) ,tag)]
    [`(,g1 ∨ ,g2 ,tag) `(,(strip-suspends g1) ∨ ,(strip-suspends g2) ,tag)]
    [_ goal]))

(define (goal-contains-delayed-relcall? goal)
  (match goal
    [`(suspend (,r ,_ ... ,_) ,_)
     (and (symbol? r)
          (regexp-match? #rx"^r:" (symbol->string r)))]
    [`(suspend ,g ,_)
     (goal-contains-delayed-relcall? g)]
    [`(∃ ,_ ,g ,_)
     (goal-contains-delayed-relcall? g)]
    [`(,g1 ∧ ,g2 ,_)
     (or (goal-contains-delayed-relcall? g1)
         (goal-contains-delayed-relcall? g2))]
    [`(,g1 ∨ ,g2 ,_)
     (or (goal-contains-delayed-relcall? g1)
         (goal-contains-delayed-relcall? g2))]
    [_ #f]))

(define (strip-labels x)
  (match x
    [`(label ,_) '(label "_")]
    [(cons a d) (cons (strip-labels a) (strip-labels d))]
    [_ x]))

(define conj-source
  "(run* (q) (== 1 1) (== 2 2) (== 3 3))")

(define disj-source
  "(run* (q)
     (conde
       [(== q 'a)]
       [(== q 'b)]
       [(== q 'c)]))")

(define relcall-source
  "(defrel (same x y)
     (== x y))

   (defrel (wrap x)
     (== x x)
     (same x 'cat))

   (run* (q)
     (wrap q))")

(define micro-source
  "(defrel (same x y)
     (Zzz (conj (== x y) (=/= x 'dog))))

   (run* (q)
     (disj (same q 'cat)
           (Zzz (== q 'dog))))")

(define (profile-jsexpr conj-assoc disj-assoc delay-placement)
  (hasheq 'conjAssoc conj-assoc
          'disjAssoc disj-assoc
          'delayPlacement delay-placement))

(define-test-suite SOURCE-MODES
  (test-case "normalize-source-mode defaults missing or blank inputs"
    (check-equal? (normalize-source-mode #f) default-source-mode)
    (check-equal? (normalize-source-mode "") default-source-mode))

  (test-case "normalize-source-mode preserves supported modes"
    (check-equal? (normalize-source-mode "mini") "mini")
    (check-equal? (normalize-source-mode "micro") "micro"))

  (test-case "normalize-source-mode rejects unsupported values"
    (check-exn
     (lambda (e)
       (and (exn:fail? e)
            (regexp-match? #rx"unsupported sourceMode" (exn-message e))))
     (thunk (normalize-source-mode "macro")))))

(define-test-suite ASSOCIATIVITY
  (test-case "Conjunctions Left Associate"
    (define PROG '((run* (q) (== 1 1) (== 2 2) (== 3 3))))
    (define-values (cfg _) (parse-prog/canonical PROG))
    (define goal (query-goal-of cfg))
    (check-true (redex-match? canonical:canonical-lang g (term ,goal)))
    (check-true
     (match goal
       [`((,_ ∧ ,_ ,_) ∧ ,_ ,_) #t]
       [_ #f])))

  (test-case "Disjunctions Right Associate"
    (define PROG '((run* (q)
                    (conde
                      [(conde
                        [(same q 'turtle)]
                        [(same q 'cat)]
                        [(== q 'dog)])]
                      [(same q 'fish)]))))
    (define-values (cfg _) (parse-prog/canonical PROG))
    (define goal (query-goal-of cfg))
    (check-true (redex-match? canonical:canonical-lang g (term ,goal)))
    (check-true
     (match goal
       [`((,_ ∨ (,_ ∨ ,_ ,_) ,_) ∨ ,_ ,_) #t]
       [_ #f]))

    (define PROG1 '((run* (q)
                      (conde
                        ((conde
                          ((same q 'turtle))
	                      ((conde
	                          ((same q 'cat))
	                          ((== q 'dog))))))
                            ((same q 'fish))))))
    (define-values (cfg1 _1) (parse-prog/canonical PROG1))
    (define goal1 (query-goal-of cfg1))
    (check-true
     (match goal1
       [`((,_ ∨ (,_ ∨ ,_ ,_) ,_) ∨ ,_ ,_) #t]
       [_ #f]))

    (define PROG2 '((run* (q)
                    (conde
                      [(same q 'turtle)]
                      [(same q 'cat)]
                      [(== q 'dog)]
                      [(same q 'fish)]))))
    (define-values (cfg2 _2) (parse-prog/canonical PROG2))
    (define goal2 (query-goal-of cfg2))
    (check-true
     (match goal2
       [`(,_ ∨ (,_ ∨ (,_ ∨ ,_ ,_) ,_) ,_) #t]
       [_ #f]))
    ))

(define-test-suite COMPILE-PROFILES
  (test-case "all 12 compile profiles preserve selected conjunction/disjunction shape"
    (for* ([conj-assoc (in-list '("left" "right"))]
           [disj-assoc (in-list '("left" "right"))]
           [delay-placement (in-list '("relbody" "relcall" "disj"))])
      (define profile
        (profile-jsexpr conj-assoc disj-assoc delay-placement))

      (define-values (conj-cfg _conj-html)
        (parse-src/canonical conj-source #:compile-profile profile))
      (define conj-goal (query-goal-of conj-cfg))
      (cond
        [(equal? conj-assoc "left")
         (check-true
          (match conj-goal
            [`((,_ ∧ ,_ ,_) ∧ ,_ ,_) #t]
            [_ #f])
          (format "expected left-associated conjunction for profile ~e, got ~e"
                  profile
                  conj-goal))]
        [else
         (check-true
          (match conj-goal
            [`(,_ ∧ (,_ ∧ ,_ ,_) ,_) #t]
            [_ #f])
          (format "expected right-associated conjunction for profile ~e, got ~e"
                  profile
                  conj-goal))])

      (define-values (disj-cfg _disj-html)
        (parse-src/canonical disj-source #:compile-profile profile))
      (define disj-goal (query-goal-of disj-cfg))
      (define disj-inner
        (match disj-goal
          [`(suspend ,inner ,_) inner]
          [_ disj-goal]))
      (define stripped-disj (strip-suspends disj-inner))
      (check-equal? (goal-top-delay? disj-goal)
                    (equal? delay-placement "disj")
                    (format "disjunction delay placement mismatch for profile ~e: ~e"
                            profile
                            disj-goal))
      (cond
        [(equal? disj-assoc "left")
         (check-true
          (match stripped-disj
            [`((,_ ∨ ,_ ,_) ∨ ,_ ,_) #t]
            [_ #f])
          (format "expected left-associated disjunction for profile ~e, got ~e"
                  profile
                  disj-inner))]
        [else
         (check-true
          (match stripped-disj
            [`(,_ ∨ (,_ ∨ ,_ ,_) ,_) #t]
            [_ #f])
          (format "expected right-associated disjunction for profile ~e, got ~e"
                  profile
                  disj-inner))])))

  (test-case "delay placement distinguishes query relcalls from relation bodies"
    (for* ([conj-assoc (in-list '("left" "right"))]
           [disj-assoc (in-list '("left" "right"))]
           [delay-placement (in-list '("relbody" "relcall" "disj"))])
      (define profile
        (profile-jsexpr conj-assoc disj-assoc delay-placement))
      (define-values (cfg _html)
        (parse-src/canonical relcall-source #:compile-profile profile))
      (define query-goal (query-goal-of cfg))
      (define wrap-goal (relation-goal-of cfg 'r:wrap))
      (check-equal? (goal-top-delay? query-goal)
                    (equal? delay-placement "relcall")
                    (format "query relcall delay mismatch for profile ~e: ~e"
                            profile
                            query-goal))
      (case (string->symbol delay-placement)
        [(relbody)
         (check-true (goal-top-delay? wrap-goal)
                     (format "wrap body should be whole-body delayed for profile ~e: ~e"
                             profile
                             wrap-goal))]
        [(relcall)
         (check-false (goal-top-delay? wrap-goal)
                      (format "wrap body should not be whole-body delayed for profile ~e: ~e"
                              profile
                              wrap-goal))
         (check-true (goal-contains-delayed-relcall? wrap-goal)
                     (format "wrap body should contain a delayed relcall for profile ~e: ~e"
                             profile
                             wrap-goal))]
        [(disj)
         (check-false (goal-contains-delay? wrap-goal)
                      (format "wrap body should not contain compiler delay for profile ~e: ~e"
                              profile
                              wrap-goal))]))))

  (test-case "normalize-compile-profile rejects unsupported axis values"
    (check-exn
     (lambda (e)
       (and (exn:fail? e)
            (regexp-match? #rx"invalid compileProfile\\.conjAssoc"
                           (exn-message e))))
     (thunk
      (normalize-compile-profile
       (hasheq 'conjAssoc "middle"
               'disjAssoc "right"
               'delayPlacement "relbody")))))

(define-test-suite MICRO-SOURCE
  (test-case "direct micro source accepts binary conj/disj, Zzz, and disequality"
    (define-values (cfg html)
      (parse-src/canonical micro-source #:source-mode "micro"))
    (check-true (redex-match? canonical:canonical-lang config cfg))
    (check-true (wf:wf-config/target? "canonical/config" cfg))
    (check-true (string? html)))

  (test-case "direct micro source rejects source-level delay spelling"
    (check-exn
     exn:fail?
     (lambda ()
       (parse-src/canonical
        "(run* (q) (delay (== q 'cat)))"
        #:source-mode "micro"))))

  (test-case "direct micro source rejects conde"
    (check-exn
     exn:fail?
     (lambda ()
       (parse-src/canonical
        "(run* (q) (conde [(== q 'cat)] [(== q 'dog)]))"
        #:source-mode "micro"))))

  (test-case "direct micro source rejects proceed"
    (check-exn
     exn:fail?
     (lambda ()
       (parse-src/canonical
        "(run* (q) (proceed (== q 'cat)))"
        #:source-mode "micro"))))

  (test-case "direct micro source rejects multi-goal defrel bodies"
    (check-exn
     exn:fail?
     (lambda ()
       (parse-src/canonical
        "(defrel (same x y)
           (== x y)
           (== y x))
         (run* (q) (same q 'cat))"
        #:source-mode "micro"))))

  (test-case "direct micro source rejects multi-goal run tails"
    (check-exn
     exn:fail?
     (lambda ()
       (parse-src/canonical
        "(run* (q) (== q 'cat) (== q 'dog))"
        #:source-mode "micro")))))

(define-test-suite MICRO-RENDERING
  (test-case "rendered micro source round-trips mini lowering for all compile profiles"
    (for* ([conj-assoc (in-list '("left" "right"))]
           [disj-assoc (in-list '("left" "right"))]
           [delay-placement (in-list '("relbody" "relcall" "disj"))])
      (define profile
        (profile-jsexpr conj-assoc disj-assoc delay-placement))
      (define rendered
        (render-micro-source (read-all-sexprs (open-input-string relcall-source))
                             #:compile-profile profile))
      (check-equal? (not (false? (regexp-match? #rx"Zzz" rendered)))
                    (not (equal? delay-placement "disj"))
                    (format "rendered micro delay visibility mismatch for ~e" profile))
      (define-values (expected-cfg _expected-html)
        (parse-src/canonical relcall-source #:compile-profile profile))
      (define-values (rendered-cfg _rendered-html)
        (parse-src/canonical rendered #:source-mode "micro"))
      (check-equal? (strip-labels expected-cfg)
                    (strip-labels rendered-cfg)
                    (format "rendered micro should round-trip canonical cfg modulo labels for ~e"
                            profile)))))

(define-test-suite DISEQUALITY-TRANSLATION
  (test-case "mini source translates disequality to canonical != goal"
    (define-values (cfg _html)
      (parse-src/canonical "(run* (q) (=/= q 'cat))"))
    (define goal (query-goal-of cfg))
    (check-true (redex-match? canonical:canonical-lang g (term ,goal)))
    (check-true
     (match goal
       [`(,_ != ,_ ,_) #t]
       [_ #f])))

  (test-case "micro source translates disequality to canonical != goal"
    (define-values (cfg _html)
      (parse-src/canonical "(run* (q) (=/= q 'cat))" #:source-mode "micro"))
    (define goal (query-goal-of cfg))
    (check-true (redex-match? canonical:canonical-lang g (term ,goal)))
    (check-true
     (match goal
       [`(,_ != ,_ ,_) #t]
       [_ #f]))))

(define-test-suite CANONICAL-TRANSLATION
  (test-case
   "run*-only canonical translation is canonical/config and wf"
   (define-values (cfg html)
     (parse-src/canonical "(run* (q) (== 'a 'a))"))
   (check-true (redex-match? canonical:canonical-lang config cfg))
   (check-true (wf:wf-config/target? "canonical/config" cfg))
   (check-true (string? html)))

  (test-case
   "defrel+run* canonical translation is canonical/config and wf"
   (define-values (cfg html)
     (parse-src/canonical
      "(defrel (same x y) (== x y))
(run* (q) (same q 'cat))"))
   (check-true (redex-match? canonical:canonical-lang config cfg))
   (check-true (wf:wf-config/target? "canonical/config" cfg))
   (check-true (string? html)))

  (test-case
   "relation-call arity mismatch parses but is rejected by wf"
   (define-values (cfg _html)
     (parse-src/canonical
      "(defrel (same x y) (== x y))
(run* (q) (same q))"))
   (check-true (redex-match? canonical:canonical-lang config cfg))
   (check-false (wf:wf-config/target? "canonical/config" cfg)))

  )

(define/provide-test-suite TRANSPILER
  #:after (thunk (displayln "Finished running tests for transpiler."))

  SOURCE-MODES
  ASSOCIATIVITY
  COMPILE-PROFILES
  MICRO-SOURCE
  MICRO-RENDERING
  DISEQUALITY-TRANSLATION
  CANONICAL-TRANSLATION)

(module+ test
  (run-tests TRANSPILER))
