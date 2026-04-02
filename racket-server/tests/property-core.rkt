#lang racket

(require rackunit
         rackunit/text-ui
         json
         redex/reduction-semantics
         (prefix-in rt: "../src/random-test-support.rkt")
         (prefix-in gk: "./generator-kernel.rkt")
         "../src/canonical-json.rkt"
         "../src/search-lattice/languages/core-lang.rkt"
         "../src/search-lattice/wf/core-wf.rkt"
         "../src/search-lattice/reduction-relations/core-red.rkt"
         "./frontier-observable-support.rkt")

;; Randomized test tuning constants.
;; Edit these values directly when you want different pressure/coverage.
(define PROPERTY-ATTEMPTS 200)
(define PROPERTY-TERM-SIZE 8)
(define PROPERTY-MAX-DEPTH 4)
(define PROPERTY-SEED 424242)
(define PROPERTY-U-POOL-SIZE 24)
(define PROPERTY-X-POOL-SIZE 16)
(define PROPERTY-C-MAX 4)
(define PROPERTY-C-EXTRA-MAX 2)
(define PROPERTY-MIN-NONEMPTY-C-HITS 1)
(define PROPERTY-MIN-EXISTS-HITS 1)
(define PROPERTY-MIN-CONJ-HITS 1)

(gk:require-positive 'PROPERTY-ATTEMPTS PROPERTY-ATTEMPTS 'property-core)
(gk:require-positive 'PROPERTY-TERM-SIZE PROPERTY-TERM-SIZE 'property-core)
(gk:require-positive 'PROPERTY-U-POOL-SIZE PROPERTY-U-POOL-SIZE 'property-core)
(gk:require-positive 'PROPERTY-X-POOL-SIZE PROPERTY-X-POOL-SIZE 'property-core)
(gk:require-positive 'PROPERTY-C-MAX PROPERTY-C-MAX 'property-core)
(gk:require-nonnegative 'PROPERTY-C-EXTRA-MAX PROPERTY-C-EXTRA-MAX 'property-core)
(unless (<= PROPERTY-C-MAX PROPERTY-U-POOL-SIZE)
  (error 'property-core
         (format "PROPERTY-C-MAX must be <= PROPERTY-U-POOL-SIZE, got ~a > ~a"
                 PROPERTY-C-MAX PROPERTY-U-POOL-SIZE)))
(unless (<= 1 PROPERTY-MAX-DEPTH 4)
  (error 'property-core
         (format "PROPERTY-MAX-DEPTH must be in [1,4], got ~a"
                 PROPERTY-MAX-DEPTH)))
(unless (<= 0 PROPERTY-MIN-NONEMPTY-C-HITS PROPERTY-ATTEMPTS)
  (error 'property-core
         (format "PROPERTY-MIN-NONEMPTY-C-HITS must be in [0, PROPERTY-ATTEMPTS], got ~a"
                 PROPERTY-MIN-NONEMPTY-C-HITS)))
(unless (<= 0 PROPERTY-MIN-EXISTS-HITS PROPERTY-ATTEMPTS)
  (error 'property-core
         (format "PROPERTY-MIN-EXISTS-HITS must be in [0, PROPERTY-ATTEMPTS], got ~a"
                 PROPERTY-MIN-EXISTS-HITS)))
(unless (<= 0 PROPERTY-MIN-CONJ-HITS PROPERTY-ATTEMPTS)
  (error 'property-core
         (format "PROPERTY-MIN-CONJ-HITS must be in [0, PROPERTY-ATTEMPTS], got ~a"
                 PROPERTY-MIN-CONJ-HITS)))

(define PROPERTY-RNG (rt:make-seeded-rng PROPERTY-SEED))

(define (final-frontier? f)
  (match f
    ['(empty-tree) #t]
    [`(⊤ ,_) #t]
    [(or (list 'FreshenedTree _ inner _)
         (list 'FreshenedShell _ inner _))
     (final-frontier? inner)]
    [`(Bounced ,inner) (final-frontier? inner)]
    [`(,_ + ,rest) (final-frontier? rest)]
    [_ #f]))

(define (final-config? cfg)
  (match cfg
    [`(,_gamma ,f) (final-frontier? f)]
    [_ (final-frontier? cfg)]))

(define (wf-config-term? cfg)
  (judgment-holds (wf-cfg/core? ,cfg)))

(define (core-shape-term? cfg)
  (redex-match? core-lang cfg cfg))

(define (next-cfg* cfg)
  (remove-duplicates
   (apply-reduction-relation core-red cfg)))

(define (tagged-next* cfg)
  (remove-duplicates
   (apply-reduction-relation/tag-with-names core-red cfg)))

(define (unique-decomposition? cfg)
  (define next* (next-cfg* cfg))
  (cond
    [(final-config? cfg) (null? next*)]
    [else (= (length next*) 1)]))

(define (progress? cfg)
  (or (final-config? cfg)
      (not (null? (next-cfg* cfg)))))

(define (wf-preserved? cfg)
  (for/and ([cfg^ (in-list (next-cfg* cfg))])
    (wf-config-term? cfg^)))

(define (trace-wf-preserved? cfg [remaining SOURCE-TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (wf-config-term? cfg)) #f]
    [else
     (match (tagged-next* cfg)
       ['() #t]
       [(list (list _ cfg^))
        (trace-wf-preserved? cfg^ (sub1 remaining))]
       [_ #f])]))

(define (core-shape-preserved? cfg)
  (and (core-shape-term? cfg)
       (for/and ([cfg^ (in-list (next-cfg* cfg))])
         (core-shape-term? cfg^))))

(define (trace-core-shape-preserved? cfg [remaining SOURCE-TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (core-shape-term? cfg)) #f]
    [else
     (match (tagged-next* cfg)
       ['() #t]
       [(list (list _ cfg^))
        (trace-core-shape-preserved? cfg^ (sub1 remaining))]
       [_ #f])]))

(define SOURCE-TRACE-CAP 64)

(define (trace-exact-scope? cfg [remaining SOURCE-TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (config-exact-scope? cfg)) #f]
    [else
     (match (tagged-next* cfg)
       ['() #t]
       [(list (list _ cfg^))
       (trace-exact-scope? cfg^ (sub1 remaining))]
       [_ #f])]))

(define (trace-c-scope-agreement? cfg [remaining SOURCE-TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (config-c-scope-agreement? cfg)) #f]
    [else
     (match (tagged-next* cfg)
       ['() #t]
       [(list (list _ cfg^))
        (trace-c-scope-agreement? cfg^ (sub1 remaining))]
       [_ #f])]))

(define (count-step-name steps expected [count 0])
  (match steps
    ['() count]
    [(cons step-name rest)
     (count-step-name rest
                      expected
                      (if (or (string=? step-name expected)
                              (and (string=? expected "core/fresh-substitute")
                                   (string-prefix? "core/fresh-substitute"
                                                   step-name)))
                          (add1 count)
                          count))]))

(define (freshened-accounting? cfg)
  (define-values (steps final-cfg status)
    (trace-deterministic core-red cfg))
  (and (eq? status 'done)
      (config-c-scope-agreement? final-cfg)
       (config-exact-scope? final-cfg)
       (<= (count-step-name steps "core/fresh-substitute")
           (count-freshened final-cfg))))

(define (freshened-accounting/exact? cfg)
  (define-values (steps final-cfg status)
    (trace-deterministic core-red cfg))
  (and (eq? status 'done)
       (config-c-scope-agreement? final-cfg)
       (config-exact-scope? final-cfg)
       (= (count-step-name steps "core/fresh-substitute")
          (count-freshened final-cfg))
       (zero? (count-bounced final-cfg))))

(define (trace-freshened-monotone? cfg
                                   [remaining SOURCE-TRACE-CAP]
                                   [freshened-count (count-freshened cfg)])
  (cond
    [(negative? remaining) #f]
    [(< (count-freshened cfg) freshened-count) #f]
    [else
     (match (tagged-next* cfg)
       ['() #t]
       [(list (list _ cfg^))
        (trace-freshened-monotone? cfg^
                                   (sub1 remaining)
                                   (count-freshened cfg))]
       [_ #f])]))

(define (trace-zero-bounced? cfg [remaining SOURCE-TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (zero? (count-bounced cfg))) #f]
    [else
     (match (tagged-next* cfg)
       ['() #t]
       [(list (list _ cfg^))
        (trace-zero-bounced? cfg^ (sub1 remaining))]
       [_ #f])]))

(define (cfg->visible-json cfg)
  (string->jsexpr
   (to-json/canonical cfg (num-query-vars/canonical cfg))))

(define (visible-json-wf/cfg? cfg)
  (visible-json-wf? (cfg->visible-json cfg)))

(define (trace-visible-json-wf/cfg? cfg [remaining SOURCE-TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (visible-json-wf/cfg? cfg)) #f]
    [else
     (match (tagged-next* cfg)
       ['() #t]
       [(list (list _ cfg^))
        (trace-visible-json-wf/cfg? cfg^ (sub1 remaining))]
       [_ #f])]))

;; Pool sizes bound generated test-data diversity only; they do not bound the
;; semantic logic-variable/name space of the language.
(define U-POOL
  (gk:make-u-pool PROPERTY-U-POOL-SIZE))

(define X-POOL
  (gk:make-x-pool PROPERTY-X-POOL-SIZE))

(define (fresh-scope-extension c)
  (define unused
    (filter (lambda (u) (not (member u c))) U-POOL))
  (define room
    (min PROPERTY-C-EXTRA-MAX
         (- PROPERTY-C-MAX (length c))
         (length unused)))
  (cond
    [(zero? room)
     (values '() c)]
    [else
     (define intro
       (rt:random-distinct/rng PROPERTY-RNG
                               unused
                               (add1 (rt:rng-random PROPERTY-RNG room))))
     (values intro (append intro c))]))

(define (make-label prefix)
  (gk:make-label/rng PROPERTY-RNG prefix))

(define (pick-one xs)
  (gk:pick-one/rng PROPERTY-RNG xs))

(define (gen-term x-env c depth)
  (gk:gen-term/rng PROPERTY-RNG x-env c depth))

(define (gen-eq-goal x-env c depth)
  `(,(gen-term x-env c depth)
    =?
    ,(gen-term x-env c depth)
    ,(make-label "eq")))

(define (fresh-x-list x-env)
  (gk:fresh-x-list/rng PROPERTY-RNG x-env X-POOL))

(define (gen-goal x-env c depth)
  (define options
    (append '(succeed eq)
            (if (zero? depth) '() '(conj exists))))
  (case (pick-one options)
    [(succeed) `(succeed ,(make-label "ok"))]
    [(eq) (gen-eq-goal x-env c depth)]
    [(conj)
     `(,(gen-goal x-env c (sub1 depth))
       ∧
       ,(gen-goal x-env c (sub1 depth))
       ,(make-label "and"))]
    [(exists)
     (define d (fresh-x-list x-env))
     (define body
       (if (null? d)
           `(succeed ,(make-label "ok"))
             `(,(car d)
             =?
             ,(rt:gen-primitive/rng PROPERTY-RNG)
             ,(make-label "eq"))))
     `(∃
       ,d
       ,body
       ,(make-label "ex"))]))

(define (gen-state c)
  `(state () () ,c () ,(make-label "st")))

(define (generate-source-config)
  `(,(gen-goal '() '() (max-depth))
    ,(gen-state '())))

(define (max-depth)
  PROPERTY-MAX-DEPTH)

(define (gen-live-tree c depth)
  (define options
    (append '(goal-state)
            (if (zero? depth) '() '(conj-tree freshened-tree))))
  (case (pick-one options)
    [(goal-state)
     `(,(gen-goal '() c depth)
       ,(gen-state c))]
    [(conj-tree)
     `(,(gen-tree c (sub1 depth))
       ×
       ,(gen-goal '() c (sub1 depth))
       ,c)]
    [(freshened-tree)
     (define-values (intro c^)
       (fresh-scope-extension c))
     (if (null? intro)
         (gen-live-tree c (sub1 depth))
         `(FreshenedTree ,intro
                     ,(gen-live-tree c^ (sub1 depth))
                     ,(make-label "fresh")))]))

(define (gen-tree c depth)
  (define options
    (append '(empty goal-state)
            (if (zero? depth) '() '(conj-tree freshened-tree))))
  (case (pick-one options)
    [(empty) '(empty-tree)]
    [(goal-state) (gen-live-tree c depth)]
    [(conj-tree) (gen-live-tree c depth)]
    [(freshened-tree)
     (define-values (intro c^)
       (fresh-scope-extension c))
     (if (null? intro)
         (gen-live-tree c depth)
         `(FreshenedTree ,intro
                     ,(gen-live-tree c^ (sub1 depth))
                     ,(make-label "fresh")))]))

(define (generate-wf-config/constructive)
  (define cfg
    (gen-tree '() (max-depth)))
  (unless (wf-config-term? cfg)
    (error 'generate-wf-config/constructive
           (format "constructed non-wf config: ~s" cfg)))
  cfg)

(define (state-c-size st)
  (match st
    [`(state ,_ ,_ ,c ,_ ,_) (length c)]
    [_ 0]))

(define (goal-flags g)
  (match g
    [`(∃ ,_ ,g2 ,_) (define-values (hex hconj) (goal-flags g2))
                    (values #t hconj)]
    [`(,g1 ∧ ,g2 ,_) (define-values (hex1 hconj1) (goal-flags g1))
                     (define-values (hex2 hconj2) (goal-flags g2))
                     (values (or hex1 hex2) #t)]
    [_ (values #f #f)]))

(define (tree-coverage s)
  (match s
    [`(empty-tree) (values #f #f #f 0)]
    [(or (list 'FreshenedTree _ s-inner _)
         (list 'FreshenedShell _ s-inner _))
     (tree-coverage s-inner)]
    [`(⊤ ,st) (define csz (state-c-size st))
              (values (> csz 0) #f #f csz)]
    [`(,g ,st) (define csz (state-c-size st))
               (define-values (hex hconj) (goal-flags g))
               (values (> csz 0) hex hconj csz)]
    [`(,s1 × ,g ,c)
     (define-values (nonempty?1 hex1 hconj1 cmax1) (tree-coverage s1))
     (define-values (hex2 hconj2) (goal-flags g))
     (define csz (length c))
     (values (or nonempty?1 (> csz 0))
             (or hex1 hex2)
             (or #t hconj1 hconj2)
             (max cmax1 csz))]
    [_ (values #f #f #f 0)]))

(define (config-coverage cfg)
  (match cfg
    [s-work
     (define-values (tree-nonempty tree-exists tree-conj tree-cmax)
       (tree-coverage s-work))
     (values tree-nonempty
             tree-exists
             tree-conj
             tree-cmax)]
    [_ (values #f #f #f 0)]))

(define (check-wf-guarded-property label pred)
  (define-values (wf-hits
                  fail-count
                  nonempty-c-hits
                  exists-node-hits
                  conj-node-hits
                  max-c-size-seen
                  fail-samples)
    (for/fold ([wf-hits 0]
               [fail-count 0]
               [nonempty-c-hits 0]
               [exists-node-hits 0]
               [conj-node-hits 0]
               [max-c-size-seen 0]
               [fail-samples '()])
              ([_ (in-range PROPERTY-ATTEMPTS)])
      (define cfg
        (generate-wf-config/constructive))
      (define-values (nonempty-c? has-exists? has-conj? cmax)
        (config-coverage cfg))
      (define ok?
        (pred cfg))
      (values (add1 wf-hits)
              (if ok? fail-count (add1 fail-count))
              (if nonempty-c? (add1 nonempty-c-hits) nonempty-c-hits)
              (if has-exists? (add1 exists-node-hits) exists-node-hits)
              (if has-conj? (add1 conj-node-hits) conj-node-hits)
              (max max-c-size-seen cmax)
              (cond
                [(or ok? (>= (length fail-samples) 3))
                 fail-samples]
                [else
                 ;; Store only the first three failures for readable output truncation.
                 (cons cfg fail-samples)]))))

  (displayln
   (format "[property-core] ~a attempts=~a wf-hits=~a (~a%%) fails=~a nonempty-c=~a exists=~a conj=~a max-c=~a seed=~a"
           label
           PROPERTY-ATTEMPTS
           wf-hits
           (real->decimal-string (* 100.0
                                    (/ (exact->inexact wf-hits) PROPERTY-ATTEMPTS))
                                2)
           fail-count
           nonempty-c-hits
           exists-node-hits
           conj-node-hits
           max-c-size-seen
           PROPERTY-SEED))

  (check-equal? wf-hits
                PROPERTY-ATTEMPTS
                (format "~a: constructive generator violated wf contract." label))

  (check-true (>= nonempty-c-hits PROPERTY-MIN-NONEMPTY-C-HITS)
              (format "~a: insufficient non-empty-c coverage (~a < ~a)."
                      label nonempty-c-hits PROPERTY-MIN-NONEMPTY-C-HITS))
  (check-true (>= exists-node-hits PROPERTY-MIN-EXISTS-HITS)
              (format "~a: insufficient exists-node coverage (~a < ~a)."
                      label exists-node-hits PROPERTY-MIN-EXISTS-HITS))
  (check-true (>= conj-node-hits PROPERTY-MIN-CONJ-HITS)
              (format "~a: insufficient conjunction-node coverage (~a < ~a)."
                      label conj-node-hits PROPERTY-MIN-CONJ-HITS))

  (check-equal? fail-count
                0
                (format "~a: counterexamples (up to 3): ~s"
                        label
                        (reverse fail-samples))))

(define (check-source-guarded-property label pred)
  (define-values (source-hits
                  fail-count
                  exists-node-hits
                  conj-node-hits
                  fail-samples)
    (for/fold ([source-hits 0]
               [fail-count 0]
               [exists-node-hits 0]
               [conj-node-hits 0]
               [fail-samples '()])
              ([_ (in-range PROPERTY-ATTEMPTS)])
      (define cfg (generate-source-config))
      (define-values (_nonempty-c? has-exists? has-conj? _cmax)
        (config-coverage cfg))
      (define ok?
        (and (wf-config-term? cfg)
             (config-exact-scope? cfg)
             (pred cfg)))
      (values (add1 source-hits)
              (if ok? fail-count (add1 fail-count))
              (if has-exists? (add1 exists-node-hits) exists-node-hits)
              (if has-conj? (add1 conj-node-hits) conj-node-hits)
              (cond
                [(or ok? (>= (length fail-samples) 3))
                 fail-samples]
                [else
                 (cons cfg fail-samples)]))))

  (displayln
   (format "[property-core] ~a attempts=~a source-hits=~a exists=~a conj=~a seed=~a"
           label
           PROPERTY-ATTEMPTS
           source-hits
           exists-node-hits
           conj-node-hits
           PROPERTY-SEED))

  (check-equal? source-hits
                PROPERTY-ATTEMPTS
                (format "~a: source generator violated contract." label))

  (check-true (>= exists-node-hits PROPERTY-MIN-EXISTS-HITS)
              (format "~a: insufficient exists-node coverage (~a < ~a)."
                      label exists-node-hits PROPERTY-MIN-EXISTS-HITS))
  (check-true (>= conj-node-hits PROPERTY-MIN-CONJ-HITS)
              (format "~a: insufficient conjunction-node coverage (~a < ~a)."
                      label conj-node-hits PROPERTY-MIN-CONJ-HITS))

  (check-equal? fail-count
                0
                (format "~a: counterexamples (up to 3): ~s"
                        label
                        (reverse fail-samples))))

(define-test-suite CORE-PROPERTIES
  (test-case "WF-guarded unique decomposition"
    (check-wf-guarded-property "unique-decomposition" unique-decomposition?))
  (test-case "WF-guarded progress"
    (check-wf-guarded-property "progress" progress?))
  (test-case "WF-guarded one-step preservation"
    (check-wf-guarded-property "wf-preserved" wf-preserved?))
  (test-case "WF-guarded trace preservation"
    (check-wf-guarded-property "trace-wf-preserved" trace-wf-preserved?))
  (test-case "WF-guarded core-shape closure"
    (check-wf-guarded-property "core-shape-preserved" core-shape-preserved?))
  (test-case "WF-guarded trace core-shape closure"
    (check-wf-guarded-property "trace-core-shape-preserved"
                               trace-core-shape-preserved?))
  (test-case "Source-guarded exact Freshened scoping"
    (check-source-guarded-property "exact-scope" config-exact-scope?))
  (test-case "Source-guarded exact c/scope agreement"
    (check-source-guarded-property "c-scope-agreement" config-c-scope-agreement?))
  (test-case "Source-guarded exact c/scope agreement through trace"
    (check-source-guarded-property "trace-c-scope-agreement" trace-c-scope-agreement?))
  (test-case "Source-guarded exact Freshened scoping through trace"
    (check-source-guarded-property "trace-exact-scope" trace-exact-scope?))
  (test-case "WF-guarded visible AST shape"
    (check-wf-guarded-property "visible-json-wf" visible-json-wf/cfg?))
  (test-case "Source-guarded visible AST shape through trace"
    (check-source-guarded-property "trace-visible-json-wf" trace-visible-json-wf/cfg?))
  (test-case "Source-guarded Freshened accounting"
    (check-source-guarded-property "freshened-accounting" freshened-accounting?))
  (test-case "Source-guarded exact Freshened accounting"
    (check-source-guarded-property "freshened-accounting/exact"
                                   freshened-accounting/exact?))
  (test-case "Source-guarded Freshened monotonicity through trace"
    (check-source-guarded-property "trace-freshened-monotone"
                                   trace-freshened-monotone?))
  (test-case "Source-guarded core traces never introduce Bounced"
    (check-source-guarded-property "trace-zero-bounced"
                                   trace-zero-bounced?)))

(define/provide-test-suite PROPERTY-CORE
  #:before
  (thunk
   (displayln
    (format "Running core property tests (attempts=~a, term-size=~a, seed=~a, pools u/x=~a/~a, c-max=~a, c-extra-max=~a)..."
            PROPERTY-ATTEMPTS
            PROPERTY-TERM-SIZE
            PROPERTY-SEED
            PROPERTY-U-POOL-SIZE
            PROPERTY-X-POOL-SIZE
            PROPERTY-C-MAX
            PROPERTY-C-EXTRA-MAX)))
  #:after (thunk (displayln "Finished core property tests."))
  CORE-PROPERTIES)
