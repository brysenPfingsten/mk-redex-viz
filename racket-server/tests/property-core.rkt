#lang racket

(require rackunit
         rackunit/text-ui
         racket/format
         racket/list
         redex/reduction-semantics
         (prefix-in rt: "../src/random-test-support.rkt")
         (prefix-in gk: "./generator-kernel.rkt")
         "../src/core-definitions.rkt"
         "../src/wf-core.rkt"
         "../src/reduction-relations/core-reduction-relations.rkt")

;; Randomized test tuning constants.
;; Edit these values directly when you want different pressure/coverage.
(define PROPERTY-ATTEMPTS 200)
(define PROPERTY-TERM-SIZE 8)
(define PROPERTY-MAX-DEPTH 4)
(define PROPERTY-SEED 424242)
(define PROPERTY-U-POOL-SIZE 24)
(define PROPERTY-X-POOL-SIZE 16)
(define PROPERTY-R-POOL-SIZE 16)
(define PROPERTY-C-MAX 4)
(define PROPERTY-C-EXTRA-MAX 2)
(define PROPERTY-MIN-NONEMPTY-C-HITS 1)
(define PROPERTY-MIN-EXISTS-HITS 1)
(define PROPERTY-MIN-CONJ-HITS 1)

(gk:require-positive 'PROPERTY-ATTEMPTS PROPERTY-ATTEMPTS 'property-core)
(gk:require-positive 'PROPERTY-TERM-SIZE PROPERTY-TERM-SIZE 'property-core)
(gk:require-positive 'PROPERTY-U-POOL-SIZE PROPERTY-U-POOL-SIZE 'property-core)
(gk:require-positive 'PROPERTY-X-POOL-SIZE PROPERTY-X-POOL-SIZE 'property-core)
(gk:require-positive 'PROPERTY-R-POOL-SIZE PROPERTY-R-POOL-SIZE 'property-core)
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

(define (prandom n)
  (rt:rng-random PROPERTY-RNG n))

(define (final-config? cfg)
  (redex-match? Core end-config cfg))

(define (wf-config-term? cfg)
  (judgment-holds (wf-config? ,cfg)))

(define (core-shape-term? cfg)
  (judgment-holds (core-shape? ,cfg)))

(define (unique-decomposition? cfg)
  (define next* (apply-reduction-relation -->cfg cfg))
  (cond
    [(final-config? cfg) (null? next*)]
    [else (= (length next*) 1)]))

(define (progress? cfg)
  (or (final-config? cfg)
      (not (null? (apply-reduction-relation -->cfg cfg)))))

(define (wf-preserved? cfg)
  (for/and ([cfg^ (in-list (apply-reduction-relation -->cfg cfg))])
    (wf-config-term? cfg^)))

(define (core-shape-preserved? cfg)
  (and (core-shape-term? cfg)
       (for/and ([cfg^ (in-list (apply-reduction-relation -->cfg cfg))])
         (core-shape-term? cfg^))))

;; Pool sizes bound generated test-data diversity only; they do not bound the
;; semantic logic-variable/name space of the language.
(define U-POOL
  (gk:make-u-pool PROPERTY-U-POOL-SIZE))

(define X-POOL
  (gk:make-x-pool PROPERTY-X-POOL-SIZE))

(define R-POOL
  (gk:make-r-pool PROPERTY-R-POOL-SIZE))

(define (extend-c c max-extra)
  (when (> (length c) PROPERTY-C-MAX)
    (error 'extend-c
           (format "incoming c is too large: |c|=~a, PROPERTY-C-MAX=~a"
                   (length c) PROPERTY-C-MAX)))
  (gk:extend-c/rng PROPERTY-RNG c U-POOL PROPERTY-C-MAX max-extra))

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

(define (max-depth)
  PROPERTY-MAX-DEPTH)

(define (gen-tree c depth)
  (define options
    (append '(empty answer goal-state)
            (if (zero? depth) '() '(conj-tree))))
  (case (pick-one options)
    [(empty) '(empty-tree)]
    [(answer)
     (define c^ (extend-c c PROPERTY-C-EXTRA-MAX))
     `(⊤ ,(gen-state c^))]
    [(goal-state)
     (define c^ (extend-c c PROPERTY-C-EXTRA-MAX))
     `(,(gen-goal '() c^ depth)
       ,(gen-state c^))]
    [(conj-tree)
     (define c^ (extend-c c PROPERTY-C-EXTRA-MAX))
     `(,(gen-tree c^ (sub1 depth))
       ×
       ,(gen-goal '() c^ (sub1 depth))
       ,c^)]))

(define (gen-rel-def r)
  (define d (rt:random-distinct/rng PROPERTY-RNG X-POOL (prandom 3)))
  `(,r
    ,d
    ,(gen-goal d '() (max-depth))))

(define (gen-rel-env)
  (define count (prandom 3))
  (map gen-rel-def
       (rt:random-distinct/rng PROPERTY-RNG R-POOL count)))

(define (generate-wf-config/constructive)
  (define cfg
    `(,(gen-rel-env)
      ,(gen-tree '() (max-depth))
      (empty-stream)))
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

(define (answer-stream-coverage as)
  (match as
    [`(empty-stream)
     (values #f 0)]
    [`(⊤ ,st)
     (define csz (state-c-size st))
     (values (> csz 0) csz)]
    [`((⊤ ,st) + ,as2)
     (define csz (state-c-size st))
     (define-values (nonempty?2 cmax2) (answer-stream-coverage as2))
     (values (or (> csz 0) nonempty?2)
             (max csz cmax2))]
    [_ (values #f 0)]))

(define (config-coverage cfg)
  (match cfg
    [`(,Gamma ,s_work ,as)
     (define-values (has-exists? has-conj?)
       (for/fold ([has-exists? #f]
                  [has-conj? #f])
                 ([rel (in-list Gamma)])
         (match rel
           [`(,_ ,_ ,g)
            (define-values (hex hconj) (goal-flags g))
            (values (or has-exists? hex)
                    (or has-conj? hconj))]
           [_ (values has-exists? has-conj?)])))
     (define-values (tree-nonempty tree-exists tree-conj tree-cmax)
       (tree-coverage s_work))
     (define-values (stream-nonempty stream-cmax)
       (answer-stream-coverage as))
     (values (or tree-nonempty stream-nonempty)
             (or has-exists? tree-exists)
             (or has-conj? tree-conj)
             (max tree-cmax stream-cmax))]
    [`(,Gamma ,s_work)
     (config-coverage `(,Gamma ,s_work (empty-stream)))]
    [_ (values #f #f #f 0)]))

(define (check-wf-guarded-property label pred)
  (define wf-hits 0)
  (define fail-count 0)
  (define nonempty-c-hits 0)
  (define exists-node-hits 0)
  (define conj-node-hits 0)
  (define max-c-size-seen 0)
  (define fail-samples '())

  (for ([_ (in-range PROPERTY-ATTEMPTS)])
    (define cfg (generate-wf-config/constructive))
    (set! wf-hits (add1 wf-hits))
    (define-values (nonempty-c? has-exists? has-conj? cmax) (config-coverage cfg))
    (when nonempty-c? (set! nonempty-c-hits (add1 nonempty-c-hits)))
    (when has-exists? (set! exists-node-hits (add1 exists-node-hits)))
    (when has-conj? (set! conj-node-hits (add1 conj-node-hits)))
    (set! max-c-size-seen (max max-c-size-seen cmax))
    (unless (pred cfg)
      (set! fail-count (add1 fail-count))
      ;; Store only the first three failures for readable output truncation.
      (when (< (length fail-samples) 3)
        (set! fail-samples (cons cfg fail-samples)))))

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

(define-test-suite CORE-PROPERTIES
  (test-case "WF-guarded unique decomposition"
    (check-wf-guarded-property "unique-decomposition" unique-decomposition?))
  (test-case "WF-guarded progress"
    (check-wf-guarded-property "progress" progress?))
  (test-case "WF-guarded one-step preservation"
    (check-wf-guarded-property "wf-preserved" wf-preserved?))
  (test-case "WF-guarded core-shape closure"
    (check-wf-guarded-property "core-shape-preserved" core-shape-preserved?)))

(define/provide-test-suite PROPERTY-CORE
  #:before
  (thunk
   (displayln
    (format "Running core property tests (attempts=~a, term-size=~a, seed=~a, pools u/x/r=~a/~a/~a, c-max=~a, c-extra-max=~a)..."
            PROPERTY-ATTEMPTS
            PROPERTY-TERM-SIZE
            PROPERTY-SEED
            PROPERTY-U-POOL-SIZE
            PROPERTY-X-POOL-SIZE
            PROPERTY-R-POOL-SIZE
            PROPERTY-C-MAX
            PROPERTY-C-EXTRA-MAX)))
  #:after (thunk (displayln "Finished core property tests."))
  CORE-PROPERTIES)
