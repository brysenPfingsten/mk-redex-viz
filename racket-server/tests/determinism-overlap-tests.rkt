#lang racket

(require racket/runtime-path
         rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in rt:
                    "../src/random-test-support.rkt")
         (prefix-in red:
                    "../src/search-lattice/reduction-relations/all.rkt")
         (prefix-in lang:
                    "../src/search-lattice/languages/all.rkt")
         (prefix-in wf:
                    "../src/search-lattice/wf/all.rkt")
         "../src/search-runtime.rkt"
         "../src/search-strategy.rkt"
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         "./example-compat-tests.rkt"
         (only-in "./search-lattice-support.rkt"
                  sigma-a
                  sigma-b
                  sigma-s)
         "./runtime-test-support.rkt")

(provide DETERMINISM-OVERLAP)

(define OVERLAP-TRACE-CAP 25)
(define OVERLAP-RANDOM-SEEDS '(424242 777777 20260320))
(define OVERLAP-RANDOM-SAMPLES-PER-STRATEGY 10)
(define OVERLAP-RANDOM-TERM-DEPTH 8)
(define OVERLAP-RANDOM-MAX-REJECTS 800)

(define-runtime-path SEARCH-LATTICE-REDUCTION-RELATIONS-DIR
  "../src/search-lattice/reduction-relations")

(define (extension-source-file? p)
  (define name
    (path->string (file-name-from-path p)))
  (and (regexp-match? #rx"\\.rkt$" name)
       (not (regexp-match? #rx"^(?:\\.#|#)" name))
       (file-exists? p)
       (not (link-exists? p))
       (not (regexp-match? #rx"/archive/" (path->string p)))))

(define (parse-src/canonical src)
  (parse-prog/canonical (read-all-sexprs (open-input-string src))))

(define/match (strategy-label strategy)
  [((search-strategy hoist scheduler))
   (format "~a/~a" hoist scheduler)])

(define (trace-overlap-events strategy
                              cfg
                              [normalized (normalize-search-strategy strategy)]
                              [step-once (lookup-search-step-once normalized)]
                              [step-index 0]
                              [acc '()])
  (define tagged-next* (step-once cfg))
  (define acc*
    (cond
      [(overlap-kind tagged-next*)
       (cons (overlap-event (strategy-label normalized)
                            cfg
                            tagged-next*
                            step-index)
             acc)]
      [else acc]))
  (cond
    [(or (null? tagged-next*) (>= step-index OVERLAP-TRACE-CAP))
     (reverse acc*)]
    [else
     (trace-overlap-events strategy
                           (tagged-successor-cfg (first tagged-next*))
                           normalized
                           step-once
                           (add1 step-index)
                           acc*)]))

(define (strategy-matches-generated? strategy cfg)
  (match-define (strategy-spec _ _ in-domain? well-formed?)
    (lookup-strategy-spec strategy))
  (and (in-domain? cfg)
       (well-formed? cfg)))

(define (generate-random-config strategy
                                rng
                                [normalized (normalize-search-strategy strategy)]
                                [attempt 0])
  (when (>= attempt OVERLAP-RANDOM-MAX-REJECTS)
    (error 'generate-random-config
           "failed to generate wf config for ~a after ~a attempts"
           (strategy-label normalized)
           OVERLAP-RANDOM-MAX-REJECTS))
  (define cfg
    (parameterize ([current-pseudo-random-generator rng])
      (match normalized
        [(search-strategy "early" "rail")
         (generate-term lang:rail-calls-lang config OVERLAP-RANDOM-TERM-DEPTH)]
        [(search-strategy "late" "rail")
         (generate-term lang:rail-calls-lang config OVERLAP-RANDOM-TERM-DEPTH)]
        [(search-strategy "early" _)
         (generate-term lang:search-base-calls-lang config OVERLAP-RANDOM-TERM-DEPTH)]
        [(search-strategy "late" _)
         (generate-term lang:search-base-calls-lang config OVERLAP-RANDOM-TERM-DEPTH)])))
  (cond
    [(strategy-matches-generated? normalized cfg) cfg]
    [else
     (generate-random-config strategy
                             rng
                             normalized
                             (add1 attempt))]))

(define (compatible-example-seeds strategies)
  (define examples
    (frontend-example-programs))
  (for*/list ([strategy (in-list strategies)]
              [ex (in-list examples)])
    (match-define (cons _label src) ex)
    (define-values (cfg0 _html) (parse-src/canonical src))
    (and (search-config-in-domain? strategy cfg0)
         (search-config-well-formed? strategy cfg0)
         (hash 'strategy strategy
               'cfg cfg0))))

(define (drop-false xs)
  (for/list ([x (in-list xs)]
             #:when x)
    x))

(define (example-overlap-events)
  (define seeds
    (drop-false (compatible-example-seeds all-surfaced-search-strategies)))
  (for/list ([seed (in-list seeds)])
    (match-define (hash* ['strategy strategy]
                         ['cfg cfg]
                         #:open)
      seed)
    (trace-overlap-events strategy cfg)))

(define (random-overlap-events)
  (for*/list ([seed (in-list OVERLAP-RANDOM-SEEDS)]
              [strategy (in-list all-surfaced-search-strategies)])
    (define rng
      (rt:make-seeded-rng seed))
    (for/list ([cfg
                (in-list
                 (for/list ([_ (in-range OVERLAP-RANDOM-SAMPLES-PER-STRATEGY)])
                   (generate-random-config strategy rng)))])
      (trace-overlap-events strategy cfg))))

(define/provide-test-suite DETERMINISM-OVERLAP
  (test-case "policy guard: no rule-priority/name-based precedence in active search-lattice semantics"
    (for ([p (in-list (find-files extension-source-file? SEARCH-LATTICE-REDUCTION-RELATIONS-DIR))])
      (define src
        (file->string p))
      (check-false
       (regexp-match? #px"step-priority" src)
       (format "forbidden priority-based determinizer found in ~a" p))
      (check-false
       (regexp-match?
        #px"side-condition[^\\]]*apply-reduction-relation/tag-with-names"
        src)
       (format "forbidden rule-name-based precedence fence found in ~a" p))))

  (test-case "explicit counterexample families stay uniquely decomposed"
    (define pending-disj
      (term ((((succeed (label "left")) ,sigma-s)
              <-+
              ((succeed (label "right")) ,sigma-s))
             × (succeed (label "k"))
             ())))
    (define bounced-branch
      (term (Bounced (((⊤ ,sigma-a) <-+ (empty-tree))
                      <-+
                      (⊤ ,sigma-b)))))
    (define seq-next*
      (apply-reduction-relation/tag-with-names red:disj-seq-red pending-disj))
    (define fused-next*
      (apply-reduction-relation/tag-with-names red:disj-fused-red pending-disj))
    (define shell-next*
      (apply-reduction-relation/tag-with-names red:search-base-fused-red bounced-branch))
    (check-false (overlap-kind seq-next*))
    (check-false (overlap-kind fused-next*))
    (check-false (overlap-kind shell-next*))
    (check-equal? (tagged-successor-name (first seq-next*)) "distribute-over-conj")
    (check-equal? (tagged-successor-name (first fused-next*)) "succeed")
    (check-equal? (tagged-successor-name (first shell-next*)) "reassociate-left-answer"))

  (test-case "overlap audit: surfaced structured strategies over frontend examples"
    (define events
      (append* (example-overlap-events)))
    (check-true (null? events)
                (if (null? events)
                    "no example-corpus overlaps"
                    (format "example overlap events found: ~s" events))))

  (test-case "overlap audit: surfaced structured strategies over generated configs"
    (define events
      (append* (append* (random-overlap-events))))
    (check-true (null? events)
                (if (null? events)
                    "no generated-config overlaps"
                    (format "generated overlap events found: ~s" events)))))

(module+ test
  (run-tests DETERMINISM-OVERLAP))
