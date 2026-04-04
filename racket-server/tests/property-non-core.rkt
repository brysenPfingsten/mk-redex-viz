#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in red: "../src/search-lattice/reduction-relations/all.rkt")
         "../src/search-lattice/picture.rkt"
         "../src/search-runtime.rkt"
         "../src/search-strategy.rkt"
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         "./example-compat-tests.rkt"
         "./frontier-observable-support.rkt"
         "./search-lattice-support.rkt")

(provide PROPERTY-NON-CORE)

(define TRACE-CAP 96)
(define ACCOUNTING-TRACE-CAP 160)

(define local-example-specs
  (list
   (list "fresh delay witness"
         "micro"
         "(run* (q)\n  (fresh (x)\n    (conj\n      (Zzz (== x 'nap))\n      (== q x))))")))

(define (example-spec label)
  (or (for/first ([pr (in-list (frontend-example-programs))]
                  #:do [(match-define (cons example-label src) pr)]
                  #:when (equal? example-label label))
        (list "mini" src))
      (for/first ([spec (in-list local-example-specs)]
                  #:do [(match-define (list example-label source-mode src) spec)]
                  #:when (equal? example-label label))
        (list source-mode src))))

(define (parse-example/canonical label)
  (define spec (example-spec label))
  (unless spec
    (error 'parse-example/canonical
           "missing example label: ~a"
           label))
  (match-define (list source-mode src) spec)
  (define-values (cfg _html)
    (parse-prog/canonical (read-all-sexprs (open-input-string src))
                          #:source-mode source-mode))
  cfg)

(define/match (strategy-label strategy)
  [((search-strategy hoist scheduler))
   (format "~a/~a" hoist scheduler)])

(define (count-step-name steps expected [count 0])
  (match steps
    ['() count]
    [(cons step-name rest)
     (count-step-name rest
                      expected
                      (if (or (string=? step-name expected)
                              (and (string=? expected "fresh-substitute")
                                   (string-prefix? "fresh-substitute"
                                                   step-name)))
                          (add1 count)
                          count))]))

(define (trace-stepper stepper cfg [remaining TRACE-CAP] [cfgs '()] [steps '()])
  (define cfgs^ (cons cfg cfgs))
  (match (remove-duplicates (stepper cfg))
    ['()
     (values (reverse cfgs^)
             (reverse steps)
             cfg
             (if (final-program? cfg) 'value 'stuck))]
    [_ #:when (zero? remaining)
       (values (reverse cfgs^) (reverse steps) cfg 'cap)]
    [(list (list name cfg^))
     (trace-stepper stepper
                    cfg^
                    (sub1 remaining)
                    cfgs^
                    (cons (~a name) steps))]
    [_ (values (reverse cfgs^) (reverse steps) cfg 'nondeterministic)]))

(define (picture-contains-name? node expected)
  (match node
    [(? hash?)
     (or (equal? (hash-ref node 'name #f) expected)
         (for/or ([child (in-list (hash-ref node 'children '()))])
           (picture-contains-name? child expected)))]
    [_ #f]))

(define (summary-counts-consistent? cfg)
  (= (count-freshened cfg)
     (+ (count-freshened-tree cfg)
        (count-freshened-shell cfg))))

(define (zero-bounced-implies-pictures-agree? cfg)
  (if (zero? (count-bounced cfg))
      (equal? (cfg->operational-picture cfg)
              (cfg->extensional-picture cfg))
      #t))

(define (non-core-picture-invariants? cfg)
  (and (config-c-scope-agreement? cfg)
       (config-exact-scope? cfg)
       (summary-counts-consistent? cfg)
       (visible-json-wf? (cfg->operational-picture cfg))
       (visible-json-wf? (cfg->extensional-picture cfg))
       (not (picture-contains-name? (cfg->extensional-picture cfg)
                                    "Bounced"))
       (zero-bounced-implies-pictures-agree? cfg)))

(define sigma-u0
  (term (state () () (u:0) () (label "su0"))))

(define valid-scoped-delayed-left-search
  (term (FreshenedTree (u:0)
                       (delay ((succeed (label "late")) ,sigma-u0))
                       (label "fresh"))))

(define valid-scoped-flip
  (term (,valid-scoped-delayed-left-search
         <-+
         (⊤ ,sigma-b))))

(define valid-scoped-rail
  (term (,valid-scoped-delayed-left-search
         <-+
         (⊤ ,sigma-b))))

(define representative-internal-trace-cases
  (list
   (list "delay"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:delay-red cfg))
         cfg-delay-goal)
   (list "search-base-seq"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-base-seq-red cfg))
         cfg-disj)
   (list "search-base-fused"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-base-fused-red cfg))
         cfg-disj)
   (list "search-dfs-seq"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-dfs-seq-red cfg))
         valid-scoped-flip)
   (list "search-dfs-fused"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-dfs-fused-red cfg))
         valid-scoped-flip)
   (list "search-flip-seq"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-flip-seq-red cfg))
         valid-scoped-flip)
   (list "search-flip-fused"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-flip-fused-red cfg))
         valid-scoped-flip)
   (list "rail-seq"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-seq-red cfg))
         valid-scoped-rail)
   (list "rail-fused"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-fused-red cfg))
         valid-scoped-rail)
   (list "calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:calls-red cfg))
         cfg-call)
   (list "search-dfs-seq-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-dfs-seq-calls-red cfg))
         cfg-call-branch)
   (list "search-dfs-fused-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-dfs-fused-calls-red cfg))
         cfg-call-branch)
   (list "search-flip-seq-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-flip-seq-calls-red cfg))
         cfg-call-branch)
   (list "search-flip-fused-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-flip-fused-calls-red cfg))
         cfg-call-branch)
   (list "rail-seq-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-seq-calls-red cfg))
         cfg-call-rail)
   (list "rail-fused-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-fused-calls-red cfg))
         cfg-call-rail)))

(define representative-surfaced-trace-cases
  (for*/list ([strategy (in-list all-surfaced-search-strategies)]
              [label (in-list '("fresh shared disj"
                                "fresh branch disj"
                                "fresh split conj"
                                "fresh delay witness"))])
    (list strategy
          label
          (parse-example/canonical label))))

(define accounting-surfaced-trace-cases
  (for*/list ([strategy (in-list all-surfaced-search-strategies)]
              [label (in-list '("fresh shared disj"
                                "fresh branch disj"
                                "fresh split conj"
                                "fresh delay witness"))])
    (list strategy
          label
          (parse-example/canonical label))))

(define-test-suite NON-CORE-PROPERTIES
  (test-case "representative internal non-core traces preserve summary, scope, and picture invariants"
    (for ([entry (in-list representative-internal-trace-cases)])
      (match-define (list label stepper cfg0) entry)
      (define-values (cfgs _steps final-cfg status)
        (trace-stepper stepper cfg0))
      (check-true (or (eq? status 'value) (eq? status 'cap))
                  (format "~a unexpectedly ~a at ~s"
                          label
                          status
                          final-cfg))
      (for ([cfg (in-list cfgs)]
            [idx (in-naturals)])
        (check-true (non-core-picture-invariants? cfg)
                    (format "~a violated invariants at step ~a: ~s"
                            label
                            idx
                            cfg)))))

  (test-case "representative surfaced traces preserve domain, summary, scope, and picture invariants"
    (for ([entry (in-list representative-surfaced-trace-cases)])
      (match-define (list strategy label cfg0) entry)
      (define-values (cfgs _steps final-cfg status)
        (trace-stepper (lookup-search-step-once strategy) cfg0))
      (check-true (or (eq? status 'value) (eq? status 'cap))
                  (format "~a / ~a unexpectedly ~a at ~s"
                          (strategy-label strategy)
                          label
                          status
                          final-cfg))
      (for ([cfg (in-list cfgs)]
            [idx (in-naturals)])
        (check-true (search-config-in-domain? strategy cfg)
                    (format "~a / ~a left its strategy domain at step ~a: ~s"
                            (strategy-label strategy)
                            label
                            idx
                            cfg))
        (check-true (non-core-picture-invariants? cfg)
                    (format "~a / ~a violated invariants at step ~a: ~s"
                            (strategy-label strategy)
                            label
                            idx
                            cfg)))))

  (test-case "Bounced wrappers are neutral except for bounced count"
    (define scoped-answer
      (term (FreshenedShell (u:0)
                            (⊤ (state () () (u:0) () (label "s")))
                            (label "fresh"))))
    (define bounced-scoped-answer
      (term (Bounced ,scoped-answer)))
    (check-true (non-core-picture-invariants? scoped-answer))
    (check-true (non-core-picture-invariants? bounced-scoped-answer))
    (check-equal? (count-answers bounced-scoped-answer)
                  (count-answers scoped-answer))
    (check-equal? (count-freshened bounced-scoped-answer)
                  (count-freshened scoped-answer))
    (check-equal? (count-freshened-tree bounced-scoped-answer)
                  (count-freshened-tree scoped-answer))
    (check-equal? (count-freshened-shell bounced-scoped-answer)
                  (count-freshened-shell scoped-answer))
    (check-equal? (count-bounced bounced-scoped-answer)
                  (add1 (count-bounced scoped-answer)))
    (check-equal? (cfg->extensional-picture bounced-scoped-answer)
                  (cfg->extensional-picture scoped-answer)))

  (test-case "Freshened tree and shell wrappers share the same visible semantics"
    (define tree-cfg
      (term (FreshenedTree (u:0)
                           (⊤ (state () () (u:0) () (label "s")))
                           (label "fresh"))))
    (define shell-cfg
      (term (FreshenedShell (u:0)
                            (⊤ (state () () (u:0) () (label "s")))
                            (label "fresh"))))
    (check-true (non-core-picture-invariants? tree-cfg))
    (check-true (non-core-picture-invariants? shell-cfg))
    (check-equal? (count-answers tree-cfg) 1)
    (check-equal? (count-answers shell-cfg) 1)
    (check-equal? (count-freshened tree-cfg) 1)
    (check-equal? (count-freshened shell-cfg) 1)
    (check-equal? (count-freshened-tree tree-cfg) 1)
    (check-equal? (count-freshened-shell tree-cfg) 0)
    (check-equal? (count-freshened-tree shell-cfg) 0)
    (check-equal? (count-freshened-shell shell-cfg) 1)
    (check-equal? (cfg->operational-picture tree-cfg)
                  (cfg->operational-picture shell-cfg))
    (check-equal? (cfg->extensional-picture tree-cfg)
                  (cfg->extensional-picture shell-cfg)))

  (test-case "completed surfaced fresh traces keep freshened and bounced accounting exact"
    (for ([entry (in-list accounting-surfaced-trace-cases)])
      (match-define (list strategy label cfg0) entry)
      (define-values (_cfgs steps final-cfg status)
        (trace-stepper (lookup-search-step-once strategy)
                       cfg0
                       ACCOUNTING-TRACE-CAP))
      (check-equal? status
                    'value
                    (format "~a / ~a did not finish under accounting cap"
                            (strategy-label strategy)
                            label))
      (check-true (non-core-picture-invariants? final-cfg)
                  (format "~a / ~a final config violated invariants: ~s"
                          (strategy-label strategy)
                          label
                          final-cfg))
      (check-equal? (count-freshened final-cfg)
                    (count-step-name steps "fresh-substitute")
                    (format "~a / ~a freshened accounting drifted"
                            (strategy-label strategy)
                            label))
      (check-equal? (count-bounced final-cfg)
                    (count-step-name steps "invoke-delay")
                    (format "~a / ~a bounced accounting drifted"
                            (strategy-label strategy)
                            label)))))

(define/provide-test-suite PROPERTY-NON-CORE
  NON-CORE-PROPERTIES)

(module+ test
  (run-tests PROPERTY-NON-CORE))
