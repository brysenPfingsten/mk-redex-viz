#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in red:
                    "../src/search-lattice/reduction-relations/all.rkt")
         (prefix-in wf:
                    "../src/search-lattice/wf/all.rkt")
         "../src/search-runtime.rkt"
         "../src/search-strategy.rkt"
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         "./example-compat-tests.rkt"
         "./frontier-observable-support.rkt"
         "./search-lattice-support.rkt")

(provide WF-TRACES)

(define TRACE-CAP 64)

(define cfg-core-fresh
  (term ((∃ (x:0)
            ((x:0 =? (sym "fresh") (label "eq-1"))
             ∧
             (x:0 =? (sym "fresh") (label "eq-2"))
             (label "and-0"))
            (label "ex-0"))
         (state () () () () (label "s")))))

(define (example-src label)
  (for/first ([pr (in-list (frontend-example-programs))]
              #:do [(match-define (cons example-label src) pr)]
              #:when (equal? example-label label))
    src))

(define (example-cfg label)
  (define src (example-src label))
  (unless src
    (error 'example-cfg "missing example label: ~a" label))
  (define-values (cfg _html)
    (parse-prog/canonical (read-all-sexprs (open-input-string src))))
  cfg)

(define (trace-summary stepper
                       invariant?
                       cfg
                       [remaining TRACE-CAP]
                       [steps 0]
                       [last-step "<start>"])
  (cond
    [(negative? remaining)
     (values 'cap steps last-step cfg)]
    [(not (invariant? cfg))
     (values 'invariant-fail steps last-step cfg)]
    [else
     (match (remove-duplicates (stepper cfg))
       ['()
        (values (if (final-program? cfg) 'value 'stuck)
                steps
                last-step
                cfg)]
       [(list (list name cfg^))
        (trace-summary stepper
                       invariant?
                       cfg^
                       (sub1 remaining)
                       (add1 steps)
                       (~a name))]
       [_ (values 'nondeterministic steps last-step cfg)])]))

(define (check-trace-invariant label stepper invariant? cfg)
  (define-values (status steps last-step final-cfg)
    (trace-summary stepper invariant? cfg))
  (check-true (or (eq? status 'value) (eq? status 'cap))
              (format "~a failed (~a after ~a steps, last=~a, cfg=~s)"
                      label
                      status
                      steps
                      last-step
                      final-cfg)))

(define internal-trace-cases
  (list
   (list "core"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:core-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-cfg/core? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-core-fresh)
   (list "delay"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:delay-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-cfg/delay? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-delay-goal)
   (list "search-base-seq"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-base-seq-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-cfg/search-base? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-disj)
   (list "search-base-fused"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-base-fused-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-cfg/search-base? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-disj)
   (list "rail-seq"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-seq-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-cfg/rail? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-rail)
   (list "rail-fused"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-fused-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-cfg/rail? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-rail)
   (list "calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:calls-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-config/calls? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-call)
   (list "search-dfs-seq-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-dfs-seq-calls-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-config/search-base-calls? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-call-branch)
   (list "search-dfs-fused-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-dfs-fused-calls-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-config/search-base-calls? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-call-branch)
   (list "search-flip-seq-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-flip-seq-calls-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-config/search-base-calls? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-call-branch)
   (list "search-flip-fused-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:search-flip-fused-calls-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-config/search-base-calls? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-call-branch)
   (list "rail-seq-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-seq-calls-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-config/rail-calls? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-call-rail)
   (list "rail-fused-calls"
         (lambda (cfg)
           (apply-reduction-relation/tag-with-names red:rail-fused-calls-red cfg))
         (lambda (cfg)
           (and (judgment-holds (wf:wf-config/rail-calls? ,cfg))
                (produced-answer-spine-only? cfg)))
         cfg-call-rail)))

(define surfaced-trace-cases
  (for*/list ([strategy (in-list all-surfaced-search-strategies)]
              [label (in-list '("same"
                                "fresh branch disj"
                                "fives/fours"))])
    (list strategy
          label
          (example-cfg label))))

(define/provide-test-suite WF-TRACES
  (test-case "representative internal traces preserve wf and exact scope"
    (for ([entry (in-list internal-trace-cases)])
      (match-define (list label stepper invariant? cfg) entry)
      (check-trace-invariant label stepper invariant? cfg)))

  (test-case "surfaced runtime traces stay inside the selected strategy domain"
    (for ([entry (in-list surfaced-trace-cases)])
      (match-define (list strategy label cfg0) entry)
      (check-trace-invariant
       (format "~a / ~a" (search-strategy->jsexpr strategy) label)
       (lookup-search-step-once strategy)
       (lambda (cfg)
         (and (search-config-in-domain? strategy cfg)
              (produced-answer-spine-only? cfg)))
       cfg0))))

(module+ test
  (run-tests WF-TRACES))
