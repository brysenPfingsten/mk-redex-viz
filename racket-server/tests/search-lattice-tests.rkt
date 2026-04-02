#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in lang:
                    "../src/search-lattice/languages/all.rkt")
         "../src/search-lattice/languages/rail-fused-calls-lang.rkt"
         "../src/search-lattice/languages/search-base-seq-calls-lang.rkt"
         (prefix-in red:
                    "../src/search-lattice/reduction-relations/all.rkt")
         "../src/search-lattice/reduction-relations/private/common.rkt"
         (only-in "../src/search-lattice/reduction-relations/search-base-fused-calls-red.rkt"
                  search-base-fused-calls-red)
         (only-in "../src/search-lattice/reduction-relations/search-base-seq-calls-red.rkt"
                  search-base-seq-calls-red)
         (prefix-in wf:
                    "../src/search-lattice/wf/all.rkt")
         "./search-lattice-support.rkt")

(define (named-step succ*)
  (match succ*
    [(list (list name cfg))
     (values name cfg)]
    [_ (error 'named-step "expected exactly one tagged successor, got ~e" succ*)]))

(define/provide-test-suite SEARCH-LATTICE
  (test-case "feature languages reflect the new split and omit proceed"
    (check-true (redex-match? lang:delay-lang s '(delay (empty-tree))))
    (check-false (redex-match? lang:delay-lang s '(proceed (empty-tree))))
    (check-true (redex-match? lang:calls-lang g '(r:delay (label "call"))))
    (check-true (redex-match? lang:disj-seq-lang KDisj (term (hole <-+ (empty-tree)))))
    (check-true (redex-match? lang:disj-fused-lang K (term (hole <-+ (empty-tree)))))
    (check-true (redex-match? lang:rail-seq-lang s '((empty-tree) +-> (empty-tree))))
    (check-true (redex-match? lang:rail-fused-lang s '((empty-tree) +-> (empty-tree))))
    (check-true (redex-match? lang:calls-lang config (term ,cfg-call))))

  (test-case "disj-seq distributes immediately while disj-fused keeps mixed states"
    (define-values (seq-name _seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-seq-red
        cfg-mixed-answer)))
    (define-values (fused-answer-name _fused-answer-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-fused-red
        cfg-mixed-answer)))
    (define-values (fused-fail-name _fused-fail-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-fused-red
        cfg-mixed-fail)))
    (check-equal? (~a seq-name) "disj-seq/distribute-over-conj")
    (check-equal? (~a fused-answer-name) "disj-fused/continue-left-answer")
    (check-equal? (~a fused-fail-name) "disj-fused/continue-left-fail"))

  (test-case "search-base search-only branches handle explicit delay with no relcalls"
    (for ([rel (in-list (list red:search-base-seq-red
                              red:search-base-fused-red))])
      (define-values (step1-name step1)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-delay-goal)))
      (define-values (step2-name _step2)
        (named-step (apply-reduction-relation/tag-with-names rel step1)))
      (check-equal? (~a step1-name) "delay/suspend-goal")
      (check-equal? (~a step2-name) "delay/invoke-delay")))

  (test-case "search-only scheduler variants differ only in delayed left-branch policy"
    (for ([entry (in-list
                  (list (list red:search-dfs-seq-red
                              "search-dfs-seq/delay-through-left"
                              (term ((delay ((empty-tree) <-+ (⊤ ,sigma-b)))
                                     (empty-stream))))
                        (list red:search-dfs-fused-red
                              "search-dfs-fused/delay-through-left"
                              (term ((delay ((empty-tree) <-+ (⊤ ,sigma-b)))
                                     (empty-stream))))
                        (list red:search-flip-seq-red
                              "search-flip-seq/delay-swap-left"
                              (term ((delay ((⊤ ,sigma-b) <-+ (empty-tree)))
                                     (empty-stream))))
                        (list red:search-flip-fused-red
                              "search-flip-fused/delay-swap-left"
                              (term ((delay ((⊤ ,sigma-b) <-+ (empty-tree)))
                                     (empty-stream))))))])
      (match-define (list rel expected-name expected-template) entry)
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-flip)))
      (check-equal? (~a step-name) expected-name)
      (check-equal? next expected-template))))

  (test-case "rail search-only branches enter the railroad from delayed left disjunction"
    (for ([entry (in-list
                  (list (list red:rail-seq-red "rail-seq/enter-right")
                        (list red:rail-fused-red "rail-fused/enter-right")))])
      (match-define (list rel expected-name) entry)
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-rail)))
      (check-equal? (~a step-name) expected-name)
      (check-true
       (or (redex-match? lang:rail-seq-lang cfg next)
           (redex-match? lang:rail-fused-lang cfg next)))))

  (test-case "calls overlay expands relcalls once and still omits proceed"
    (define-values (step-name next)
      (named-step (apply-reduction-relation/tag-with-names red:calls-red cfg-call)))
    (check-equal? (~a step-name) "calls/expand")
    (check-false (redex-match? lang:calls-lang s '(proceed (empty-tree))))
    (check-true (redex-match? lang:calls-lang config next)))

  (test-case "search-base +calls branches expand inside their chosen search discipline"
    (define-values (seq-name seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        search-base-seq-calls-red
        cfg-call-branch)))
    (define-values (fused-name fused-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        search-base-fused-calls-red
        cfg-call-branch)))
    (check-equal? (~a seq-name) "search-base-seq-calls/expand")
    (check-equal? (~a fused-name) "search-base-fused-calls/expand")
    (check-true (redex-match? search-base-seq-calls-lang config seq-next))
    (check-true (redex-match? lang:search-base-fused-calls-lang config fused-next)))

  (test-case "scheduled +calls reducers are deterministic and shape-closed"
    (for ([entry (in-list
                  (list (list (lambda (prog) (redex-match? lang:search-base-seq-calls-lang config prog))
                              red:search-dfs-seq-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-fused-calls-lang config prog))
                              red:search-dfs-fused-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-seq-calls-lang config prog))
                              red:search-flip-seq-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-fused-calls-lang config prog))
                              red:search-flip-fused-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:rail-seq-calls-lang config prog))
                              red:rail-seq-calls-red
                              cfg-call-rail)
                        (list (lambda (prog) (redex-match? lang:rail-fused-calls-lang config prog))
                              red:rail-fused-calls-red
                              cfg-call-rail)))])
      (match-define (list matcher rel prog) entry)
      (check-true (progress? rel prog))
      (check-true (unique-decomposition? rel prog))
      (check-true (states-wf? prog))
      (check-true (shape-closed? matcher rel prog))))

  (test-case "scheduler/calls assembly commutes on representative seq and fused examples"
    (define alt-search-dfs-seq-calls-red
      (union-reduction-relations
       (context-closure
        (extend-reduction-relation red:search-dfs-seq-red search-base-seq-calls-lang)
        search-base-seq-calls-lang
        (Γ hole))
       (reduction-relation
        search-base-seq-calls-lang
        #:domain config
        [--> (Γ ((in-hole KDisj (in-hole K ((r t ... tag) σ))) as_1))
             (Γ ((in-hole KDisj (in-hole K (g_new σ))) as_1))
             (where g_new
                    ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
             "alt-search-dfs-seq-calls/expand"])))
    (define alt-rail-fused-calls-red
      (union-reduction-relations
       (context-closure
        (extend-reduction-relation red:rail-fused-red rail-fused-calls-lang)
        rail-fused-calls-lang
        (Γ hole))
       (reduction-relation
        rail-fused-calls-lang
        #:domain config
        [--> (Γ ((in-hole K ((r t ... tag) σ)) as_1))
             (Γ ((in-hole K (g_new σ)) as_1))
             (where g_new
                    ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
             "alt-rail-fused-calls/expand"])))
    (check-equal?
     (apply-reduction-relation red:search-dfs-seq-calls-red cfg-call-branch)
     (apply-reduction-relation alt-search-dfs-seq-calls-red cfg-call-branch))
    (check-equal?
     (apply-reduction-relation red:rail-fused-calls-red cfg-call-rail)
     (apply-reduction-relation alt-rail-fused-calls-red cfg-call-rail)))

  (test-case "progress, determinism, state wf, and shape closure hold across the internal lattice"
    (for ([entry (in-list
                  (list (list (lambda (prog) (redex-match? lang:core-lang cfg prog))
                              red:core-red
                              (term ((⊤ ,sigma-a) (empty-stream))))
                        (list (lambda (prog) (redex-match? lang:delay-lang cfg prog))
                              red:delay-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:disj-seq-lang cfg prog))
                              red:disj-seq-red cfg-mixed-answer)
                        (list (lambda (prog) (redex-match? lang:disj-fused-lang cfg prog))
                              red:disj-fused-red cfg-mixed-answer)
                        (list (lambda (prog) (redex-match? lang:search-base-seq-lang cfg prog))
                              red:search-base-seq-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:search-base-fused-lang cfg prog))
                              red:search-base-fused-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:search-base-seq-lang cfg prog))
                              red:search-dfs-seq-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-base-fused-lang cfg prog))
                              red:search-dfs-fused-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-base-seq-lang cfg prog))
                              red:search-flip-seq-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-base-fused-lang cfg prog))
                              red:search-flip-fused-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:rail-seq-lang cfg prog))
                              red:rail-seq-red cfg-rail)
                        (list (lambda (prog) (redex-match? lang:rail-fused-lang cfg prog))
                              red:rail-fused-red cfg-rail)
                        (list (lambda (prog) (redex-match? lang:calls-lang config prog))
                              red:calls-red cfg-call)
                        (list (lambda (prog) (redex-match? lang:search-base-seq-calls-lang config prog))
                              red:search-dfs-seq-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-fused-calls-lang config prog))
                              red:search-dfs-fused-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-seq-calls-lang config prog))
                              red:search-flip-seq-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-fused-calls-lang config prog))
                              red:search-flip-fused-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:rail-seq-calls-lang config prog))
                              red:rail-seq-calls-red cfg-call-rail)
                        (list (lambda (prog) (redex-match? lang:rail-fused-calls-lang config prog))
                              red:rail-fused-calls-red cfg-call-rail)))])
      (match-define (list matcher rel prog) entry)
      (check-true (progress? rel prog))
      (check-true (unique-decomposition? rel prog))
      (check-true (states-wf? prog))
      (check-true (shape-closed? matcher rel prog))))

  (test-case "WF judgments align with the new search-only and calls split"
    (check-true
     (judgment-holds
      (wf:wf-cfg/core? ((⊤ ,sigma-a) (empty-stream)))))
    (check-true
     (judgment-holds
      (wf:wf-cfg/delay? ,cfg-delay-goal)))
    (check-true
     (judgment-holds
      (wf:wf-cfg/disj? ,cfg-disj)))
    (check-true
     (judgment-holds
      (wf:wf-cfg/search-base? ,cfg-flip)))
    (check-true
     (judgment-holds
      (wf:wf-cfg/rail?
       (((delay (empty-tree)) +-> (⊤ ,sigma-b))
        (empty-stream)))))
    (check-true
     (judgment-holds
      (wf:wf-config/calls? ,cfg-call)))
    (check-true
     (judgment-holds
      (wf:wf-config/search-base-calls? ,cfg-call-branch)))
    (check-true
     (judgment-holds
      (wf:wf-config/rail-calls?
       (,gamma-delay
        (((delay (empty-tree)) +-> (⊤ ,sigma-b))
         (empty-stream)))))))

(module+ test
  (run-tests SEARCH-LATTICE))
