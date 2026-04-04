#lang racket

(require json
         rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "../src/search-lattice/picture.rkt"
         (prefix-in lang:
                    "../src/search-lattice/languages/all.rkt")
         (prefix-in red:
                    "../src/search-lattice/reduction-relations/all.rkt")
         "../src/search-lattice/reduction-relations/private/common.rkt"
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
    (check-true (redex-match? lang:core-lang QFresh (term (FreshenedTree (u:0) hole (label "fresh")))))
    (check-false (redex-match? lang:delay-lang cfg '(delay (empty-tree))))
    (check-true (redex-match? lang:delay-lang cfg (term ,delayed-left-search)))
    (check-false (redex-match? lang:delay-lang cfg '(proceed (empty-tree))))
    (check-true (redex-match? lang:calls-lang g '(r:delay (label "call"))))
    (check-true (redex-match? lang:disj-lang KBranch (term (hole <-+ (empty-tree)))))
    (check-true
     (redex-match?
      lang:disj-lang
      KLate
      (term (hole × (succeed (label "k")) ()))))
    (check-true (redex-match? lang:rail-lang cfg '((empty-tree) +-> (empty-tree))))
    (check-true
     (redex-match?
      lang:rail-lang
      KBranch
      (term ((empty-tree) +-> hole))))
    (check-false
     (redex-match?
      lang:core-lang
      search
      (term (((⊤ ,sigma-a) + (empty-tree))
             × (succeed (label "k"))
             ()))))
    (check-true (redex-match? lang:calls-lang config (term ,cfg-call))))

  (test-case "disj-seq distributes immediately while disj-fused keeps mixed states"
    (define pending-disj
      (term ((((succeed (label "left")) ,sigma-s)
              <-+
              ((succeed (label "right")) ,sigma-s))
             × (succeed (label "k"))
             ())))
    (define-values (seq-name _seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-seq-red
        pending-disj)))
    (define-values (fused-pending-name _fused-pending-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-fused-red
        pending-disj)))
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
    (check-equal? (~a fused-pending-name) "core/succeed")
    (check-equal? (~a fused-answer-name) "disj-fused/continue-left-answer")
    (check-equal? (~a fused-fail-name) "disj-fused/continue-left-fail"))

  (test-case "disj-fused continues freshened answers structurally"
    (define freshened-answer
      (term (((FreshenedTree (u:0) (⊤ ,sigma-a) (label "fresh")) <-+ (⊤ ,sigma-b))
             × (succeed (label "k"))
             ())))
    (define-values (step-name next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-fused-red
        freshened-answer)))
    (check-equal? (~a step-name) "disj-fused/continue-left-answer")
    (check-equal? next
                  (term ((FreshenedTree (u:0)
                                    ((succeed (label "k")) ,sigma-a)
                                    (label "fresh"))
                         <-+
                         ((⊤ ,sigma-b) × (succeed (label "k")) ())))))

  (test-case "search-base search-only branches handle explicit delay with no relcalls"
    (for ([rel (in-list (list red:search-base-seq-red
                              red:search-base-fused-red))])
      (define-values (step1-name step1)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-delay-goal)))
      (define-values (step2-name _step2)
        (named-step (apply-reduction-relation/tag-with-names rel step1)))
      (check-equal? (~a step1-name) "delay/suspend-goal")
      (check-equal? (~a step2-name) "delay/invoke-delay")))

  (test-case "scope-sensitive delay roots preserve QFresh outside and inside suspend"
    (define fresh-outside-suspend
      (term ((∃ (x:0)
                (suspend (x:0 =? (sym "nap") (label "eq"))
                         (label "zz"))
                (label "fresh"))
             ,sigma-s)))
    (define suspend-outside-fresh
      (term ((suspend
              (∃ (x:0)
                 (x:0 =? (sym "nap") (label "eq"))
                 (label "fresh"))
              (label "zz"))
             ,sigma-s)))
    (define scoped-state
      (term (state () () (u:0) () (label "s"))))
    (define scoped-eq
      (term (u:0 =? (sym "nap") (label "eq"))))
    (define uninstantiated-eq
      (term (x:0 =? (sym "nap") (label "eq"))))
    (define-values (fresh-step-1-name fresh-step-1)
      (named-step (apply-reduction-relation/tag-with-names
                   red:delay-red
                   fresh-outside-suspend)))
    (define-values (fresh-step-2-name fresh-step-2)
      (named-step (apply-reduction-relation/tag-with-names
                   red:delay-red
                   fresh-step-1)))
    (define-values (fresh-step-3-name fresh-step-3)
      (named-step (apply-reduction-relation/tag-with-names
                   red:delay-red
                   fresh-step-2)))
    (check-equal? (~a fresh-step-1-name) "core/fresh-substitute")
    (check-equal? (~a fresh-step-2-name) "delay/suspend-goal")
    (check-equal? (~a fresh-step-3-name) "delay/invoke-delay")
    (check-equal? fresh-step-2
                  (term (FreshenedTree (u:0)
                                       (delay (,scoped-eq ,scoped-state))
                                       (label "fresh"))))
    (check-equal? fresh-step-3
                  (term (FreshenedShell (u:0)
                                        (Bounced (,scoped-eq ,scoped-state))
                                        (label "fresh"))))
    (define-values (suspend-step-1-name suspend-step-1)
      (named-step (apply-reduction-relation/tag-with-names
                   red:delay-red
                   suspend-outside-fresh)))
    (define-values (suspend-step-2-name suspend-step-2)
      (named-step (apply-reduction-relation/tag-with-names
                   red:delay-red
                   suspend-step-1)))
    (define-values (suspend-step-3-name _suspend-step-3)
      (named-step (apply-reduction-relation/tag-with-names
                   red:delay-red
                   suspend-step-2)))
    (check-equal? (~a suspend-step-1-name) "delay/suspend-goal")
    (check-equal? (~a suspend-step-2-name) "delay/invoke-delay")
    (check-equal? (~a suspend-step-3-name) "core/fresh-substitute")
    (check-equal? suspend-step-1
                  (term (delay ((∃ (x:0)
                                   ,uninstantiated-eq
                                   (label "fresh"))
                                ,sigma-s))))
    (check-equal? suspend-step-2
                  (term (Bounced ((∃ (x:0)
                                     ,uninstantiated-eq
                                     (label "fresh"))
                                  ,sigma-s)))))

  (test-case "empty fresh frames are real scoped frames"
    (define empty-fresh
      (term ((∃ ()
                (succeed (label "inner"))
                (label "fresh-empty"))
             ,sigma-s)))
    (define-values (step-1-name step-1)
      (named-step (apply-reduction-relation/tag-with-names
                   red:core-red
                   empty-fresh)))
    (define-values (step-2-name step-2)
      (named-step (apply-reduction-relation/tag-with-names
                   red:core-red
                   step-1)))
    (define-values (step-3-name step-3)
      (named-step (apply-reduction-relation/tag-with-names
                   red:core-red
                   step-2)))
    (check-equal? (~a step-1-name) "core/fresh-substitute")
    (check-equal? (~a step-2-name) "core/succeed")
    (check-equal? (~a step-3-name) "core/final-answer-into-shell")
    (check-equal? step-1
                  (term (FreshenedTree ()
                                       ((succeed (label "inner")) ,sigma-s)
                                       (label "fresh-empty"))))
    (check-equal? step-2
                  (term (FreshenedTree ()
                                       (⊤ ,sigma-s)
                                       (label "fresh-empty"))))
    (check-equal? step-3
                  (term (FreshenedShell ()
                                        (⊤ ,sigma-s)
                                        (label "fresh-empty")))))

  (test-case "nested fresh traces preserve empty middle frames"
    (define nested-fresh
      (term ((∃ (x:0)
                (∃ ()
                   (∃ (x:1)
                      (succeed (label "ok"))
                      (label "fy"))
                   (label "fempty"))
                (label "fx"))
             ,sigma-s)))
    (define-values (step-1-name step-1)
      (named-step (apply-reduction-relation/tag-with-names
                   red:core-red
                   nested-fresh)))
    (define-values (step-2-name step-2)
      (named-step (apply-reduction-relation/tag-with-names
                   red:core-red
                   step-1)))
    (define-values (step-3-name step-3)
      (named-step (apply-reduction-relation/tag-with-names
                   red:core-red
                   step-2)))
    (check-equal? (~a step-1-name) "core/fresh-substitute")
    (check-equal? (~a step-2-name) "core/fresh-substitute")
    (check-equal? (~a step-3-name) "core/fresh-substitute")
    (check-equal? step-3
                  (term (FreshenedTree (u:0)
                                       (FreshenedTree ()
                                                       (FreshenedTree (u:1)
                                                                       ((succeed (label "ok"))
                                                                        (state () () (u:1 u:0) () (label "s")))
                                                                       (label "fy"))
                                                       (label "fempty"))
                                       (label "fx")))))

  (test-case "scoped delay-floating keeps subtree-local QFresh on the payload"
    (define scoped-conj-expected
      (term (delay ((FreshenedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))
                    × (succeed (label "k"))
                    ()))))
    (define scoped-dfs-expected
      (term (delay ((FreshenedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))
                    <-+
                    (⊤ ,sigma-b)))))
    (define scoped-flip-expected
      (term (delay ((⊤ ,sigma-b)
                    <-+
                    (FreshenedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))))))
    (define scoped-rail-expected
      (term (delay ((FreshenedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))
                    +-> (⊤ ,sigma-b)))))
    (define scoped-return-rail
      (term ((⊤ ,sigma-b) +-> ,scoped-delayed-left-search)))
    (define scoped-return-expected
      (term (delay ((⊤ ,sigma-b)
                    <-+
                    (FreshenedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))))))
    (define-values (conj-name conj-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:delay-red
        cfg-scoped-delay-through-conj)))
    (check-equal? (~a conj-name) "delay/delay-through-conj")
    (check-equal? conj-next scoped-conj-expected)
    (for ([entry (in-list
                  (list (list red:search-dfs-seq-red
                              cfg-scoped-flip
                              "search-dfs-seq/delay-through-left"
                              scoped-dfs-expected)
                        (list red:search-dfs-fused-red
                              cfg-scoped-flip
                              "search-dfs-fused/delay-through-left"
                              scoped-dfs-expected)
                        (list red:search-flip-seq-red
                              cfg-scoped-flip
                              "search-flip-seq/delay-swap-left"
                              scoped-flip-expected)
                        (list red:search-flip-fused-red
                              cfg-scoped-flip
                              "search-flip-fused/delay-swap-left"
                              scoped-flip-expected)
                        (list red:rail-seq-red
                              cfg-scoped-rail
                              "rail-seq/enter-right"
                              scoped-rail-expected)
                        (list red:rail-fused-red
                              cfg-scoped-rail
                              "rail-fused/enter-right"
                              scoped-rail-expected)
                        (list red:rail-seq-red
                              scoped-return-rail
                              "rail-seq/return-left"
                              scoped-return-expected)
                        (list red:rail-fused-red
                              scoped-return-rail
                              "rail-fused/return-left"
                              scoped-return-expected)))])
      (match-define (list rel cfg expected-name expected-next) entry)
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel cfg)))
      (check-equal? (~a step-name) expected-name)
      (check-equal? next expected-next)))

  (test-case "search-base promotes bare answers and forbids buried +"
    (define-values (seq-name seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-seq-red
        cfg-disj)))
    (define-values (fused-name fused-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-fused-red
        cfg-disj)))
    (define illegal-prefix-conj
      (term (((⊤ ,sigma-a) + (empty-tree))
             × (succeed (label "k"))
             ())))
    (check-false
     (redex-match?
      lang:search-base-lang
      cfg
      illegal-prefix-conj))
    (check-false
     (redex-match?
      lang:search-base-lang
      cfg
      (term (((⊤ ,sigma-a) + (empty-tree)) <-+ (⊤ ,sigma-b)))))
    (check-equal? (~a seq-name) "disj/promote-left-answer")
    (check-equal? (~a fused-name) "disj/promote-left-answer")
    (check-true (produced-answer-spine-only? seq-next))
    (check-true (produced-answer-spine-only? fused-next))
    (check-true (redex-match? lang:search-base-lang cfg seq-next))
    (check-true (redex-match? lang:search-base-lang cfg fused-next)))

  (test-case "search-base reassociates then closes bounced segments when an answer appears"
    (define bounced-branch
      (term (Bounced (((⊤ ,sigma-a) <-+ (empty-tree))
                      <-+
                      (⊤ ,sigma-b)))))
    (define bad-bounced-promotion
      (term (Bounced ((((⊤ ,sigma-a) + (empty-tree))
                       <-+
                       (⊤ ,sigma-b))))))
    (define-values (seq-name-1 seq-mid)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-seq-red
        bounced-branch)))
    (define-values (seq-name-2 seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-seq-red
        seq-mid)))
    (define-values (fused-name-1 fused-mid)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-fused-red
        bounced-branch)))
    (define-values (fused-name-2 fused-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-fused-red
        fused-mid)))
    (check-equal? (~a seq-name-1) "disj/reassociate-left-answer")
    (check-equal? (~a seq-name-2) "disj/promote-left-answer")
    (check-equal? (~a fused-name-1) "disj/reassociate-left-answer")
    (check-equal? (~a fused-name-2) "disj/promote-left-answer")
    (check-equal? seq-mid
                  (term (Bounced ((⊤ ,sigma-a)
                                  <-+
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-equal? fused-mid
                  (term (Bounced ((⊤ ,sigma-a)
                                  <-+
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-false
     (member bad-bounced-promotion
             (map tagged-successor-cfg
                  (apply-reduction-relation/tag-with-names
                   red:search-base-seq-red
                   bounced-branch))))
    (check-false
     (member bad-bounced-promotion
             (map tagged-successor-cfg
                  (apply-reduction-relation/tag-with-names
                   red:search-base-fused-red
                   bounced-branch))))
    (check-equal? seq-next
                  (term (Bounced ((⊤ ,sigma-a)
                                  +
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-equal? fused-next
                  (term (Bounced ((⊤ ,sigma-a)
                                  +
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-true (produced-answer-spine-only? seq-next))
    (check-true (produced-answer-spine-only? fused-next)))

  (test-case "canonical JSON preserves bounced observables under Freshened prefixes"
    (define rendered
      (cfg->operational-picture
       (term (() (FreshenedShell
                  (u:0)
                  (Bounced (empty-tree))
                  (label "fresh"))))))
    (check-equal? (hash-ref rendered 'name) "Freshened")
    (check-equal? (hash-ref rendered 'id) "fresh")
    (define child (first (hash-ref rendered 'children)))
    (check-equal? (hash-ref child 'name) "Bounced"))

  (test-case "extensional pictures erase bounced nodes while operational pictures keep them"
    (define cfg
      (term (() (FreshenedShell
                 (u:0)
                 (Bounced (empty-tree))
                 (label "fresh")))))
    (define operational (cfg->operational-picture cfg))
    (define extensional (cfg->extensional-picture cfg))
    (check-equal? (hash-ref operational 'name) "Freshened")
    (check-equal? (hash-ref (first (hash-ref operational 'children)) 'name) "Bounced")
    (check-equal? (hash-ref extensional 'name) "Freshened")
    (check-equal? (hash-ref (first (hash-ref extensional 'children)) 'name) "Empty"))

  (test-case "summary judgments expose answer, bounced, and freshening counts"
    (define sigma-u0
      (term (state () () (u:0) () (label "su0"))))
    (define core-summary
      (first
       (judgment-holds
        (wf:wf-summary-cfg/core?
         ,(term (FreshenedTree (u:0)
                               (⊤ ,sigma-u0)
                               (label "fresh")))
         summary)
        summary)))
    (define delay-summary
      (first
       (judgment-holds
        (wf:wf-summary-cfg/delay?
         ,(term (FreshenedShell
                 (u:0)
                 (Bounced (⊤ ,sigma-u0))
                 (label "fresh")))
         summary)
        summary)))
    (define disj-summary
      (first
       (judgment-holds
        (wf:wf-summary-cfg/disj?
         ,(term ((⊤ ,sigma-a) + ((⊤ ,sigma-b) + (empty-tree))))
         summary)
        summary)))
    (check-equal? core-summary '(wf-summary 1 0 1 0))
    (check-equal? delay-summary '(wf-summary 1 1 0 1))
    (check-equal? disj-summary '(wf-summary 2 0 0 0)))

  (test-case "search-only scheduler variants differ only in delayed left-branch policy"
      (for ([entry (in-list
                  (list
                   (list red:search-dfs-seq-red
                         "search-dfs-seq/delay-through-left"
                         (term (delay (((succeed (label "late")) ,sigma-s)
                                       <-+
                                       (⊤ ,sigma-b)))))
                   (list red:search-dfs-fused-red
                         "search-dfs-fused/delay-through-left"
                         (term (delay (((succeed (label "late")) ,sigma-s)
                                       <-+
                                       (⊤ ,sigma-b)))))
                   (list red:search-flip-seq-red
                         "search-flip-seq/delay-swap-left"
                         (term (delay ((⊤ ,sigma-b)
                                       <-+
                                       ((succeed (label "late")) ,sigma-s)))))
                   (list red:search-flip-fused-red
                         "search-flip-fused/delay-swap-left"
                         (term (delay ((⊤ ,sigma-b)
                                       <-+
                                       ((succeed (label "late")) ,sigma-s)))))))])
      (match-define (list rel expected-name expected-template) entry)
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-flip)))
      (check-equal? (~a step-name) expected-name)
      (check-equal? next expected-template)))

  (test-case "rail search-only branches enter the railroad from delayed left disjunction"
    (for ([entry (in-list
                  (list (list red:rail-seq-red "rail-seq/enter-right")
                        (list red:rail-fused-red "rail-fused/enter-right")))])
      (match-define (list rel expected-name) entry)
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-rail)))
      (check-equal? (~a step-name) expected-name)
      (check-true (redex-match? lang:rail-lang cfg next))))

  (test-case "rail seq continues reducing right-branch work after invoke-delay"
    (define cfg-delayed-right-work
      (term ((delay ((u:0 =? (sym "later") (label "later")) ,sigma-s))
             <-+
             ((u:0 =? (sym "now") (label "now")) ,sigma-s))))
    (define-values (enter-name enter-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-seq-red
        cfg-delayed-right-work)))
    (check-equal? (~a enter-name) "rail-seq/enter-right")
    (define-values (invoke-name invoke-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-seq-red
        enter-next)))
    (check-equal? (~a invoke-name) "delay/invoke-delay")
    (define-values (resume-name resume-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-seq-red
        invoke-next)))
    (check-equal? (~a resume-name) "core/unify-success")
    (check-true (redex-match? lang:rail-lang cfg resume-next)))

  (test-case "rail promotes bare right-branch answers and forbids branch-internal +"
    (define-values (seq-name seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-seq-red
        (term ((empty-tree) +-> (⊤ ,sigma-b))))))
    (define-values (fused-name fused-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-fused-red
        (term ((empty-tree) +-> (⊤ ,sigma-b))))))
    (check-false
     (redex-match?
      lang:rail-lang
      cfg
      (term ((empty-tree) +-> ((⊤ ,sigma-b) + (empty-tree))))))
    (check-equal? (~a seq-name) "rail-seq/promote-right-observable")
    (check-equal? (~a fused-name) "rail-fused/promote-right-observable")
    (check-true (produced-answer-spine-only? seq-next))
    (check-true (produced-answer-spine-only? fused-next))
    (check-true (redex-match? lang:rail-lang cfg seq-next))
    (check-true (redex-match? lang:rail-lang cfg fused-next)))

  (test-case "calls overlay expands relcalls once and still omits proceed"
    (define-values (step-name next)
      (named-step (apply-reduction-relation/tag-with-names red:calls-red cfg-call)))
    (check-equal? (~a step-name) "calls/expand")
    (check-false (redex-match? lang:calls-lang cfg '(proceed (empty-tree))))
    (check-true (redex-match? lang:calls-lang config next)))

  (test-case "search-base +calls branches expand inside their chosen search discipline"
    (define-values (seq-name seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-seq-calls-red
        cfg-call-branch)))
    (define-values (fused-name fused-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-base-fused-calls-red
        cfg-call-branch)))
    (check-equal? (~a seq-name) "search-base-seq-calls/expand")
    (check-equal? (~a fused-name) "search-base-fused-calls/expand")
    (check-true (redex-match? lang:search-base-calls-lang config seq-next))
    (check-true (redex-match? lang:search-base-calls-lang config fused-next)))

  (test-case "scheduled +calls reducers are deterministic and shape-closed"
    (for ([entry (in-list
                  (list (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-dfs-seq-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-dfs-fused-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-flip-seq-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-flip-fused-calls-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:rail-calls-lang config prog))
                              red:rail-seq-calls-red
                              cfg-call-rail)
                        (list (lambda (prog) (redex-match? lang:rail-calls-lang config prog))
                              red:rail-fused-calls-red
                              cfg-call-rail)))])
      (match-define (list matcher rel prog) entry)
      (check-true (progress? rel prog))
      (check-true (unique-decomposition? rel prog))
      (check-true (states-wf? prog))
      (check-true (shape-closed? matcher rel prog))
      (check-true (invariant-closed? produced-answer-spine-only? rel prog))))

  (test-case "scheduler/calls assembly commutes on representative seq and fused examples"
    (define alt-search-dfs-seq-calls-expand
       (reduction-relation
       lang:search-base-calls-lang
       #:domain config
       [--> (Γ (in-hole QShell (in-hole KBranch (in-hole KLocal ((r t ... tag) σ)))))
            (Γ (in-hole QShell (in-hole KBranch (in-hole KLocal (g_new σ)))))
            (where g_new
                   ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
            "alt-search-dfs-seq-calls/expand"]))
    (define alt-search-dfs-seq-calls-red
      (union-reduction-relations
       (context-closure
        (extend-reduction-relation red:search-dfs-seq-red lang:search-base-calls-lang)
        lang:search-base-calls-lang
        (Γ hole))
       alt-search-dfs-seq-calls-expand))
    (define alt-rail-fused-calls-expand
       (reduction-relation
       lang:rail-calls-lang
       #:domain config
       [--> (Γ (in-hole QShell (in-hole KLate (in-hole KLocal ((r t ... tag) σ)))))
            (Γ (in-hole QShell (in-hole KLate (in-hole KLocal (g_new σ)))))
            (where g_new
                   ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
            "alt-rail-fused-calls/expand"]))
    (define alt-rail-fused-calls-red
      (union-reduction-relations
       (context-closure
        (extend-reduction-relation red:rail-fused-red lang:rail-calls-lang)
        lang:rail-calls-lang
        (Γ hole))
       alt-rail-fused-calls-expand))
    (check-equal?
     (apply-reduction-relation red:search-dfs-seq-calls-red cfg-call-branch)
     (apply-reduction-relation alt-search-dfs-seq-calls-red cfg-call-branch))
    (check-equal?
     (apply-reduction-relation red:rail-fused-calls-red cfg-call-rail)
     (apply-reduction-relation alt-rail-fused-calls-red cfg-call-rail)))

  (test-case "progress, determinism, state wf, and shape closure hold across the internal lattice"
    (for ([entry (in-list
                  (list (list (lambda (prog) (redex-match? lang:core-lang search prog))
                              red:core-red
                              (term ((succeed (label "ok")) ,sigma-a)))
                        (list (lambda (prog) (redex-match? lang:delay-lang cfg prog))
                              red:delay-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:disj-lang cfg prog))
                              red:disj-seq-red cfg-mixed-answer)
                        (list (lambda (prog) (redex-match? lang:disj-lang cfg prog))
                              red:disj-fused-red cfg-mixed-answer)
                        (list (lambda (prog) (redex-match? lang:search-base-lang cfg prog))
                              red:search-base-seq-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:search-base-lang cfg prog))
                              red:search-base-fused-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:search-base-lang cfg prog))
                              red:search-dfs-seq-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-base-lang cfg prog))
                              red:search-dfs-fused-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-base-lang cfg prog))
                              red:search-flip-seq-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-base-lang cfg prog))
                              red:search-flip-fused-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:rail-lang cfg prog))
                              red:rail-seq-red cfg-rail)
                        (list (lambda (prog) (redex-match? lang:rail-lang cfg prog))
                              red:rail-fused-red cfg-rail)
                        (list (lambda (prog) (redex-match? lang:calls-lang config prog))
                              red:calls-red cfg-call)
                        (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-dfs-seq-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-dfs-fused-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-flip-seq-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-base-calls-lang config prog))
                              red:search-flip-fused-calls-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:rail-calls-lang config prog))
                              red:rail-seq-calls-red cfg-call-rail)
                        (list (lambda (prog) (redex-match? lang:rail-calls-lang config prog))
                              red:rail-fused-calls-red cfg-call-rail)))])
      (match-define (list matcher rel prog) entry)
      (check-true (progress? rel prog))
      (check-true (unique-decomposition? rel prog))
      (check-true (states-wf? prog))
      (check-true (shape-closed? matcher rel prog))
      (check-true (invariant-closed? produced-answer-spine-only? rel prog))))

  (test-case "WF judgments align with the new search-only and calls split"
    (check-true
     (judgment-holds
      (wf:wf-cfg/core? (⊤ ,sigma-a))))
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
       (,delayed-left-search +-> (⊤ ,sigma-b)))))
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
        (,delayed-left-search +-> (⊤ ,sigma-b)))))))
  )

(module+ test
  (run-tests SEARCH-LATTICE))
