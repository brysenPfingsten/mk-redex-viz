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
    (check-true (redex-match? lang:core-lang FreshCtx (term (ScopedTree (u:0) hole (label "fresh")))))
    (check-false (redex-match? lang:core-lang FreshCtx+ (term hole)))
    (check-true (redex-match? lang:core-lang FreshCtx+ (term (ScopedTree () hole (label "fresh")))))
    (check-false (redex-match? lang:delay-lang cfg '(delay (empty-tree))))
    (check-true (redex-match? lang:delay-lang cfg (term ,delayed-left-search)))
    (check-false (redex-match? lang:delay-lang cfg '(proceed (empty-tree))))
    (check-true (redex-match? lang:relcall-lang g '(r:delay (label "call"))))
    (check-true (redex-match? lang:disj-lang BranchCtx (term (hole <-+ (empty-tree)))))
    (check-true
     (redex-match?
      lang:disj-lang
      LateCtx
      (term (hole × (succeed (label "k")) ()))))
    (check-true (redex-match? lang:rail-lang cfg '((empty-tree) +-> (empty-tree))))
    (check-true
     (redex-match?
      lang:rail-lang
      BranchCtx
      (term ((empty-tree) +-> hole))))
    (check-false
     (redex-match?
      lang:core-lang
      search
      (term (((⊤ ,sigma-a) + (empty-tree))
             × (succeed (label "k"))
             ()))))
    (check-true (redex-match? lang:relcall-lang config (term ,cfg-call))))

  (test-case "disj-early distributes immediately while disj-late keeps mixed states"
    (define pending-disj
      (term ((((succeed (label "left")) ,sigma-s)
              <-+
              ((succeed (label "right")) ,sigma-s))
             × (succeed (label "k"))
             ())))
    (define-values (early-name _seq-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-early-red
        pending-disj)))
    (define-values (late-pending-name _fused-pending-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-late-red
        pending-disj)))
    (define-values (late-answer-name _fused-answer-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-late-red
        cfg-mixed-answer)))
    (define-values (late-fail-name _fused-fail-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-late-red
        cfg-mixed-fail)))
    (check-equal? (~a early-name) "distribute-over-conj")
    (check-equal? (~a late-pending-name) "succeed")
    (check-equal? (~a late-answer-name) "distribute-over-conj")
    (check-equal? (~a late-fail-name) "distribute-over-conj"))

  (test-case "disj-late distributes freshened answers before inherited consequence steps"
    (define freshened-answer
      (term (((ScopedTree (u:0) (⊤ ,sigma-a) (label "fresh")) <-+ (⊤ ,sigma-b))
             × (succeed (label "k"))
             ())))
    (define-values (step-name next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-late-red
        freshened-answer)))
    (check-equal? (~a step-name) "distribute-over-conj")
    (check-equal? next
                  (term (((ScopedTree (u:0)
                                     (⊤ ,sigma-a)
                                     (label "fresh"))
                          × (succeed (label "k"))
                          ())
                         <-+
                         ((⊤ ,sigma-b) × (succeed (label "k")) ())))))

  (test-case "search search-only branches handle explicit delay with no relcalls"
    (for ([rel (in-list (list red:search-early-red
                              red:search-late-red))])
      (define-values (step1-name step1)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-delay-goal)))
      (define-values (step2-name _step2)
        (named-step (apply-reduction-relation/tag-with-names rel step1)))
      (check-equal? (~a step1-name) "suspend-goal")
      (check-equal? (~a step2-name) "invoke-delay")))

  (test-case "scope-sensitive delay roots preserve FreshCtx outside and inside suspend"
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
    (check-equal? (~a fresh-step-1-name) "fresh-substitute")
    (check-equal? (~a fresh-step-2-name) "suspend-goal")
    (check-equal? (~a fresh-step-3-name) "invoke-delay")
    (check-equal? fresh-step-2
                  (term (ScopedTree (u:0)
                                       (delay (,scoped-eq ,scoped-state))
                                       (label "fresh"))))
    (check-equal? fresh-step-3
                  (term (ScopedShell (u:0)
                                        (Deferred (,scoped-eq ,scoped-state))
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
    (check-equal? (~a suspend-step-1-name) "suspend-goal")
    (check-equal? (~a suspend-step-2-name) "invoke-delay")
    (check-equal? (~a suspend-step-3-name) "fresh-substitute")
    (check-equal? suspend-step-1
                  (term (delay ((∃ (x:0)
                                   ,uninstantiated-eq
                                   (label "fresh"))
                                ,sigma-s))))
    (check-equal? suspend-step-2
                  (term (Deferred ((∃ (x:0)
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
    (check-equal? (~a step-1-name) "fresh-substitute")
    (check-equal? (~a step-2-name) "succeed")
    (check-equal? (~a step-3-name) "finish-answer")
    (check-equal? step-1
                  (term (ScopedTree ()
                                       ((succeed (label "inner")) ,sigma-s)
                                       (label "fresh-empty"))))
    (check-equal? step-2
                  (term (ScopedTree ()
                                       (⊤ ,sigma-s)
                                       (label "fresh-empty"))))
    (check-equal? step-3
                  (term (ScopedShell ()
                                        (⊤ ,sigma-s)
                                        (label "fresh-empty")))))

  (test-case "core shellification only fires on real fresh prefixes"
    (check-equal?
     (apply-reduction-relation/tag-with-names
      red:core-red
      (term (⊤ ,sigma-s)))
     '())
    (check-equal?
     (apply-reduction-relation/tag-with-names
      red:core-red
      (term (empty-tree)))
     '())
    (define fresh-answer
      (term (ScopedTree (u:0)
                           (⊤ ,sigma-s)
                           (label "fresh-answer"))))
    (define fresh-fail
      (term (ScopedTree (u:0)
                           (empty-tree)
                           (label "fresh-fail"))))
    (define-values (answer-name answer-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:core-red
        fresh-answer)))
    (define-values (fail-name fail-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:core-red
        fresh-fail)))
    (check-equal? (~a answer-name) "finish-answer")
    (check-equal? (~a fail-name) "finish-fail")
    (check-equal? answer-next
                  (term (ScopedShell (u:0)
                                        (⊤ ,sigma-s)
                                        (label "fresh-answer"))))
    (check-equal? fail-next
                  (term (ScopedShell (u:0)
                                        (empty-tree)
                                        (label "fresh-fail")))))

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
    (check-equal? (~a step-1-name) "fresh-substitute")
    (check-equal? (~a step-2-name) "fresh-substitute")
    (check-equal? (~a step-3-name) "fresh-substitute")
    (check-equal? step-3
                  (term (ScopedTree (u:0)
                                       (ScopedTree ()
                                                       (ScopedTree (u:1)
                                                                       ((succeed (label "ok"))
                                                                        (state () () (u:1 u:0) () (label "s")))
                                                                       (label "fy"))
                                                       (label "fempty"))
                                       (label "fx")))))

  (test-case "scoped delay-floating keeps subtree-local FreshCtx on the payload"
    (define scoped-conj-expected
      (term (delay ((ScopedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))
                    × (succeed (label "k"))
                    ()))))
    (define scoped-dfs-expected
      (term (delay ((ScopedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))
                    <-+
                    (⊤ ,sigma-b)))))
    (define scoped-flip-expected
      (term (delay ((⊤ ,sigma-b)
                    <-+
                    (ScopedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))))))
    (define scoped-rail-expected
      (term (delay ((ScopedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))
                    +-> (⊤ ,sigma-b)))))
    (define scoped-return-rail
      (term ((⊤ ,sigma-b) +-> ,scoped-delayed-left-search)))
    (define scoped-return-expected
      (term (delay ((⊤ ,sigma-b)
                    <-+
                    (ScopedTree (u:0)
                                   ((succeed (label "late")) ,sigma-s)
                                   (label "fresh"))))))
    (define-values (conj-name conj-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:delay-red
        cfg-scoped-delay-through-conj)))
    (check-equal? (~a conj-name) "delay-through-conj")
    (check-equal? conj-next scoped-conj-expected)
    (for ([entry (in-list
                  (list (list red:search-dfs-early-red
                              cfg-scoped-flip
                              "delay-through-left"
                              scoped-dfs-expected)
                        (list red:search-dfs-late-red
                              cfg-scoped-flip
                              "delay-through-left"
                              scoped-dfs-expected)
                        (list red:search-flip-early-red
                              cfg-scoped-flip
                              "delay-swap-left"
                              scoped-flip-expected)
                        (list red:search-flip-late-red
                              cfg-scoped-flip
                              "delay-swap-left"
                              scoped-flip-expected)
                        (list red:rail-early-red
                              cfg-scoped-rail
                              "enter-right"
                              scoped-rail-expected)
                        (list red:rail-late-red
                              cfg-scoped-rail
                              "enter-right"
                              scoped-rail-expected)
                        (list red:rail-early-red
                              scoped-return-rail
                              "return-left"
                              scoped-return-expected)
                        (list red:rail-late-red
                              scoped-return-rail
                              "return-left"
                              scoped-return-expected)))])
      (match-define (list rel cfg expected-name expected-next) entry)
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel cfg)))
      (check-equal? (~a step-name) expected-name)
      (check-equal? next expected-next)))

  (test-case "search promotes bare answers and forbids buried +"
    (define-values (early-name early-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-early-red
        cfg-disj)))
    (define-values (late-name late-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-late-red
        cfg-disj)))
    (define illegal-prefix-conj
      (term (((⊤ ,sigma-a) + (empty-tree))
             × (succeed (label "k"))
             ())))
    (check-false
     (redex-match?
      lang:search-lang
      cfg
      illegal-prefix-conj))
    (check-false
     (redex-match?
      lang:search-lang
      cfg
      (term (((⊤ ,sigma-a) + (empty-tree)) <-+ (⊤ ,sigma-b)))))
    (check-equal? (~a early-name) "promote-left-answer")
    (check-equal? (~a late-name) "promote-left-answer")
    (check-true (produced-answer-spine-only? early-next))
    (check-true (produced-answer-spine-only? late-next))
    (check-true (redex-match? lang:search-lang cfg early-next))
    (check-true (redex-match? lang:search-lang cfg late-next)))

  (test-case "neutral disjunction keeps answers answers tree-freshened and shellifies skipped fails"
    (define fresh-answer
      (term ((ScopedTree (u:0)
                            (⊤ ,sigma-a)
                            (label "fresh-answer"))
             <-+
             (empty-tree))))
    (define fresh-fail
      (term (ScopedTree (u:0)
                           ((empty-tree) <-+ (⊤ ,sigma-b))
                           (label "fresh-fail"))))
    (define-values (answer-name answer-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-early-red
        fresh-answer)))
    (define-values (fail-name fail-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:disj-early-red
        fresh-fail)))
    (check-equal? (~a answer-name) "promote-left-answer")
    (check-equal? (~a fail-name) "skip-left-fail")
    (check-equal? answer-next
                  (term ((ScopedTree (u:0)
                                        (⊤ ,sigma-a)
                                        (label "fresh-answer"))
                         +
                         (empty-tree))))
    (define sigma-u0
      (term (state () () (u:0) () (label "fresh-state"))))
    (check-equal?
     (first
      (judgment-holds
       (wf:wf-summary-cfg/disj?
        ,(term ((ScopedTree (u:0)
                               (⊤ ,sigma-u0)
                               (label "fresh-answer"))
                +
                (empty-tree)))
        summary)
       summary))
     '(wf-summary 1 0 1 0))
    (check-equal? fail-next
                  (term (ScopedShell (u:0)
                                        (⊤ ,sigma-b)
                                        (label "fresh-fail")))))

  (test-case "search reassociates then closes bounced segments when an answer appears"
    (define bounced-branch
      (term (Deferred (((⊤ ,sigma-a) <-+ (empty-tree))
                      <-+
                      (⊤ ,sigma-b)))))
    (define bad-bounced-promotion
      (term (Deferred ((((⊤ ,sigma-a) + (empty-tree))
                       <-+
                       (⊤ ,sigma-b))))))
    (define-values (early-name-1 early-mid)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-early-red
        bounced-branch)))
    (define-values (early-name-2 early-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-early-red
        early-mid)))
    (define-values (late-name-1 late-mid)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-late-red
        bounced-branch)))
    (define-values (late-name-2 late-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-late-red
        late-mid)))
    (check-equal? (~a early-name-1) "reassociate-left-result")
    (check-equal? (~a early-name-2) "promote-left-answer")
    (check-equal? (~a late-name-1) "reassociate-left-result")
    (check-equal? (~a late-name-2) "promote-left-answer")
    (check-equal? early-mid
                  (term (Deferred ((⊤ ,sigma-a)
                                  <-+
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-equal? late-mid
                  (term (Deferred ((⊤ ,sigma-a)
                                  <-+
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-false
     (member bad-bounced-promotion
             (map tagged-successor-cfg
                  (apply-reduction-relation/tag-with-names
                   red:search-early-red
                   bounced-branch))))
    (check-false
     (member bad-bounced-promotion
             (map tagged-successor-cfg
                  (apply-reduction-relation/tag-with-names
                   red:search-late-red
                   bounced-branch))))
    (check-equal? early-next
                  (term (Deferred ((⊤ ,sigma-a)
                                  +
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-equal? late-next
                  (term (Deferred ((⊤ ,sigma-a)
                                  +
                                  ((empty-tree) <-+ (⊤ ,sigma-b))))))
    (check-true (produced-answer-spine-only? early-next))
    (check-true (produced-answer-spine-only? late-next)))

  (test-case "canonical JSON preserves bounced observables under Freshened prefixes"
    (define rendered
      (cfg->operational-picture
       (term (() (ScopedShell
                  (u:0)
                  (Deferred (empty-tree))
                  (label "fresh"))))))
    (check-equal? (hash-ref rendered 'name) "Freshened")
    (check-equal? (hash-ref rendered 'id) "fresh")
    (define child (first (hash-ref rendered 'children)))
    (check-equal? (hash-ref child 'name) "Deferred"))

  (test-case "extensional pictures erase bounced nodes while operational pictures keep them"
    (define cfg
      (term (() (ScopedShell
                 (u:0)
                 (Deferred (empty-tree))
                 (label "fresh")))))
    (define operational (cfg->operational-picture cfg))
    (define extensional (cfg->extensional-picture cfg))
    (check-equal? (hash-ref operational 'name) "Freshened")
    (check-equal? (hash-ref (first (hash-ref operational 'children)) 'name) "Deferred")
    (check-equal? (hash-ref extensional 'name) "Freshened")
    (check-equal? (hash-ref (first (hash-ref extensional 'children)) 'name) "Empty"))

  (test-case "summary judgments expose answer, bounced, and freshening counts"
    (define sigma-u0
      (term (state () () (u:0) () (label "su0"))))
    (define core-summary
      (first
       (judgment-holds
        (wf:wf-summary-cfg/core?
         ,(term (ScopedTree (u:0)
                               (⊤ ,sigma-u0)
                               (label "fresh")))
         summary)
        summary)))
    (define delay-summary
      (first
       (judgment-holds
        (wf:wf-summary-cfg/delay?
         ,(term (ScopedShell
                 (u:0)
                 (Deferred (⊤ ,sigma-u0))
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
                   (list red:search-dfs-early-red
                         "delay-through-left"
                         (term (delay (((succeed (label "late")) ,sigma-s)
                                       <-+
                                       (⊤ ,sigma-b)))))
                   (list red:search-dfs-late-red
                         "delay-through-left"
                         (term (delay (((succeed (label "late")) ,sigma-s)
                                       <-+
                                       (⊤ ,sigma-b)))))
                   (list red:search-flip-early-red
                         "delay-swap-left"
                         (term (delay ((⊤ ,sigma-b)
                                       <-+
                                       ((succeed (label "late")) ,sigma-s)))))
                   (list red:search-flip-late-red
                         "delay-swap-left"
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
                  (list (list red:rail-early-red "enter-right")
                        (list red:rail-late-red "enter-right")))])
      (match-define (list rel expected-name) entry)
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel cfg-rail)))
      (check-equal? (~a step-name) expected-name)
      (check-true (redex-match? lang:rail-lang cfg next))))

  (test-case "rail early continues reducing right-branch work after invoke-delay"
    (define cfg-delayed-right-work
      (term ((delay ((u:0 =? (sym "later") (label "later")) ,sigma-s))
             <-+
             ((u:0 =? (sym "now") (label "now")) ,sigma-s))))
    (define-values (enter-name enter-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-early-red
        cfg-delayed-right-work)))
    (check-equal? (~a enter-name) "enter-right")
    (define-values (invoke-name invoke-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-early-red
        enter-next)))
    (check-equal? (~a invoke-name) "invoke-delay")
    (define-values (resume-name resume-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-early-red
        invoke-next)))
    (check-equal? (~a resume-name) "unify-success")
    (check-true (redex-match? lang:rail-lang cfg resume-next)))

  (test-case "rail promotes bare right-branch answers and forbids branch-internal +"
    (define-values (early-name early-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-early-red
        (term ((empty-tree) +-> (⊤ ,sigma-b))))))
    (define-values (late-name late-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:rail-late-red
        (term ((empty-tree) +-> (⊤ ,sigma-b))))))
    (check-false
     (redex-match?
      lang:rail-lang
      cfg
      (term ((empty-tree) +-> ((⊤ ,sigma-b) + (empty-tree))))))
    (check-equal? (~a early-name) "promote-right-answer")
    (check-equal? (~a late-name) "promote-right-answer")
    (check-true (produced-answer-spine-only? early-next))
    (check-true (produced-answer-spine-only? late-next))
    (check-true (redex-match? lang:rail-lang cfg early-next))
    (check-true (redex-match? lang:rail-lang cfg late-next)))

  (test-case "relcall overlay expands relcalls once and still omits proceed"
    (define-values (step-name next)
      (named-step (apply-reduction-relation/tag-with-names red:relcall-red cfg-call)))
    (check-equal? (~a step-name) "expand-relcall")
    (check-false (redex-match? lang:relcall-lang cfg '(proceed (empty-tree))))
    (check-true (redex-match? lang:relcall-lang config next)))

  (test-case "search +relcall branches expand inside their chosen search discipline"
    (define-values (early-name early-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-early-relcall-red
        cfg-call-branch)))
    (define-values (late-name late-next)
      (named-step
       (apply-reduction-relation/tag-with-names
        red:search-late-relcall-red
        cfg-call-branch)))
    (check-equal? (~a early-name) "expand-relcall")
    (check-equal? (~a late-name) "expand-relcall")
    (check-true (redex-match? lang:search-relcall-lang config early-next))
    (check-true (redex-match? lang:search-relcall-lang config late-next)))

  (test-case "scheduled +relcall reducers are deterministic and shape-closed"
    (for ([entry (in-list
                  (list (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-dfs-early-relcall-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-dfs-late-relcall-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-flip-early-relcall-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-flip-late-relcall-red
                              cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:rail-relcall-lang config prog))
                              red:rail-early-relcall-red
                              cfg-call-rail)
                        (list (lambda (prog) (redex-match? lang:rail-relcall-lang config prog))
                              red:rail-late-relcall-red
                              cfg-call-rail)))])
      (match-define (list matcher rel prog) entry)
      (check-true (progress? rel prog))
      (check-true (unique-decomposition? rel prog))
      (check-true (states-wf? prog))
      (check-true (shape-closed? matcher rel prog))
      (check-true (invariant-closed? produced-answer-spine-only? rel prog))))

  (test-case "scheduler/relcall assembly commutes on representative early and late examples"
    (define alt-search-dfs-early-relcall-expand
       (reduction-relation
       lang:search-relcall-lang
       #:domain config
       [--> (Γ (in-hole ShellCtx (in-hole BranchCtx (in-hole LocalCtx ((r t ... tag) σ)))))
            (Γ (in-hole ShellCtx (in-hole BranchCtx (in-hole LocalCtx (g_new σ)))))
            (where g_new
                   ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
            "expand-relcall"]))
    (define alt-search-dfs-early-relcall-red
      (union-reduction-relations
       (context-closure
        (extend-reduction-relation red:search-dfs-early-red lang:search-relcall-lang)
        lang:search-relcall-lang
        (Γ hole))
       alt-search-dfs-early-relcall-expand))
    (define alt-rail-late-relcall-expand
       (reduction-relation
       lang:rail-relcall-lang
       #:domain config
       [--> (Γ (in-hole ShellCtx (in-hole LateCtx (in-hole LocalCtx ((r t ... tag) σ)))))
            (Γ (in-hole ShellCtx (in-hole LateCtx (in-hole LocalCtx (g_new σ)))))
            (where g_new
                   ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
            "expand-relcall"]))
    (define alt-rail-late-relcall-red
      (union-reduction-relations
       (context-closure
        (extend-reduction-relation red:rail-late-red lang:rail-relcall-lang)
        lang:rail-relcall-lang
        (Γ hole))
       alt-rail-late-relcall-expand))
    (check-equal?
     (apply-reduction-relation red:search-dfs-early-relcall-red cfg-call-branch)
     (apply-reduction-relation alt-search-dfs-early-relcall-red cfg-call-branch))
    (check-equal?
     (apply-reduction-relation red:rail-late-relcall-red cfg-call-rail)
     (apply-reduction-relation alt-rail-late-relcall-red cfg-call-rail)))

  (test-case "progress, determinism, state wf, and shape closure hold across the internal lattice"
    (for ([entry (in-list
                  (list (list (lambda (prog) (redex-match? lang:core-lang search prog))
                              red:core-red
                              (term ((succeed (label "ok")) ,sigma-a)))
                        (list (lambda (prog) (redex-match? lang:delay-lang cfg prog))
                              red:delay-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:disj-lang cfg prog))
                              red:disj-early-red cfg-mixed-answer)
                        (list (lambda (prog) (redex-match? lang:disj-lang cfg prog))
                              red:disj-late-red cfg-mixed-answer)
                        (list (lambda (prog) (redex-match? lang:search-lang cfg prog))
                              red:search-early-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:search-lang cfg prog))
                              red:search-late-red cfg-delay-goal)
                        (list (lambda (prog) (redex-match? lang:search-lang cfg prog))
                              red:search-dfs-early-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-lang cfg prog))
                              red:search-dfs-late-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-lang cfg prog))
                              red:search-flip-early-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:search-lang cfg prog))
                              red:search-flip-late-red cfg-flip)
                        (list (lambda (prog) (redex-match? lang:rail-lang cfg prog))
                              red:rail-early-red cfg-rail)
                        (list (lambda (prog) (redex-match? lang:rail-lang cfg prog))
                              red:rail-late-red cfg-rail)
                        (list (lambda (prog) (redex-match? lang:relcall-lang config prog))
                              red:relcall-red cfg-call)
                        (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-dfs-early-relcall-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-dfs-late-relcall-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-flip-early-relcall-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:search-relcall-lang config prog))
                              red:search-flip-late-relcall-red cfg-call-branch)
                        (list (lambda (prog) (redex-match? lang:rail-relcall-lang config prog))
                              red:rail-early-relcall-red cfg-call-rail)
                        (list (lambda (prog) (redex-match? lang:rail-relcall-lang config prog))
                              red:rail-late-relcall-red cfg-call-rail)))])
      (match-define (list matcher rel prog) entry)
      (check-true (progress? rel prog))
      (check-true (unique-decomposition? rel prog))
      (check-true (states-wf? prog))
      (check-true (shape-closed? matcher rel prog))
      (check-true (invariant-closed? produced-answer-spine-only? rel prog))))

  (test-case "WF judgments align with the new search-only and relcall split"
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
      (wf:wf-cfg/search? ,cfg-flip)))
    (check-true
     (judgment-holds
      (wf:wf-cfg/rail?
       (,delayed-left-search +-> (⊤ ,sigma-b)))))
    (check-true
     (judgment-holds
      (wf:wf-config/relcall? ,cfg-call)))
    (check-true
     (judgment-holds
      (wf:wf-config/search-relcall? ,cfg-call-branch)))
    (check-true
     (judgment-holds
      (wf:wf-config/rail-relcall?
       (,gamma-delay
        (,delayed-left-search +-> (⊤ ,sigma-b)))))))
  )

(module+ test
  (run-tests SEARCH-LATTICE))
