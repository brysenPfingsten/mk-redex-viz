#lang racket

(require racket/list
         rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in lang:
                    "../dormant-branch-semantics/source/languages/all.rkt")
         (prefix-in distributed:
                    "all.rkt")
         (prefix-in factored:
                    "../dormant-branch-semantics/source/reduction-relations/all.rkt")
         (prefix-in wf:
                    "../dormant-branch-semantics/source/wf/all.rkt")
         "../dormant-branch-semantics/source/reduction-relations/private/common.rkt"
         "../dormant-branch-semantics/source/structural-observations.rkt"
         (prefix-in strict: "../../s-reference/source.rkt")
         (only-in "../../shared/wf.rkt" wf-s?)
         "../dormant-branch-semantics/tests/frontier-observable-support.rkt"
         "../dormant-branch-semantics/tests/search-lattice-support.rkt")

(provide DISTRIBUTED-PRESENTATION)

(define TRACE-CAP 64)

(define CORE-RULES
  '(expand-conjunction
    succeed
    fail
    conj-return
    conj-fail
    allocate-fresh
    unify-success
    unify-violates-disequality
    unify-fail
    disequality-success
    disequality-fail
    finish-success
    finish-failure))

(define DELAY-RULES
  '(suspend-goal
    bubble-delay-through-conj
    force-delay))

(define DISJUNCTION-RULES
  '(expand-disjunction
    skip-left-failure
    reassociate-left-result
    commit-choice-answer))

(define SEARCH-JOIN-RULES
  '(skip-right-failure
    reassociate-left-result/search-join
    reassociate-right-result/left-nested
    reassociate-right-result/right-nested
    commit-right-choice-answer))

(define (sort-symbols names)
  (sort names symbol<?))

(define (rule-name-union . name-lists)
  (sort-symbols (remove-duplicates (append* name-lists))))

(define (check-rule-inventory label relation expected)
  (define actual
    (reduction-relation->rule-names relation))
  (check-equal? (length actual)
                (length (remove-duplicates actual))
                (format "~a repeats a static source-rule name" label))
  (check-equal? (sort-symbols actual)
                (sort-symbols expected)
                (format "~a static source-rule inventory drifted" label)))

(define (count-step-name steps expected [count 0])
  (match steps
    ['() count]
    [(cons step-name rest)
     (count-step-name rest
                      expected
                      (if (string=? step-name expected)
                          (add1 count)
                          count))]))

(struct step-witness
  (name source target)
  #:transparent)

(define (check-exact-step relation witness)
  (define raw-successors
    (apply-reduction-relation/tag-with-names
     relation
     (step-witness-source witness)))
  (check-equal? (length raw-successors)
                1
                (format "expected one raw ~a proof for ~s, got ~s"
                        (step-witness-name witness)
                        (step-witness-source witness)
                        raw-successors))
  (when (pair? raw-successors)
    (define successor
      (first raw-successors))
    (check-equal? (tagged-successor-name successor)
                  (step-witness-name witness))
    (check-equal? (tagged-successor-cfg successor)
                  (step-witness-target witness))))

(define (check-exact-domain-step relation wf? shape? witness)
  (define source
    (step-witness-source witness))
  (define target
    (step-witness-target witness))
  (check-true (wf? source))
  (check-true (shape? source))
  (check-true (produced-answer-spine-only? source))
  (check-exact-step relation witness)
  (check-true (wf? target))
  (check-true (shape? target))
  (check-true (produced-answer-spine-only? target)))

;; These are source fixtures for this experiment's own Work grammar. Keeping
;; the allocation examples here avoids making an alternate scheduling policy
;; depend on the application's strict compiler or a syntax conversion.
(define shared-fresh-frontier
  `(More
    (Work (Owners)
          (∃ (x:q)
             (∃ (x:x)
                (((x:x =? (sym "left") (label "left-x")) ∧
                  (x:q =? (sym "left") (label "left-q")) (label "left")) ∨
                 ((x:x =? (sym "right") (label "right-x")) ∧
                  (x:q =? (sym "right") (label "right-q")) (label "right"))
                 (label "choice"))
                (label "shared-fresh"))
             (label "query"))
          ,sigma-s)))

(define branch-fresh-frontier
  `(More
    (Work (Owners)
          (∃ (x:q)
             ((∃ (x:x)
                 ((x:x =? (sym "left") (label "left-x")) ∧
                  (x:q =? (sym "left") (label "left-q")) (label "left"))
                 (label "left-fresh")) ∨
              (∃ (x:x)
                 ((x:x =? (sym "right") (label "right-x")) ∧
                  (x:q =? (sym "right") (label "right-q")) (label "right"))
                 (label "right-fresh"))
              (label "choice"))
             (label "query"))
          ,sigma-s)))

(define (wf-disjunction? cfg)
  (judgment-holds (wf:wf-cfg/disj? ,cfg)))

(define (wf-search? cfg)
  ;; The experiment deliberately retains the historical common DisjR carrier;
  ;; production rail WF is the exact owner-aware checker for that syntax.
  (judgment-holds (wf:wf-cfg/rail? ,cfg)))

(define (disjunction-shape? cfg)
  (redex-match? lang:disj-lang F cfg))

(define (search-shape? cfg)
  (redex-match? distributed:distributed-search-lang F cfg))

(define (trace-locked? relation wf? shape? cfg [remaining TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (and (wf? cfg)
               (shape? cfg)
               (produced-answer-spine-only? cfg)
               (structurally-well-formed? cfg)))
     #f]
    [else
     (match (apply-reduction-relation/tag-with-names relation cfg)
       ['() (final-program? cfg)]
       [(list (list _ cfg^))
        (trace-locked? relation wf? shape? cfg^ (sub1 remaining))]
       [_ #f])]))

;; Inspect every exact named edge, including configurations that have not yet
;; produced an answer. No deduplication or forward search masks ambiguity.
(define (checked-trace relation cfg valid?
                       [remaining TRACE-CAP] [steps '()] [configurations '()])
  (check-true (valid? cfg) (format "invalid reached configuration: ~s" cfg))
  (match (apply-reduction-relation/tag-with-names relation cfg)
    ['() (values (reverse steps) (reverse (cons cfg configurations)))]
    [(list (list name next))
     (when (zero? remaining) (error 'checked-trace "step bound exhausted"))
     (checked-trace relation next valid? (sub1 remaining)
                    (cons name steps) (cons cfg configurations))]
    [successors (error 'checked-trace "nonunique named successors: ~s" successors)]))

;; Observation for the ground-equality witness below. Read only the completed
;; answer spine: pending Search, Work, and commit contain no public answers.
(define (ground-witness-events cfg)
  (match cfg
    [`(collect ,inner) (ground-witness-events inner)]
    [`(Forced ,_ ,inner) (cons 'force (ground-witness-events inner))]
    [`(Emit ,_ ,answer ,rest)
     (cons (ground-witness-answer-label answer) (ground-witness-events rest))]
    [`(Last ,_ ,answer) (list (ground-witness-answer-label answer))]
    [`(Solo ,_ ,state) (list (ground-witness-state-label state))]
    [_ '()]))

(define (ground-witness-answer-label answer)
  (match answer
    [`(Answer ,_ ,state) (ground-witness-state-label state)]
    [_ (error 'ground-witness-answer-label "unexpected answer: ~s" answer)]))

(define (ground-witness-state-label state)
  (match state
    [`(state ,_ ,_ ((,_ =? ,_ (label ,label))) ,_) label]
    [_ (error 'ground-witness-state-label "unexpected state: ~s" state)]))

(define pending-left-choice
  (term
   (More
    (Conj (Owners)
     (DisjL (Owners)
            (Work (Owners) (succeed (label "left")) ,sigma-s)
            (Work (Owners) (succeed (label "right")) ,sigma-s))
     (succeed (label "continuation"))))))

(define pending-right-choice
  (term
   (More
    (Conj (Owners)
     (DisjR (Owners)
            (Work (Owners) (succeed (label "left")) ,sigma-s)
            (Work (Owners) (succeed (label "right")) ,sigma-s))
     (succeed (label "continuation"))))))

(define failed-left-choice
  (term
   (More
    (Conj (Owners) (DisjL (Owners) (Dead (Owners)) (Dead (Owners)))
          (succeed (label "continuation"))))))

(define failed-right-choice
  (term
   (More
    (Conj (Owners) (DisjR (Owners) (Dead (Owners)) (Dead (Owners)))
          (succeed (label "continuation"))))))

(define/provide-test-suite DISTRIBUTED-PRESENTATION
  (test-case "distributed relations have an isolated static rule inventory"
    (check-rule-inventory
     "distributed/disjunction"
     distributed:disj-distributed-red
     (rule-name-union CORE-RULES
                      DISJUNCTION-RULES
                      '(distribute-choice)))
    (check-rule-inventory
     "distributed/search"
     distributed:search-distributed-red
     (rule-name-union CORE-RULES
                      DELAY-RULES
                      DISJUNCTION-RULES
                      SEARCH-JOIN-RULES
                      '(distribute-choice
                        distribute-right-choice)))
    (check-rule-inventory
     "distributed/DFS"
     distributed:search-dfs-distributed-red
     (rule-name-union CORE-RULES
                      DELAY-RULES
                      DISJUNCTION-RULES
                      SEARCH-JOIN-RULES
                      '(distribute-choice
                        distribute-right-choice
                        dfs-delay-left)))
    (check-rule-inventory
     "distributed/flip"
     distributed:search-flip-distributed-red
     (rule-name-union CORE-RULES
                      DELAY-RULES
                      DISJUNCTION-RULES
                      SEARCH-JOIN-RULES
                      '(distribute-choice
                        distribute-right-choice
                        flip-delay-left)))
    (check-rule-inventory
     "distributed/rail"
     distributed:rail-distributed-red
     (rule-name-union CORE-RULES
                      DELAY-RULES
                      DISJUNCTION-RULES
                      SEARCH-JOIN-RULES
                      '(distribute-choice
                        distribute-right-choice
                        rail-enter-right
                        rail-return-left))))

  (test-case "distributed focus grammar keeps ordinary and choice focus distinct"
    (define pending-conjunction
      (term
       (More
        (Conj (Owners) hole (succeed (label "continuation"))))))
    (define pending-choice
      (term (More (DisjL (Owners) hole (Dead (Owners))))))
    (check-true
     (redex-match? distributed:distributed-disj-lang
                   EarlyWF
                   pending-conjunction))
    (check-true
     (redex-match? distributed:distributed-search-lang
                   EarlyWF
                   pending-conjunction))
    (check-false
     (redex-match? distributed:distributed-disj-lang
                   EarlyChoiceWF
                   pending-conjunction))
    (check-true
     (redex-match? distributed:distributed-disj-lang
                   EarlyChoiceWF
                   pending-choice)))

  (test-case "distributed and factored conjunction steps remain explicit"
    (check-exact-step
     distributed:disj-distributed-red
     (step-witness
      "distribute-choice"
      pending-left-choice
      (term
       (More
        (DisjL (Owners)
         (Conj (Owners) (Work (Owners) (succeed (label "left")) ,sigma-s)
               (succeed (label "continuation")))
         (Conj (Owners) (Work (Owners) (succeed (label "right")) ,sigma-s)
               (succeed (label "continuation"))))))))
    (check-exact-step
     factored:disj-red
     (step-witness
      "succeed"
      pending-left-choice
      (term
       (More
        (Conj (Owners)
         (DisjL (Owners)
                (Returned (Owners) ,sigma-s)
                (Work (Owners) (succeed (label "right")) ,sigma-s))
         (succeed (label "continuation")))))))
    (check-exact-step
     distributed:search-distributed-red
     (step-witness
      "distribute-right-choice"
      pending-right-choice
      (term
       (More
        (DisjR (Owners)
         (Conj (Owners) (Work (Owners) (succeed (label "left")) ,sigma-s)
               (succeed (label "continuation")))
         (Conj (Owners) (Work (Owners) (succeed (label "right")) ,sigma-s)
               (succeed (label "continuation"))))))))
    (check-exact-step
     factored:rail-red
     (step-witness
      "succeed"
      pending-right-choice
      (term
       (More
        (Conj (Owners)
         (DisjR (Owners)
                (Work (Owners) (succeed (label "left")) ,sigma-s)
                (Returned (Owners) ,sigma-s))
         (succeed (label "continuation")))))))
    (check-exact-step
     distributed:disj-distributed-red
     (step-witness
      "distribute-choice"
      failed-left-choice
      (term
       (More
        (DisjL (Owners)
               (Conj (Owners) (Dead (Owners)) (succeed (label "continuation")))
               (Conj (Owners) (Dead (Owners)) (succeed (label "continuation"))))))))
    (check-exact-step
     factored:disj-red
     (step-witness
      "skip-left-failure"
      failed-left-choice
      (term
       (More
        (Conj (Owners) (Dead (Owners)) (succeed (label "continuation")))))))
    (check-exact-step
     distributed:search-distributed-red
     (step-witness
      "distribute-right-choice"
      failed-right-choice
      (term
       (More
        (DisjR (Owners)
               (Conj (Owners) (Dead (Owners)) (succeed (label "continuation")))
               (Conj (Owners) (Dead (Owners)) (succeed (label "continuation"))))))))
    (check-exact-step
     factored:rail-red
     (step-witness
      "skip-right-failure"
      failed-right-choice
      (term
       (More
        (Conj (Owners) (Dead (Owners)) (succeed (label "continuation"))))))))

  (test-case "distributed presentation inherits disjunction expansion and answer commitment"
    (check-exact-step
     distributed:disj-distributed-red
     (step-witness
      "expand-disjunction"
      (term
       (More
        (Work (Owners)
         ((succeed (label "left"))
          ∨
          (fail (label "right"))
          (label "split"))
         ,sigma-s)))
      (term
       (More
        (DisjL (Owners)
               (Work (Owners) (succeed (label "left")) ,sigma-s)
               (Work (Owners) (fail (label "right")) ,sigma-s))))))
    (check-exact-step
     distributed:search-distributed-red
     (step-witness
      "commit-choice-answer"
      cfg-disj
      (term
       (Emit (Owners)
             (Answer (Owners) ,sigma-a)
             (More (Returned (Owners) ,sigma-b)))))))

  (test-case "distributed search inherits the exact delay steps"
    (define suspended
      (term
       (More
        (PendingDelay (Owners)
         (Work (Owners) (succeed (label "inner")) ,sigma-s)))))
    (define forced
      (term
       (Forced (Owners)
        (More (Work (Owners) (succeed (label "inner")) ,sigma-s)))))

    (check-exact-step
     distributed:search-distributed-red
     (step-witness "suspend-goal" cfg-delay-goal suspended))
    (check-exact-step
     distributed:search-distributed-red
     (step-witness "force-delay" suspended forced)))

  (test-case "both presentations share the inherited nested-result trace"
    (define nested-answer
      (term
       (More
        (DisjL (Owners)
         (DisjL (Owners) (Returned (Owners) ,sigma-a) (Returned (Owners) ,sigma-b))
         (Dead (Owners))))))
    (define reassociated-answer
      (term
       (More
        (DisjL (Owners)
               (Returned (Owners) ,sigma-a)
               (DisjL (Owners) (Returned (Owners) ,sigma-b) (Dead (Owners)))))))
    (define committed-first-answer
      (term
       (Emit (Owners) (Answer (Owners) ,sigma-a)
             (More
              (DisjL (Owners) (Returned (Owners) ,sigma-b) (Dead (Owners)))))))
    (define nested-failure
      (term
       (More
        (DisjL (Owners)
         (DisjL (Owners) (Dead (Owners)) (Returned (Owners) ,sigma-b))
         (Dead (Owners))))))
    (define skipped-failure
      (term
       (More
        (DisjL (Owners) (Returned (Owners) ,sigma-b) (Dead (Owners))))))
    (define committed-second-answer
      (term
       (Emit (Owners) (Answer (Owners) ,sigma-b) (More (Dead (Owners))))))
    (define finished-failure
      (term
       (Emit (Owners) (Answer (Owners) ,sigma-b) (Done (Owners)))))
    (for ([relation (in-list (list distributed:disj-distributed-red
                                    factored:disj-red))])
      (check-exact-step
       relation
       (step-witness "reassociate-left-result"
                     nested-answer
                     reassociated-answer))
      (check-exact-step
       relation
       (step-witness "commit-choice-answer"
                     reassociated-answer
                     committed-first-answer))
      (check-exact-step
       relation
       (step-witness "skip-left-failure"
                     nested-failure
                     skipped-failure))
      (check-exact-step
       relation
       (step-witness "commit-choice-answer"
                     skipped-failure
                     committed-second-answer))
      (check-exact-step
       relation
       (step-witness "finish-failure"
                     committed-second-answer
                     finished-failure))))

  (test-case "both presentations preserve shared and branch-local introductions"
    (for ([relation (in-list (list distributed:disj-distributed-red
                                    factored:disj-red))])
      (define-values (shared-steps shared-final shared-status)
        (trace-deterministic relation shared-fresh-frontier))
      (define-values (branch-steps branch-final branch-status)
        (trace-deterministic relation branch-fresh-frontier))
      (check-true (trace-locked? relation wf-disjunction? disjunction-shape?
                                 shared-fresh-frontier))
      (check-true (trace-locked? relation wf-disjunction? disjunction-shape?
                                 branch-fresh-frontier))
      (check-equal? shared-status 'done)
      (check-equal? branch-status 'done)
      (check-true (wf-disjunction? shared-final))
      (check-true (wf-disjunction? branch-final))
      (check-true (redex-match? lang:disj-lang F shared-final))
      (check-true (redex-match? lang:disj-lang F branch-final))
      (check-true (structurally-well-formed? shared-final))
      (check-true (structurally-well-formed? branch-final))
      (check-true (final-program? shared-final))
      (check-true (final-program? branch-final))
      (check-equal? (count-step-name shared-steps "allocate-fresh") 2)
      (check-equal? (count-step-name branch-steps "allocate-fresh") 3)
      (check-equal? (term (structural-answer-count ,shared-final)) 2)
      (check-equal? (term (structural-answer-count ,branch-final)) 2)))

  (test-case "distributing pending bind over nested choices changes ordered Delay observations"
    ;; A finite source goal, with ground equalities distinguishing answer
    ;; trails. No allocation, relation calls, divergence, or input conversion
    ;; accounts for the difference: only the placement of pending conjunction.
    (define goal
      '(((((sym "A") =? (sym "A") (label "A")) ∨
          ((sym "B") =? (sym "B") (label "B")) (label "inner")) ∨
         ((sym "C") =? (sym "C") (label "C")) (label "outer")) ∧
        (suspend (succeed (label "k")) (label "suspend-k"))
        (label "conjunction")))
    (define initial `(More (Work (Owners) ,goal ,sigma-s)))
    (define (old-valid? cfg)
      (and (search-shape? cfg) (wf-search? cfg)
           (produced-answer-spine-only? cfg)))
    (define-values (factored-steps factored-configs)
      (checked-trace factored:rail-red initial old-valid?))
    (define-values (distributed-steps distributed-configs)
      (checked-trace distributed:rail-distributed-red initial old-valid?))
    (define strict-initial (strict:retained-query-initial goal #:state sigma-s))
    (define-values (strict-steps strict-configs)
      (checked-trace strict:retained-red `(collect ,strict-initial)
                     (lambda (cfg)
                       (and (redex-match? strict:ScopeS q cfg) (wf-s? cfg)))))
    (define (answer-state label)
      `(state () () (((sym ,label) =? (sym ,label) (label ,label))) (label "s")))
    (define (answer label) `(Answer (Owners) ,(answer-state label)))
    (define factored-final
      `(Forced (Owners)
               (Forced (Owners)
                       (Emit (Owners) ,(answer "A")
                             (Forced (Owners)
                                     (Emit (Owners) ,(answer "B")
                                           (Last (Owners) ,(answer "C"))))))))
    (define distributed-final
      `(Forced (Owners)
               (Forced (Owners)
                       (Forced (Owners)
                               (Emit (Owners) ,(answer "C")
                                     (Emit (Owners) ,(answer "A")
                                           (Last (Owners) ,(answer "B"))))))))
    ;; The strict account owns the terminal state directly. Compare native
    ;; structures separately; the shared observation above reads their events.
    (define strict-final
      `(Forced (Owners)
               (Forced (Owners)
                       (Emit (Owners) ,(answer "A")
                             (Forced (Owners)
                                     (Emit (Owners) ,(answer "B")
                                           (Solo (Owners) ,(answer-state "C"))))))))
    (check-equal? (last factored-configs) factored-final)
    (check-equal? (last strict-configs) strict-final)
    (check-equal? (last distributed-configs) distributed-final)
    (check-equal? (length factored-steps) 26)
    (check-equal? (length distributed-steps) 28)
    (check-equal? (count-step-name distributed-steps "distribute-choice") 2)
    (check-equal? (count-step-name factored-steps "reassociate-left-result") 1)
    (check-equal? (count-step-name strict-steps "advance-delay") 0)
    (check-equal? (count-step-name strict-steps "collect-delay") 3)
    (define factored-prefixes
      '(() (force) (force force) (force force "A")
           (force force "A" force) (force force "A" force "B")
           (force force "A" force "B" "C")))
    (define distributed-prefixes
      '(() (force) (force force) (force force force)
           (force force force "C") (force force force "C" "A")
           (force force force "C" "A" "B")))
    (check-equal? (remove-duplicates (map ground-witness-events factored-configs))
                  factored-prefixes)
    (check-equal? (remove-duplicates (map ground-witness-events strict-configs))
                  factored-prefixes)
    (check-equal? (remove-duplicates (map ground-witness-events distributed-configs))
                  distributed-prefixes)
    ;; Public advance stops at each Delay; internal force transitions in the
    ;; strict mplus resumptions do not themselves add a Forced observation.
    (define strict-round-0 (strict:retained-run strict-initial TRACE-CAP))
    (define strict-round-1
      (strict:retained-run `(advance ,strict-round-0) TRACE-CAP))
    (define strict-round-2
      (strict:retained-run `(advance ,strict-round-1) TRACE-CAP))
    (define strict-round-3
      (strict:retained-run `(advance ,strict-round-2) TRACE-CAP))
    (check-equal? (map ground-witness-events
                       (list strict-round-0 strict-round-1 strict-round-2 strict-round-3))
                  '(() (force) (force force "A") (force force "A" force "B" "C")))
    (for ([frontier (in-list (list strict-round-0 strict-round-1 strict-round-2))])
      (check-true (strict:retained-frontier? frontier))
      (check-false (final-program? frontier)))
    (check-equal? strict-round-3 strict-final))

  (test-case "distributed search reassociates and commits through a Forced spine"
    (define forced-branch
      (term
       (Forced (Owners)
        (More
         (DisjL (Owners)
                (DisjL (Owners) (Returned (Owners) ,sigma-a) (Dead (Owners)))
                (Returned (Owners) ,sigma-b))))))
    (define forced-mid
      (term
       (Forced (Owners)
        (More
         (DisjL (Owners)
                (Returned (Owners) ,sigma-a)
                (DisjL (Owners) (Dead (Owners)) (Returned (Owners) ,sigma-b)))))))
    (define forced-next
      (term
       (Forced (Owners)
        (Emit (Owners) (Answer (Owners) ,sigma-a)
              (More
               (DisjL (Owners) (Dead (Owners)) (Returned (Owners) ,sigma-b)))))))
    (check-exact-step
     distributed:search-distributed-red
     (step-witness "reassociate-left-result" forced-branch forced-mid))
    (check-exact-step
     distributed:search-distributed-red
     (step-witness "commit-choice-answer" forced-mid forced-next))
    (check-exact-step
     distributed:search-distributed-red
     (step-witness
      "commit-choice-answer"
      (term
       (Forced (Owners)
        (Emit (Owners) (Answer (Owners) ,sigma-a)
              (More
               (DisjL (Owners) (Returned (Owners) ,sigma-b) (Dead (Owners)))))))
      (term
       (Forced (Owners)
        (Emit (Owners) (Answer (Owners) ,sigma-a)
              (Emit (Owners) (Answer (Owners) ,sigma-b)
                    (More (Dead (Owners))))))))))

  (test-case "distributed search keeps inherited delay traces inside the search node"
    (check-true
     (trace-locked? distributed:search-distributed-red
                    wf-search?
                    search-shape?
                    cfg-delay-goal)))

  (test-case "mixed answers expose the presentation-specific conjunction step"
    (define distributed-successors
      (apply-reduction-relation/tag-with-names
       distributed:search-distributed-red
       cfg-mixed-answer))
    (define factored-successors
      (apply-reduction-relation/tag-with-names
       factored:search-red
       cfg-mixed-answer))
    (check-equal? (length distributed-successors) 1)
    (check-equal? (length factored-successors) 1)
    (check-equal? (tagged-successor-name (first distributed-successors))
                  "distribute-choice")
    (check-equal? (tagged-successor-name (first factored-successors))
                  "resume-left-choice-success"))

  (test-case "distributed disjunction and scheduler witnesses remain in their domains"
    (check-exact-domain-step
     distributed:disj-distributed-red
     wf-disjunction?
     disjunction-shape?
     (step-witness
      "distribute-choice"
      cfg-mixed-answer
      (term
       (More
        (DisjL (Owners)
         (DisjL (Owners)
          (Conj (Owners) (Returned (Owners) ,sigma-a) (succeed (label "k")))
          (Conj (Owners) (Returned (Owners) ,sigma-b) (succeed (label "k"))))
         (Dead (Owners)))))))
    (check-exact-domain-step
     distributed:search-dfs-distributed-red
     wf-search?
     search-shape?
     (step-witness
      "dfs-delay-left"
      cfg-flip
      (term
       (More
        (PendingDelay (Owners)
         (DisjL (Owners)
          (Work (Owners) (succeed (label "late")) ,sigma-s)
          (Returned (Owners) ,sigma-b)))))))
    (check-exact-domain-step
     distributed:search-flip-distributed-red
     wf-search?
     search-shape?
     (step-witness
      "flip-delay-left"
      cfg-flip
      (term
       (More
        (PendingDelay (Owners)
         (DisjL (Owners)
          (Returned (Owners) ,sigma-b)
          (Work (Owners) (succeed (label "late")) ,sigma-s))))))))

  (test-case "distributed schedulers move delayed owners with their payloads"
    (define scoped-payload
      (term
       (Work (Owners (Owner (u:0) (label "fresh")))
             (succeed (label "late"))
             ,sigma-s)))
    (define right-active-source
      (term
       (More
        (DisjR (Owners)
               (Returned (Owners) ,sigma-b)
               ,scoped-delayed-left-search))))

    (for ([entry
           (in-list
            (list
             (list distributed:search-dfs-distributed-red
                   cfg-scoped-flip
                   "dfs-delay-left"
                   (term
                    (More
                     (PendingDelay (Owners)
                      (DisjL (Owners)
                             ,scoped-payload
                             (Returned (Owners) ,sigma-b))))))
             (list distributed:search-flip-distributed-red
                   cfg-scoped-flip
                   "flip-delay-left"
                   (term
                    (More
                     (PendingDelay (Owners)
                      (DisjL (Owners)
                             (Returned (Owners) ,sigma-b)
                             ,scoped-payload)))))
             (list distributed:rail-distributed-red
                   cfg-scoped-rail
                   "rail-enter-right"
                   (term
                    (More
                     (PendingDelay (Owners)
                      (DisjR (Owners)
                             ,scoped-payload
                             (Returned (Owners) ,sigma-b))))))
             (list distributed:rail-distributed-red
                   right-active-source
                   "rail-return-left"
                   (term
                    (More
                     (PendingDelay (Owners)
                      (DisjL (Owners)
                             (Returned (Owners) ,sigma-b)
                             ,scoped-payload)))))))])
      (match-define
        (list relation source scheduler-name target)
        entry)
      (check-exact-step
       relation
       (step-witness scheduler-name source target))))

  (test-case "distributed rail enters the right branch and commits its answer"
    (define entered
      (term
       (More
        (PendingDelay (Owners)
         (DisjR (Owners)
          (Work (Owners) (succeed (label "late")) ,sigma-s)
          (Returned (Owners) ,sigma-b))))))
    (define committed
      (term
       (Emit (Owners) (Answer (Owners) ,sigma-b) (More (Dead (Owners))))))

    (check-exact-step
     distributed:rail-distributed-red
     (step-witness "rail-enter-right" cfg-rail entered))
    (check-true (search-shape? entered))
    (check-exact-step
     distributed:rail-distributed-red
     (step-witness
      "commit-right-choice-answer"
      (term (More (DisjR (Owners) (Dead (Owners)) (Returned (Owners) ,sigma-b))))
      committed))
    (check-true (search-shape? committed)))

  (test-case "distributed schedulers normalize before scheduling"
    (define scheduled-choice
      (term
       (More
        (Conj (Owners)
         (DisjL (Owners) (PendingDelay (Owners) (Dead (Owners))) (Dead (Owners)))
         (succeed (label "continuation"))))))
    (define expected
      (term
       (More
        (DisjL (Owners)
         (Conj (Owners) (PendingDelay (Owners) (Dead (Owners)))
               (succeed (label "continuation")))
         (Conj (Owners) (Dead (Owners)) (succeed (label "continuation")))))))
    (for ([relation
           (in-list
            (list distributed:search-dfs-distributed-red
                  distributed:search-flip-distributed-red
                  distributed:rail-distributed-red))])
      (check-exact-step relation
                        (step-witness "distribute-choice"
                                      scheduled-choice
                                      expected))))

  (test-case "distributed relcall variants retain explicit call expansion"
    (define expected
      (term
       (,gamma-delay
        (More
         (Work (Owners)
               (suspend (succeed (label "inner")) (label "zz"))
               ,sigma-a)))))
    (for ([relation
           (in-list
            (list distributed:search-distributed-relcall-red
                  distributed:search-dfs-distributed-relcall-red
                  distributed:search-flip-distributed-relcall-red
                  distributed:rail-distributed-relcall-red))])
      (check-exact-step relation
                        (step-witness "expand-relcall"
                                      cfg-call
                                      expected))))

  (test-case "distributed relcall expands inside the active branch"
    (check-exact-step
     distributed:search-distributed-relcall-red
     (step-witness
      "expand-relcall"
      cfg-call-branch
      (term
       (,gamma-delay
        (More
         (DisjL (Owners)
          (Work (Owners)
                (suspend (succeed (label "inner")) (label "zz"))
                ,sigma-a)
          (Returned (Owners) ,sigma-b))))))))

  (test-case "distributed DFS and relcall assembly order has the same successor"
    (define alternate-search-dfs-distributed-relcall-red
      (let ([relcall-expand
             (reduction-relation
              distributed:distributed-search-relcall-lang
              #:domain config
              [--> (Γ (in-hole EarlyWF (Work owners (r t ... tag) σ)))
                   (Γ (in-hole EarlyWF (Work owners g_new σ)))
                   (where g_new
                          ,(instantiate-call-host
                            (term Γ)
                            (term r)
                            (term (t ...))))
                   "expand-relcall"])])
        (union-reduction-relations
         (context-closure
          (extend-reduction-relation
           distributed:search-dfs-distributed-red
           distributed:distributed-search-relcall-lang)
          distributed:distributed-search-relcall-lang
          (Γ hole))
         relcall-expand)))

    (check-equal?
     (apply-reduction-relation
      distributed:search-dfs-distributed-relcall-red
      cfg-call-branch)
     (apply-reduction-relation
      alternate-search-dfs-distributed-relcall-red
      cfg-call-branch))))

(module+ test
  (exit (run-tests DISTRIBUTED-PRESENTATION)))
