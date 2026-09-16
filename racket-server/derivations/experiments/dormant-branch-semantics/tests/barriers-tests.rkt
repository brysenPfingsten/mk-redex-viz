#lang racket

(require rackunit redex/reduction-semantics
         (prefix-in app: "../../../../src/search-runtime.rkt") "../../../../src/search-strategy.rkt"
         (prefix-in dfs: "../source/reduction-relations/search-dfs-relcall-red.rkt")
         (prefix-in flip: "../source/reduction-relations/search-flip-relcall-red.rkt")
         (prefix-in rail: "../source/reduction-relations/rail-relcall-red.rkt")
         (prefix-in base: "../source/languages/search-relcall-lang.rkt")
         (prefix-in wf: "../source/wf/all.rkt")
         "../../../../src/sexpr-read.rkt"
         (only-in "../../../../src/transpiler.rkt" parse-prog/canonical)
         (prefix-in strict: "../../../matrix/full-source.rkt")
         (prefix-in lattice: "../source/languages/rail-relcall-lang.rkt")
         (prefix-in direct: "../../../s-reference/interpreter.rkt")
         (prefix-in online: "../derivation/interpreter.rkt")
         (prefix-in source: "../derivation/source.rkt")
         (only-in "../../../shared/kernel.rkt"
                  owners-support owners-append named-variable? address-state)
         (only-in "../../../shared/maps.rkt" Q-SE Q-SN)
         "../../../test-support/witnesses.rkt")

;; These are diagnostics between existing semantics, not a proposed scheduler
;; interpreter or a correspondence map that evaluates away their differences.
(struct edge (name before after) #:transparent)
(struct trace (configuration edges) #:transparent)

(define strategies
  (list (strict-search) (search-strategy "dfs")
        (search-strategy "flip") (search-strategy "rail")))
(define empty-state '(state () () () (label "initial")))
(define (atom name) `((sym ,name) =? (sym ,name) (label ,name)))
(define (suspend-goal goal) `(suspend ,goal (label "delay")))
(define (disj left right) `(,left ∨ ,right (label "disj")))
(define (conj left right) `(,left ∧ ,right (label "conj")))
(define failure '(fail (label "fail")))

;; These diagnostics intentionally execute the earlier online sources. The
;; application's scheduler choices now use strict matrix rows, so the former
;; sources must be named explicitly rather than obtained from GUI routing.
(define (lookup-search-step-once strategy)
  (match strategy
    [(strict-search) (app:lookup-search-step-once strategy)]
    [(search-strategy "dfs") dfs:step-once]
    [(search-strategy "flip") flip:step-once]
    [(search-strategy "rail") rail:step-once]))
(define (search-config-in-domain? strategy cfg)
  (match strategy
    [(strict-search) (app:search-config-in-domain? strategy cfg)]
    [(search-strategy "rail") (redex-match? lattice:rail-relcall-lang config cfg)]
    [_ (redex-match? base:search-relcall-lang config cfg)]))
(define (search-config-well-formed? strategy cfg)
  (match strategy
    [(strict-search) (app:search-config-well-formed? strategy cfg)]
    [(search-strategy "rail") (judgment-holds (wf:wf-config/rail-relcall? ,cfg))]
    [_ (judgment-holds (wf:wf-config/search-relcall? ,cfg))]))
(define (configuration-status cfg)
  (match cfg
    [`(program ,_ ,_) (app:configuration-status cfg)]
    [`(,_ ,frontier)
     (define (status body)
       (match body
         [(or `(Done ,_) `(Last ,_ ,_)) 'complete]
         [(or `(Emit ,_ ,_ ,tail) `(Forced ,_ ,tail)) (status tail)]
         [`(More (PendingDelay ,_ ,_)) 'paused]
         [`(More ,_) 'running]))
     (status frontier)]))

(define (configuration-body configuration)
  (match configuration
    [`(program ,_ ,_ ,body) body]
    [`(program ,_ ,body) body]
    [`(,_ ,body) body]))

(define (initial strategy goal [definitions '()] [owners '(Owners)] [state empty-state])
  (match strategy
    [(strict-search) `(program ,definitions (commit (eval ,owners ,goal ,state)))]
    [(search-strategy _) `(,definitions (More (Work ,owners ,goal ,state)))]))

;; A bound limits observation; it does not assert termination. Only the native
;; complete status does that. Each original configuration is checked for WF.
(define (run-trace strategy configuration [fuel 200] [reversed '()])
  (check-true (search-config-in-domain? strategy configuration))
  (check-true (search-config-well-formed? strategy configuration))
  (match (configuration-status configuration)
    ['complete (trace configuration (reverse reversed))]
    [(or 'running 'paused)
     (cond
       [(zero? fuel) (trace configuration (reverse reversed))]
       [else
        (define successors
          (match* (strategy (configuration-status configuration))
            [((strict-search) 'paused)
             (list (list "advance" (app:advance-configuration configuration)))]
            [(_ _) ((lookup-search-step-once strategy) configuration)]))
        (check-equal? (length successors) 1)
        (match-define (list (list name next)) successors)
        (run-trace strategy next (sub1 fuel)
                   (cons (edge name configuration next) reversed))])]
    [status (error 'run-trace "unexpected native status ~e for ~e" status configuration)]))

(define (answer-states frontier)
  (match frontier
    [`(Emit ,_ (Answer ,_ ,state) ,rest) (cons state (answer-states rest))]
    [`(Last ,_ (Answer ,_ ,state)) (list state)]
    [`(Solo ,_ ,state) (list state)]
    [`(,(or 'Forced) ,_ ,rest) (answer-states rest)]
    [`(advance ,rest) (answer-states rest)]
    [_ '()]))

(define (state-labels state)
  (match state [`(state ,_ ,_ ,trail ,_) (map (lambda (equation) (second (last equation))) trail)]))

(define strict-focus
  (term-match/single strict:StrictSRel
    [(in-hole C (eval owners g σ)) (list (term C) (term owners) (term g) (term σ))]))
(define lattice-focus
  (term-match/single lattice:rail-relcall-lang
    [(Γ (in-hole WorkFocus (Work owners g σ)))
     (list (term WorkFocus) (term owners) (term g) (term σ))]))

(define (focus strategy configuration)
  ((if (strict-search? strategy) strict-focus lattice-focus) configuration))

(define (events strategy execution)
  (append-map
   (lambda (transition)
     (match-define (edge name before after) transition)
     (append
      (match name
        ["advance" '(public-advance)]
        ["force-delay" (list (if (strict-search? strategy) 'internal-force 'public-force))]
        [(or "eval-atom" "unify-success" "fail" "succeed")
         (match-define (list _ _ goal _) (focus strategy before))
         (list (list 'work (second (last goal))))]
        [_ '()])
      (for/list ([state (in-list
                        (drop (answer-states (configuration-body after))
                              (length (answer-states (configuration-body before)))))])
        (list 'commit (state-labels state)))))
   (trace-edges execution)))

(define (check-complete execution)
  (check-equal? (configuration-status (trace-configuration execution)) 'complete))

;; This observer follows the unique context hole. Answer-private Owners never
;; enter the residual branch. Unlike Q-SN, it retains Owner groups and tags.
(define (has-hole? datum)
  (match datum
    [(? (lambda (x) (equal? x (term hole)))) #t]
    [(cons left right) (or (has-hole? left) (has-hole? right))]
    [_ #f]))

(define (context-owners context [owners '(Owners)])
  (match context
    [(? (lambda (x) (equal? x (term hole)))) owners]
    [`(program ,_ ,inner) (context-owners inner owners)]
    [`(,(or 'mplus 'mplusR 'DisjL 'DisjR) ,local ,left ,right)
     (context-owners (if (has-hole? left) left right) (owners-append owners local))]
    [`(,(or 'bind 'Conj) ,local ,inner ,_)
     (context-owners inner (owners-append owners local))]
    [`(,(or 'Yield 'Emit) ,local ,_ ,inner)
     (context-owners inner (owners-append owners local))]
    [`(,(or 'Forced 'PendingDelay) ,local ,inner)
     (context-owners inner (owners-append owners local))]
    [`(,(or 'force 'render 'commit 'advance 'collect 'More) ,inner)
     (context-owners inner owners)]
    [_ (raise-argument-error 'context-owners "native evaluation context" context)]))

;; Alpha variables are observation data, never input to either evaluator. One
;; path's ordered support supplies the SAME renaming for its Owners, pending
;; goal, substitution, disequalities, and replay trail. Unallocated names fail.
(define (rename-scoped datum support)
  (match datum
    [(? named-variable? name)
     (match (index-of support name)
       [#f (error 'rename-scoped "unallocated variable ~e in ~e" name support)]
       [index `(allocated ,index)])]
    [(cons left right) (cons (rename-scoped left support) (rename-scoped right support))]
    [_ datum]))

(define (canonical-frontier frontier [inherited '()])
  (match frontier
    [`(Done ,owners) `(Done ,(rename-scoped owners (owners-support owners inherited)))]
    [`(Last ,owners (Answer ,answer-owners ,state))
     ;; Observe the historical terminal's complete path. This neutral tag is
     ;; observation data, not a conversion to the current Solo constructor.
     (define local (owners-append owners answer-owners))
     (define here (owners-support local inherited))
     `(terminal-answer ,(rename-scoped local here) ,(rename-scoped state here))]
    [`(Solo ,owners ,state)
     (define here (owners-support owners inherited))
     `(terminal-answer ,(rename-scoped owners here) ,(rename-scoped state here))]
    [`(Emit ,owners (Answer ,answer-owners ,state) ,rest)
     (define here (owners-support owners inherited))
     (define answer-world (owners-support answer-owners here))
     `(Emit ,(rename-scoped owners here)
            (Answer ,(rename-scoped answer-owners answer-world)
                    ,(rename-scoped state answer-world))
            ,(canonical-frontier rest here))]
    [`(Forced ,owners ,rest)
     (define here (owners-support owners inherited))
     `(Forced ,(rename-scoped owners here) ,(canonical-frontier rest here))]
    [_ (raise-argument-error 'canonical-frontier "completed native Frontier" frontier)]))

;; This is a SECOND, explicitly weaker observation. It keeps Forced/Emit
;; shape and each leaf's complete ordered Owner groups, but forgets which
;; ancestor node physically carried a shared group. It is not scoped alpha:
;; delayed-sibling-capture needs this additional scope-transport quotient.
(define (world-frontier frontier [inherited '(Owners)])
  (match frontier
    [`(Done ,owners)
     (define world (owners-append inherited owners))
     `(Done ,(rename-scoped world (owners-support world)))]
    [`(Last ,owners (Answer ,answer-owners ,state))
     (define world (owners-append inherited (owners-append owners answer-owners)))
     (define support (owners-support world))
     `(terminal-answer ,(rename-scoped world support) ,(rename-scoped state support))]
    [`(Solo ,owners ,state)
     (define world (owners-append inherited owners))
     (define support (owners-support world))
     `(terminal-answer ,(rename-scoped world support) ,(rename-scoped state support))]
    [`(Emit ,owners (Answer ,answer-owners ,state) ,rest)
     (define here (owners-append inherited owners))
     (define world (owners-append here answer-owners))
     (define support (owners-support world))
     `(Emit (Answer ,(rename-scoped world support) ,(rename-scoped state support))
            ,(world-frontier rest here))]
    [`(Forced ,owners ,rest)
     `(Forced ,(world-frontier rest (owners-append inherited owners)))]
    [_ (raise-argument-error 'world-frontier "completed native Frontier" frontier)]))

;; Test-only observations consume each native terminal directly. Neither
;; observer constructs a configuration to execute or passes historical Last
;; values to the current S/E/N representation maps. The named observation
;; retains literal supports; the numeric one addresses each world's state
;; using that world's support. Both deliberately forget Owner groups/tags.
(define (allocation-state state support carrier)
  (match-define `(state ,sub ,dis ,trail ,tag) state)
  (define supported `(state (Support ,@support) ,sub ,dis ,trail ,tag))
  (match carrier
    ['named supported]
    ['numeric (address-state supported support)]))

(define (allocation-frontier frontier carrier [inherited '()])
  (match frontier
    [`(Done ,owners)
     (define support (owners-support owners inherited))
     `(Done ,(match carrier ['named `(Support ,@support)] ['numeric (length support)]))]
    [`(Last ,owners (Answer ,private ,state))
     `(terminal-answer
       ,(allocation-state state (owners-support (owners-append owners private) inherited) carrier))]
    [`(Solo ,owners ,state)
     `(terminal-answer ,(allocation-state state (owners-support owners inherited) carrier))]
    [`(Emit ,owners (Answer ,private ,state) ,rest)
     (define here (owners-support owners inherited))
     `(Emit ,(allocation-state state (owners-support private here) carrier)
            ,(allocation-frontier rest carrier here))]
    [`(Forced ,owners ,rest)
     `(Forced ,(allocation-frontier rest carrier (owners-support owners inherited)))]
    [_ (raise-argument-error 'allocation-frontier "completed native Frontier" frontier)]))

;; Independently observe a current E or N map result to check the test
;; observation above. This accepts only those rows' native terminal shape.
(define (mapped-frontier-observation frontier)
  (match frontier
    [`(Done ,supply) `(Done ,supply)]
    [`(Solo ,state) `(terminal-answer ,state)]
    [`(Emit ,state ,rest) `(Emit ,state ,(mapped-frontier-observation rest))]
    [`(Forced ,rest) `(Forced ,(mapped-frontier-observation rest))]
    [_ (raise-argument-error 'mapped-frontier-observation "completed native E/N Frontier" frontier)]))

(define (allocation-observations strategy execution)
  (sort
   (for/list ([transition (in-list (trace-edges execution))]
              #:when (equal? (edge-name transition) "allocate-fresh"))
     (match-define (list context local goal state) (focus strategy (edge-after transition)))
     (define owners (owners-append (context-owners context) local))
     (define support (owners-support owners))
     (list (rename-scoped owners support)
           (rename-scoped goal support)
           (rename-scoped state support)))
   string<? #:key (lambda (observation) (format "~s" observation))))

;; Online source and native lattice traces are collected independently. There
;; is no state translation or search for a future matching configuration here.
;; This finite runner requires completion, rather than interpreting its bound
;; as an acceptable observation endpoint.
(define (run-online-source configuration [fuel 2000] [reversed '()])
  (check-true (source:source-in-domain? configuration))
  (define body (configuration-body configuration))
  (define focus (source:decompose body))
  (check-equal? (source:plug (source:focus-term focus) (source:focus-frames focus)) body)
  (cond
    [(source:complete? body) (trace configuration (reverse reversed))]
    [(zero? fuel) (error 'run-online-source "finite validation witness exceeded its bound")]
    [else
     (define next
       (match (source:step configuration)
         [#f
          (check-true (source:frontier? body))
          (list "advance-invocation" (source:advance configuration))]
         [step step]))
     (match-define (list name successor) next)
     (run-online-source successor (sub1 fuel)
                        (cons (edge name configuration successor) reversed))]))

(define (scoped-work context local goal state)
  (define owners (owners-append (context-owners context) local))
  (define support (owners-support owners))
  (list (rename-scoped owners support)
        (rename-scoped goal support)
        (rename-scoped state support)))

(define (online-atomic-work execution)
  (for/list ([transition (in-list (trace-edges execution))]
             #:when (equal? (edge-name transition) "eval-atom"))
    (define focus (source:decompose (configuration-body (edge-before transition))))
    (match-define `(eval ,owners ,goal ,state) (source:focus-term focus))
    (scoped-work (source:plug (term hole) (source:focus-frames focus)) owners goal state)))

(define (native-atomic-work strategy execution)
  (for/list ([transition (in-list (trace-edges execution))]
             #:when (member (edge-name transition)
                            '("succeed" "fail" "unify-success" "unify-fail"
                              "unify-violates-disequality" "disequality-success"
                              "disequality-fail")))
    (match-define (list context owners goal state) (focus strategy (edge-before transition)))
    (scoped-work context owners goal state)))

(define (compile-for text profile strategy)
  (define-values (configuration _html query)
    (parse-prog/canonical (read-all-sexprs (open-input-string text))
                          #:compile-profile profile #:search-strategy strategy))
  (match-define `(program ,definitions (commit (eval ,owners ,goal ,state))) configuration)
  (values (initial strategy goal definitions owners state) query))

(define (compiled-input configuration)
  (match configuration
    [`(program ,definitions (commit (eval ,owners ,goal ,state)))
     (list definitions owners goal state)]
    [`(,definitions (More (Work ,owners ,goal ,state)))
     (list definitions owners goal state)]))

(module+ test
  (test-case "terminal observations preserve native ownership paths without extending current maps"
    (define root '(Owners (Owner (u:9) (label "root")) (Owner () (label "empty"))))
    (define private '(Owners (Owner (u:2) (label "private"))))
    (define state '(state ((u:2 u:9)) ((u:2 (sym "avoid")))
                          ((u:2 =? u:9 (label "alias"))) (label "terminal")))
    (define historical `(Last ,root (Answer ,private ,state)))
    (define current `(Solo ,(owners-append root private) ,state))
    (check-not-equal? historical current)
    (check-equal? (answer-states historical) (list state))
    (check-equal? (answer-states current) (list state))
    (check-equal? (canonical-frontier historical) (canonical-frontier current))
    (check-equal? (world-frontier historical) (world-frontier current))
    (check-equal? (canonical-frontier current)
                  '(terminal-answer
                    (Owners (Owner ((allocated 0)) (label "root"))
                            (Owner () (label "empty"))
                            (Owner ((allocated 1)) (label "private")))
                    (state (((allocated 1) (allocated 0)))
                           (((allocated 1) (sym "avoid")))
                           (((allocated 1) =? (allocated 0) (label "alias")))
                           (label "terminal"))))
    (for ([carrier '(named numeric)] [map (list Q-SE Q-SN)])
      (check-equal? (allocation-frontier historical carrier)
                    (allocation-frontier current carrier))
      (check-equal? (allocation-frontier current carrier)
                    (mapped-frontier-observation (map current)))
      (check-exn exn:fail? (lambda () (map historical))))
    (check-exn exn:fail? (lambda () (canonical-frontier `(One ,root ,state)))))

  (test-case "eager siblings perform semantic work before Strict can commit their first answer"
    (define goal (disj (atom "A") (atom "B")))
    (for ([strategy (in-list strategies)])
      (define execution (run-trace strategy (initial strategy goal)))
      (check-complete execution)
      (check-equal? (events strategy execution)
                    (if (strict-search? strategy)
                        '((work "A") (work "B") (commit ("A")) (commit ("B")))
                        '((work "A") (commit ("A")) (work "B") (commit ("B")))))
      (check-equal? (map edge-name (trace-edges execution))
                    (if (strict-search? strategy)
                        '("eval-disj" "eval-atom" "eval-atom" "mplus-one" "commit-yield" "commit-one")
                        '("expand-disjunction" "unify-success" "commit-choice-answer"
                          "unify-success" "finish-success")))
      (when (strict-search? strategy)
        (check-equal? (direct:collect-all (direct:run goal))
                      (configuration-body (trace-configuration execution))))))

  (test-case "bind candidates remain uncommitted but the eager tail changes work order"
    (define goal (conj (disj (atom "A") (atom "B")) failure))
    (for ([strategy (in-list strategies)])
      (define execution (run-trace strategy (initial strategy goal)))
      (check-complete execution)
      (check-equal? (events strategy execution)
                    (if (strict-search? strategy)
                        '((work "A") (work "B") (work "fail") (work "fail"))
                        '((work "A") (work "fail") (work "B") (work "fail"))))
      (for ([transition (in-list (trace-edges execution))])
        (check-equal? (answer-states (configuration-body (edge-after transition))) '()))))

  (test-case "left delay changes work timing even when completed Frontier observations agree"
    (define goal (disj (suspend-goal (atom "A")) (atom "B")))
    (define executions
      (for/list ([strategy (in-list strategies)])
        (define execution (run-trace strategy (initial strategy goal)))
        (check-complete execution)
        (check-equal? (events strategy execution)
                      (match strategy
                        [(strict-search)
                         '((work "B") public-advance internal-force (work "A")
                           (commit ("B")) (commit ("A")))]
                        [(search-strategy "dfs")
                         '(public-force (work "A") (commit ("A")) (work "B") (commit ("B")))]
                        [(search-strategy (or "flip" "rail"))
                         '(public-force (work "B") (commit ("B")) (work "A") (commit ("A")))]))
        execution))
    (check-equal? (canonical-frontier (configuration-body (trace-configuration (first executions))))
                  (canonical-frontier (configuration-body (trace-configuration (fourth executions))))))

  (test-case "compiler policy stays fixed across the eager and divergent scheduler comparisons"
    (for ([case (in-list
                 (list
                  (list "relbody" "(run* (q) (conde [(== q 'a)] [(== q 'b)]))" #f)
                  (list "disj" "(defrel (spin) (spin)) (run* (q) (conde [(== q 'a)] [(spin)]))" #t)))])
      (match-define (list placement source divergent?) case)
      (define profile (hasheq 'conjAssoc "left" 'disjAssoc "right" 'delayPlacement placement))
      (define compiled
        (for/list ([strategy (in-list strategies)])
          (define-values (configuration query) (compile-for source profile strategy))
          (list strategy configuration query)))
      (for ([entry (in-list compiled)])
        (match-define (list strategy configuration query) entry)
        (check-equal? (compiled-input configuration) (compiled-input (second (first compiled))))
        (check-equal? query (third (first compiled)))
        (define execution (run-trace strategy configuration 50))
        (define answers (answer-states (configuration-body (trace-configuration execution))))
        (cond
          [divergent?
           (check-equal? (length answers) (if (strict-search? strategy) 0 1))
           (match-define (edge name before after) (last (trace-edges execution)))
           (check-equal? name (if (strict-search? strategy) "eval-call" "expand-relcall"))
           (check-equal? before after)
           ;; The exact sole native successor closes the cycle. This justifies
           ;; persistence of the missing/present answer beyond the bound.
           (check-equal? ((lookup-search-step-once strategy) after) (list (list name after)))]
          [else
           (check-complete execution)
           (check-equal? (length answers) 2)
           (check-equal? (events strategy execution)
                         (if (strict-search? strategy)
                             '((work "u2") (work "u3") (commit ("u2")) (commit ("u3")))
                             '((work "u2") (commit ("u2")) (work "u3") (commit ("u3")))))]))))

  (test-case "guarded DFS repeats its left-priority shape; other schedulers expose the finite sibling"
    (define call '(r:loop (label "loop-call")))
    (define definitions `((r:loop () ,(suspend-goal call))))
    (define goal (disj call (atom "B")))
    (for ([strategy (in-list strategies)])
      (define execution (run-trace strategy (initial strategy goal definitions) 60))
      (match strategy
        [(search-strategy "dfs")
         (check-equal? (answer-states (configuration-body (trace-configuration execution))) '())
         (check-false (member '(work "B") (events strategy execution)))
         (define first-force (findf (lambda (step) (equal? (edge-name step) "force-delay"))
                                    (trace-edges execution)))
         (check-not-false first-force)
         (define cycle (run-trace strategy (edge-before first-force) 4))
         (check-equal? (map edge-name (trace-edges cycle))
                       '("force-delay" "expand-relcall" "suspend-goal" "dfs-delay-left"))
         (match-define (list defs frontier) (edge-before first-force))
         (check-equal? (trace-configuration cycle) `(,defs (Forced (Owners) ,frontier)))]
        [_
         (check-equal? (map state-labels (answer-states (configuration-body (trace-configuration execution))))
                       '(("B")))
         (check-not-false (member '(commit ("B")) (events strategy execution)))])))

  (test-case "scoped allocation comparison preserves shared, private, unused, sparse, and tagged worlds"
    (for ([name '(empty-binder unused-binder shared-outer sibling-reuse
                 answer-local-continuation allocated-failure failed-sibling
                 allocation-across-delay delayed-sibling-capture
                 sparse-inherited-ancestry inherited-state-and-trail)])
      (define example (findf (lambda (item) (eq? (witness-name item) name)) witnesses))
      (check-not-false example)
      (define strict-execution
        (run-trace (strict-search)
                   (initial (strict-search) (witness-goal example) '()
                            (witness-owners example) (witness-state example))))
      (check-complete strict-execution)
      (define strict-final (configuration-body (trace-configuration strict-execution)))
      (check-equal? (allocation-frontier strict-final 'named)
                    (mapped-frontier-observation (Q-SE strict-final)))
      (check-equal? (allocation-frontier strict-final 'numeric)
                    (mapped-frontier-observation (Q-SN strict-final)))
      (check-equal? (direct:collect-all
                    (direct:run (witness-goal example) #:owners (witness-owners example)
                                #:state (witness-state example))) strict-final)
      (for ([strategy (in-list (list (search-strategy "flip") (search-strategy "rail")))])
        (with-check-info (['witness name] ['strategy strategy])
          (define execution
            (run-trace strategy (initial strategy (witness-goal example) '()
                                        (witness-owners example) (witness-state example))))
          (check-complete execution)
          (define final (configuration-body (trace-configuration execution)))
          (check-equal? (world-frontier final) (world-frontier strict-final))
          (if (eq? name 'delayed-sibling-capture)
              (check-not-equal? (canonical-frontier final) (canonical-frontier strict-final))
              (check-equal? (canonical-frontier final) (canonical-frontier strict-final)))
          (check-equal? (allocation-observations strategy execution)
                        (allocation-observations (strict-search) strict-execution))
          (check-equal? (allocation-frontier final 'numeric)
                        (allocation-frontier strict-final 'numeric))
          (when (eq? name 'sibling-reuse)
            (check-not-equal? (allocation-frontier final 'named)
                              (allocation-frontier strict-final 'named)))))))

  (test-case "numeric erasure cannot replace preservation of Owner groups and tags"
    (define goals
      (list '(succeed (label "ok"))
            '(∃ () (succeed (label "ok")) (label "empty-owner"))
            '(∃ (x:a x:b) (succeed (label "ok")) (label "pair"))
            '(∃ (x:a) (∃ (x:b) (succeed (label "ok")) (label "inner")) (label "outer"))))
    (for ([strategy (in-list strategies)])
      (define finals
        (for/list ([goal (in-list goals)])
          (define execution (run-trace strategy (initial strategy goal)))
          (check-complete execution)
          (configuration-body (trace-configuration execution))))
      ;; Both sides come from actual complete reachable executions.
      (check-equal? (allocation-frontier (first finals) 'numeric)
                    (allocation-frontier (second finals) 'numeric))
      (check-not-equal? (canonical-frontier (first finals)) (canonical-frontier (second finals)))
      (check-equal? (allocation-frontier (third finals) 'numeric)
                    (allocation-frontier (fourth finals) 'numeric))
      (check-not-equal? (canonical-frontier (third finals)) (canonical-frontier (fourth finals)))))

  (test-case "twenty full-owner online witnesses preserve ordered atomic work and completed path-worlds"
    (check-equal? (length validation-witnesses) 20)
    (for* ([policy '(dfs flip rail)] [example (in-list validation-witnesses)])
      (with-check-info (['policy policy] ['witness (witness-name example)])
        (define goal (witness-goal example))
        (define owners (witness-owners example))
        (define state (witness-state example))
        (define strategy (search-strategy (symbol->string policy)))
        (define source-execution
          (run-online-source (source:initial goal #:policy policy #:owners owners #:state state)))
        (define native-execution (run-trace strategy (initial strategy goal '() owners state) 2000))
        (check-complete native-execution)
        (define source-final (configuration-body (trace-configuration source-execution)))
        (define native-final (configuration-body (trace-configuration native-execution)))
        (check-true (source:complete? source-final))
        (check-equal? (online:collect-all (online:run goal #:policy policy #:owners owners #:state state))
                      source-final)
        ;; The positive relation is explicitly world-frontier, NOT raw or
        ;; canonical-frontier equality. Scope transport remains observable at
        ;; the stronger level tested above.
        (check-equal? (world-frontier source-final) (world-frontier native-final))
        (define attempts (online-atomic-work source-execution))
        (check-equal? attempts (native-atomic-work strategy native-execution))
        (when (eq? (witness-name example) 'intermediate-success-then-failure)
          (check-true (ormap (lambda (attempt)
                               (match attempt
                                 [(list _ `(fail ,_) _) #t]
                                 [_ #f])) attempts)))))))
