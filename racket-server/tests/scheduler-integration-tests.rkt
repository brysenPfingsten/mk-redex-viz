#lang racket

(require rackunit rackunit/text-ui json redex/reduction-semantics
         "../src/app.rkt" "../src/program-runner.rkt"
         "../src/search-runtime.rkt" "../src/sexpr-read.rkt"
         (only-in "../src/transpiler.rkt" parse-prog/canonical query-info)
         (prefix-in scheduler: "../derivations/matrix/scheduler-source.rkt")
         (prefix-in strict: "../derivations/matrix/full-source.rkt")
         (only-in "../derivations/shared/wf.rkt" wf-s-rel?)
         "../derivations/test-support/witnesses.rkt"
         "test-http-helpers.rkt")

(provide SCHEDULER-INTEGRATION)

(struct edge (name before after) #:transparent)

(define (native-relation strategy)
  (match strategy
    [(strict-search) strict:strict-s-rel-red]
    [(search-strategy "dfs") scheduler:strict-dfs-red]
    [(search-strategy "flip") scheduler:strict-flip-red]
    [(search-strategy "rail") scheduler:strict-rail-red]))

(define (native-wf? strategy configuration)
  (match strategy
    [(strict-search) (wf-s-rel? configuration)]
    [(search-strategy _) (scheduler:scheduler-well-formed? configuration)]))

(define (contains-constructor? datum constructor)
  (match datum
    [(cons head tail)
     (or (eq? head constructor)
         (contains-constructor? head constructor)
         (contains-constructor? tail constructor))]
    [_ #f]))

(define (picture-nodes picture)
  (cons picture (append-map picture-nodes (hash-ref picture 'children '()))))

(define (configuration-body configuration)
  (match configuration
    [`(program ,_ ,body) body]
    [`(,_ ,body) body]))

;; Count only the public spine, independently of the runtime's extraction.
(define (answer-states term)
  (match term
    [`(Emit ,_ (Answer ,_ ,state) ,rest) (cons state (answer-states rest))]
    [`(Solo ,_ ,state) (list state)]
    [`(Forced ,_ ,rest) (answer-states rest)]
    [`(,(or 'advance 'collect) ,rest) (answer-states rest)]
    [_ '()]))

(define (check-payload response session)
  (define payload (string->jsexpr (response-body->string response)))
  (check-equal? (hash-ref payload 'executionStatus)
                (symbol->string (model-session-status session)))
  (check-equal? (hash-ref payload 'stepKind)
                (symbol->string (model-session-current-step-kind session)))
  (check-equal? (hash-ref payload 'answerCount)
                (length (model-session-current-answer-nodes session)))
  (check-equal? (string->jsexpr (hash-ref payload 'program))
                (model-session-current-picture session)))

;; Each session edge is checked against exactly one native named contraction.
;; Strict public invocation is the one separate, explicitly prescribed step.
(define (check-trace api library [fuel 300] [reversed '()])
  (define configuration (model-session-current-config api))
  (define strategy (model-session-search-strategy api))
  (check-equal? configuration (model-session-current-config library))
  (check-true (native-wf? strategy configuration))
  (when (member strategy (list (search-strategy "dfs") (search-strategy "flip")))
    (check-false (contains-constructor? configuration 'mplusR))
    (check-false (contains-constructor? configuration 'YieldR)))
  (define answer-count (length (answer-states (configuration-body configuration))))
  (check-equal? (length (model-session-current-answer-nodes api)) answer-count)
  (check-equal? (count (lambda (node) (member (hash-ref node 'renderRole #f)
                                           '("answer-node" "terminal-answer")))
                       (picture-nodes (model-session-current-picture api)))
                answer-count)
  (match (model-session-status api)
    ['complete
     (check-equal? (apply-reduction-relation (native-relation strategy) configuration) '())
     (values api (reverse reversed))]
    [(and status (or 'running 'paused))
     (when (zero? fuel) (error 'check-trace "finite witness exceeded its bound"))
     (define expected
       (match status
         ['paused
          (match-define `(program ,definitions ,frontier) configuration)
          (check-equal? (apply-reduction-relation (native-relation strategy) configuration) '())
          (list "advance" `(program ,definitions (advance ,frontier)))]
         [_
          (match-define (list successor)
            (apply-reduction-relation/tag-with-names (native-relation strategy) configuration))
          successor]))
     (define-values (response next-api) (step! api))
     (define next-library (model-session-step library))
     (check-equal? (list (model-session-current-step-name next-api)
                         (model-session-current-config next-api)) expected)
     (check-equal? (model-session-current-step-kind next-api)
                   (if (eq? status 'paused) 'public-operation 'reduction))
     (check-payload response next-api)
     (check-trace next-api next-library (sub1 fuel)
                  (cons (edge (first expected) configuration (second expected)) reversed))]
    [other (error 'check-trace "unexpected status ~e" other)]))

(define empty-state '(state () () () (label "initial")))
(define empty-query (query-info '() '() '(label "query") #f))
(define (open-goal strategy goal [owners '(Owners)] [state empty-state])
  (open-compiled
   `(program () (commit (eval ,owners ,goal ,state)))
   empty-query strategy))

(define strict-active
  (term-match/single scheduler:StrictRail
    [(in-hole C (eval owners a σ)) (term a)]))
(define (state-label state)
  (match state [`(state ,_ ,_ (,equation ,_ ...) ,_) (second (last equation))]))
(define (meaningful-events strategy edges)
  (append-map
   (lambda (transition)
     (match-define (edge name before after) transition)
     (append
      (match name
        ["advance" '(public-advance)]
        ["force-delay" '(internal-force)]
        ["eval-atom" (list (list 'work (second (last (strict-active before)))))]
        [_ '()])
      (for/list ([state (in-list (drop (answer-states (configuration-body after))
                                      (length (answer-states (configuration-body before)))))])
        (list 'commit (state-label state)))))
   edges))

(define (frontier-shape frontier)
  (match frontier
    [`(Forced ,_ ,rest) `(Forced ,(frontier-shape rest))]
    [`(Emit ,_ (Answer ,_ ,state) ,rest) `(Emit ,(state-label state) ,(frontier-shape rest))]
    [`(Solo ,_ ,state) `(Solo ,(state-label state))]
    [`(More ,_) 'More]
    [`(Done ,_) 'Done]))

(define/provide-test-suite SCHEDULER-INTEGRATION
  (test-case "every GUI scheduler finishes sibling work before commitment"
    (define goal '(((sym "A") =? (sym "A") (label "A")) ∨
                   ((sym "B") =? (sym "B") (label "B")) (label "choice")))
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define session (open-goal strategy goal))
      (define-values (_final edges) (check-trace session session))
      (check-equal? (meaningful-events strategy edges)
                    '((work "A") (work "B") (commit "A") (commit "B")))))

  (test-case "unguarded strict operands never commit a known answer in any scheduler"
    (define call '(r:loop (label "loop")))
    (define definitions `((r:loop () ,call)))
    (for* ([strategy (in-list all-surfaced-search-strategies)]
           [goal (in-list
                  (list `(((sym "A") =? (sym "A") (label "A")) ∨ ,call (label "choice"))
                        `((suspend ,call (label "delay")) ∨
                          ((sym "B") =? (sym "B") (label "B")) (label "choice"))))])
      (define initial (open-compiled `(program ,definitions (commit (eval (Owners) ,goal ,empty-state)))
                                     empty-query strategy))
      (define final
        (for/fold ([session initial]) ([_ (in-range 60)])
          (check-equal? (model-session-current-answer-nodes session) '())
          (check-true (native-wf? strategy (model-session-current-config session)))
          (model-session-step session)))
      (check-equal? (model-session-current-answer-nodes final) '())
      ;; Close the exact call self-loop; fuel exhaustion alone is not the claim.
      (define cfg (model-session-current-config final))
      (check-equal? (apply-reduction-relation/tag-with-names (native-relation strategy) cfg)
                    (list (list "eval-call" cfg)))))

  (test-case "strict schedulers still distinguish a guarded infinite left branch"
    (define call '(r:loop (label "loop")))
    (define definitions `((r:loop () (suspend ,call (label "guard")))))
    (define goal `(,call ∨ ((sym "B") =? (sym "B") (label "B")) (label "choice")))
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define initial (open-compiled `(program ,definitions (commit (eval (Owners) ,goal ,empty-state)))
                                     empty-query strategy))
      (define final
        (for/fold ([session initial]) ([_ (in-range 60)]) (model-session-step session)))
      (check-equal? (length (model-session-current-answer-nodes final))
                    (if (equal? strategy (search-strategy "dfs")) 0 1))))

  (test-case "all twelve profiles initialize and step all three native scheduler carriers"
    (define source
      "(defrel (same x y) (== x y))
       (defrel (choose q) (conde [(same q 'a)] [(same q 'b)] [(same q 'c)]))
       (run* (q) (same 'ready 'ready) (choose q) (=/= q 'a))")
    (check-equal? all-surfaced-search-strategies
                  (map search-strategy '("dfs" "flip" "rail")))
    (for* ([conjunction '("left" "right")]
           [disjunction '("left" "right")]
           [placement '("relbody" "relcall" "disj")]
           [strategy (in-list all-surfaced-search-strategies)])
      (define profile (hasheq 'conjAssoc conjunction 'disjAssoc disjunction 'delayPlacement placement))
      (with-check-info (['profile profile] ['strategy strategy])
        (define-values (compiled _html query)
          (parse-prog/canonical (read-all-sexprs (open-input-string source))
                                #:compile-profile profile #:search-strategy strategy))
        (define-values (response api)
          (init! #f (make-post-init-request source
                     (hasheq 'text source 'sourceMode "mini" 'compileProfile profile)
                     #:strategy strategy) 'scheduler-profile))
        (define library (open-source source #:compile-profile profile #:search-strategy strategy))
        (check-match compiled `(program ,_ (commit (eval (Owners) ,_ ,_))))
        (check-equal? compiled (model-session-current-config api))
        (check-equal? query (model-session-query api))
        (check-payload response api)
        (define-values (final edges) (check-trace api library))
        (check-not-false (member "advance-delay" (map edge-name edges)))
        (check-not-false (member "advance" (map edge-name edges)))
        (check-equal? (sort (model-session-current-host-answers final) symbol<?) '(b c)))))

  (test-case "nested rails preserve their own work order and public force boundaries"
    (define (atom name) `((sym ,name) =? (sym ,name) (label ,name)))
    (define goal
      `((suspend ,(atom "A") (label "delay-A"))
        ∨ (,(atom "B") ∨ (suspend ,(atom "C") (label "delay-C")) (label "inner"))
        (label "outer")))
    (for ([strategy (in-list (cons default-search-strategy all-surfaced-search-strategies))])
      (define initial (open-goal strategy goal))
      (define-values (final edges) (check-trace initial initial 80))
      (check-equal?
       (meaningful-events strategy edges)
       (match strategy
         [(or (strict-search) (search-strategy (or "flip" "rail")))
          '((work "B") public-advance internal-force (work "A") (commit "B")
            public-advance internal-force (work "C") (commit "A") (commit "C"))]
         [(search-strategy "dfs")
          '((work "B") public-advance internal-force (work "A") (commit "A") (commit "B")
            public-advance (work "C") (commit "C"))]))
      (check-equal?
       (frontier-shape (configuration-body (model-session-current-config final)))
       (match strategy
         [(search-strategy "dfs") '(Forced (Emit "A" (Emit "B" (Forced (Solo "C")))))]
         [_ '(Forced (Emit "B" (Forced (Emit "A" (Solo "C")))))]))
      (when (equal? strategy (search-strategy "rail"))
        (check-true (ormap (lambda (transition) (contains-constructor? (edge-after transition) 'mplusR)) edges)))
      (check-equal?
       (for/list ([transition (in-list edges)]
                  #:when (equal? (edge-name transition)
                                  "advance"))
         (frontier-shape (configuration-body (edge-before transition))))
       (match strategy
         [(search-strategy "dfs") '(More (Forced (Emit "A" (Emit "B" More))))]
         [_ '(More (Forced (Emit "B" More)))]))
      ;; The first commitment leaves a mature Search tail, never an
      ;; unevaluated sibling. Delayed bodies remain legitimately suspended.
      (define first-emission
        (edge-after (findf (lambda (transition)
                            (= 1 (length (answer-states (configuration-body (edge-after transition)))))) edges)))
      (check-match first-emission
                   `(program ,_ (Forced ,_ (Emit ,_ ,_ (commit ,(? scheduler:scheduler-value?))))))))

  (test-case "existing allocation witnesses retain native scope across scheduler forcing"
    (for* ([name '(unused-binder allocation-across-delay sparse-inherited-ancestry
                                answer-local-continuation delayed-sibling-capture)]
           [strategy (in-list all-surfaced-search-strategies)])
      (define example (findf (lambda (item) (eq? (witness-name item) name)) witnesses))
      (with-check-info (['witness name] ['strategy strategy])
        (define initial (open-goal strategy (witness-goal example)
                                   (witness-owners example) (witness-state example)))
        (define-values (final edges) (check-trace initial initial 120))
        (define scopes (map (lambda (node) (hash-ref node 'scope)) (model-session-current-answer-nodes final)))
        (check-equal? (sort (map length scopes) <)
                      (match name
                        [(or 'unused-binder 'allocation-across-delay) '(2)]
                        ['sparse-inherited-ancestry '(4)]
                        [(or 'answer-local-continuation 'delayed-sibling-capture) '(2 3)]))
        (when (eq? name 'sparse-inherited-ancestry)
          (check-equal? scopes '((9 2 7 0))))
        (when (eq? name 'allocation-across-delay)
          (define labels (map edge-name edges))
          (check-true (< (index-of labels "advance-delay")
                         (last (indexes-of labels "allocate-fresh"))))))))

  (test-case "pending conjunction candidates never enter the committed answer spine"
    (define goal
      '((((nat 0) =? (nat 0) (label "left")) ∨ ((nat 1) =? (nat 1) (label "right")) (label "choice"))
        ∧ (fail (label "pending")) (label "bind")))
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define initial (open-goal strategy goal))
      (define-values (final edges) (check-trace initial initial 80))
      (check-equal? (model-session-current-answer-nodes final) '())
      (check-equal? (meaningful-events strategy edges)
                    '((work "left") (work "right") (work "pending") (work "pending")))
      (for ([transition (in-list edges)])
        (check-equal? (answer-states (configuration-body (edge-after transition))) '()))
      (check-true (ormap (lambda (transition) (contains-constructor? (edge-after transition) 'One)) edges)))))

(module+ test
  (define failures (run-tests SCHEDULER-INTEGRATION))
  (unless (zero? failures)
    (error 'SCHEDULER-INTEGRATION "~a test case(s) failed" failures)))
