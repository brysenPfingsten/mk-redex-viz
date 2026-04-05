#lang racket
(require rackunit
         rackunit/text-ui
         web-server/http/response-structs
         web-server/http/request-structs
         json
         "../src/app.rkt"
         "../src/search-strategy.rkt"
         "../src/zipper.rkt"
         "../src/transpiler.rkt"
         "./test-http-helpers.rkt"
         "./example-compat-tests.rkt")

(define sample-tree
  '(() ((∃
          (x:q)
          ((sym "tree1") =? (sym "horse") (label "u5"))
          (label "f0"))
        (state () () () () (label "s")))))

(define step/const-tree-output
  (make-stepper (lambda (_) (list (list "foo" sample-tree)))))

(define streamed-answer-tree
  '(() ((⊤ (state () () () () (label "answer")))
        + ((succeed (label "ok")) (state () () () () (label "tail"))))))

(define step/streamed-answer-output
  (make-stepper (lambda (_) (list (list "stream-step" streamed-answer-tree)))))

(define sample-program-jsexpr
  (hasheq 'activeChildIndex 0
          'children
          (list (hasheq 'id "u5"
                        'left (hasheq 'sym "tree1")
                        'name "Unify"
                        'renderRole "goal-leaf"
                        'right (hasheq 'sym "horse")))
          'disequalities '()
          'id "f0"
          'name "Fresh"
          'reified "_.0"
          'renderRole "goal-fresh"
          'stateId "s"
          'sub '()
          'trail '()
          'vars (list (hasheq 'var "q"))))

(define (check-sample-program-response response expected-step expected-step-name)
  (define payload (string->jsexpr (response-body->string response)))
  (match-define (hash* ['step step]
                       ['stepName step-name]
                       ['program program]
                       #:open)
    payload)
  (check-equal? step expected-step)
  (check-equal? step-name expected-step-name)
  (check-equal? (string->jsexpr program)
                sample-program-jsexpr))

(define (json-contains-name? node target)
  (match node
    [(hash* ['name name]
            ['children children]
            #:open)
     (or (equal? name target)
         (json-contains-name? children target))]
    [(hash* ['name name] #:open)
     (equal? name target)]
    [(list xs ...) (ormap (lambda (x) (json-contains-name? x target)) xs)]
    [_ #f]))

(define (json-contains-pair? node)
  (match node
    [(? hash? h)
     (or (hash-has-key? h 'pair)
         (for/or ([value (in-hash-values h)])
           (json-contains-pair? value)))]
    [(list xs ...)
     (for/or ([x (in-list xs)])
       (json-contains-pair? x))]
    [_ #f]))

(define (json-strip-spine node)
  (match node
    [(hash* ['name name]
            ['children (list child)]
            #:open)
     #:when (member name '("Deferred" "Freshened"))
     (json-strip-spine child)]
    [_ node]))

(define (json-root-name node)
  (match-define (hash* ['name name] #:open)
    (json-strip-spine node))
  name)

(define (json-live-search-root node)
  (match (json-strip-spine node)
    [(hash* ['name "Emit"]
            ['children (list _answer rest)]
            #:open)
     (json-live-search-root rest)]
    [other other]))

(define (json-live-search-root-name node)
  (match-define (hash* ['name name] #:open)
    (json-live-search-root node))
  name)

(define (collect-json-ids node [acc '()])
  (match node
    [(hash* ['id id]
            ['children children]
            #:open)
     (collect-json-ids children (cons id acc))]
    [(hash* ['children children] #:open)
     (collect-json-ids children acc)]
    [(list xs ...)
     (for/fold ([ids acc]) ([x (in-list xs)])
       (collect-json-ids x ids))]
    [_ acc]))

(define (json-id-counts node [acc (hash)])
  (match node
    [(hash* ['id id]
            ['children children]
            #:open)
     (json-id-counts children
                     (hash-update acc id add1 0))]
    [(hash* ['children children] #:open)
     (json-id-counts children acc)]
    [(list xs ...)
     (for/fold ([counts acc]) ([x (in-list xs)])
       (json-id-counts x counts))]
    [_ acc]))

(define (duplicate-json-ids node)
  (for/list ([(id count) (in-dict (json-id-counts node))]
             #:when (> count 1))
    id))

(define (json-node-names-by-id node target-id [acc '()])
  (match node
    [(hash* ['id id]
            ['name name]
            ['children children]
            #:open)
     (define next-acc
       (if (equal? id target-id)
           (cons name acc)
           acc))
     (json-node-names-by-id children target-id next-acc)]
    [(hash* ['children children] #:open)
     (json-node-names-by-id children target-id acc)]
    [(list xs ...)
     (for/fold ([names acc]) ([x (in-list xs)])
       (json-node-names-by-id x target-id names))]
    [_ acc]))

(define (all-json-ids-appear-in-source? source node)
  (for/and ([id (in-list (collect-json-ids node))])
    (regexp-match? (regexp-quote (format "[[~a]]" id)) source)))

(define disj-delay-program
  "(defrel (same x y)
     (== x y))

   (run 2 (q)
     (conde
       [(same q 'cat)]
       [(same q 'dog)]))")

(define same-program
  "(defrel (same x y)
     (== x y))

   (run* (q)
     (conde
       [(conde
          [(same q 'turtle)]
          [(same q 'cat)]
       [(== q 'dog)])]
      [(same q 'fish)]))")

(define hoist-witness-micro-program
  "(run 2 (q)
     (conj
       (disj
         (== q 'hoist)
         (== q 'witness))
       (== q q)))")

(define disj-relcall-program
  "(defrel (same x y)
     (== x y))

   (defrel (wrap x)
     (== x x)
     (same x 'cat))

   (run* (q)
     (conde
       [(wrap q)]
       [(== q 'dog)]))")

(define dotted-pair-program
  "(run 1 (q r)
     (== q (cons 'left 'right))
     (== r 'done))")

(define source-derived-names
  '("Fresh" "Goal-Conj" "Goal-Disj" "Goal-Delay" "Rel-Call" "Unify" "Disequality"))

(define MIN-DEEP-TRACE-STEPS 20)
(define SCOPED-VISIBLE-PAIR-CAP 120)
(define STRATEGY-WITNESS-CAP 120)
(define PAIR-WITNESS-CAP 12)
(define DEEP-TRACE-CAP 64)

(define (render-source->micro src)
  (define response (source-convert! (make-post-source-convert-request src)))
  (check-equal? (response-code response) 200)
  (define body (string->jsexpr (response-body->string response)))
  (match-define (hash* ['source rendered] #:open) body)
  rendered)

(define (find-source-duplicate-ids ses [remaining 80])
  (cond
    [(zero? remaining) #f]
    [else
     (define-values (step-response ses^) (step! ses))
     (match (response-body->string step-response)
       ["null" #f]
       [out
        (define payload (string->jsexpr out))
        (match-define (hash* ['program program] #:open) payload)
        (define program-json (string->jsexpr program))
        (define duplicates (duplicate-json-ids program-json))
        (define source-duplicates
          (for/list ([id (in-list duplicates)]
                     #:when
                     (for/or ([name (in-list (json-node-names-by-id program-json id))])
                       (member name source-derived-names)))
            id))
        (if (null? source-duplicates)
            (find-source-duplicate-ids ses^ (sub1 remaining))
            source-duplicates)])]))

(define (collect-step-names ses remaining)
  (cond
    [(zero? remaining) '()]
    [else
     (define-values (response ses^) (step! ses))
     (match (response-body->string response)
       ["null" '()]
       [out
        (match-define (hash* ['stepName step-name] #:open) (string->jsexpr out))
        (cons step-name
              (collect-step-names ses^
                                  (sub1 remaining)))])]))

(define (nth-step-payload ses n)
  (define-values (response ses^) (step! ses))
  (match (response-body->string response)
    ["null"
     (values #f ses^)]
    [out
     (define payload (string->jsexpr out))
     (if (zero? n)
         (values payload ses^)
         (nth-step-payload ses^ (sub1 n)))]))

(define (collect-step-payloads ses remaining)
  (cond
    [(zero? remaining) '()]
    [else
     (define-values (response ses^) (step! ses))
     (match (response-body->string response)
       ["null" '()]
       [out
        (define payload (string->jsexpr out))
        (cons payload
              (collect-step-payloads ses^
                                     (sub1 remaining)))])]))

(define (payload->program-json payload)
  (string->jsexpr (hash-ref payload 'program)))

(define (payload-contains-name? payload target)
  (json-contains-name? (payload->program-json payload) target))

(define (scoped-visible-change? left right [left-step-name #f])
  (define left-program (payload->program-json left))
  (define right-program (payload->program-json right))
  (and (or (not left-step-name)
           (equal? (hash-ref left 'stepName) left-step-name))
       (json-contains-name? left-program "Freshened")
       (json-contains-name? right-program "Freshened")
       (not (equal? left-program right-program))))

(define (collect-scoped-visible-changes payloads [acc '()])
  (match payloads
    ['() (reverse acc)]
    [(list _) (reverse acc)]
    [(cons left (cons right rest))
     (define next-acc
       (if (scoped-visible-change? left right)
           (cons (list left right) acc)
           acc))
     (collect-scoped-visible-changes (cons right rest) next-acc)]))

(define (count-pair-payloads payloads [acc 0])
  (match payloads
    ['() acc]
    [(cons payload rest)
     (define next-acc
       (if (json-contains-pair? (payload->program-json payload))
           (add1 acc)
           acc))
     (count-pair-payloads rest next-acc)]))

(define (example-src label)
  (for/first ([pr (in-list (frontend-example-programs))]
              #:do [(match-define (cons example-label src) pr)]
              #:when (equal? example-label label))
    src))

(define-test-suite STEP!
  #:before (thunk (displayln "Running tests for step!..."))
  #:after  (thunk (displayln "Finished running tests for step!"))

  (test-case "step! sends null reponse with header if no more reductions and does not affect zipper"
              (define zip (zipper '() (step "foo" '(() ())) '() 1))
              (define stepper (make-stepper (λ (_) '())))
              (define ses (session zip stepper 1 default-search-strategy))
              (define-values (response ses^) (step! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response)
                            (list (make-header #"X-Done" #"true")))
              (check-equal? (response-body->string response) "null")
              (match-define (session new-zipper _ _ _) ses^)
              (check-equal? zip new-zipper))

  (test-case "step! advances via stepper when no future cache and updates state"
              (define zip (zipper '() (step "foo" sample-tree) '() 1))
              (define stepper step/const-tree-output)
              (define ses (session zip stepper 1 default-search-strategy))
              (define-values (response ses^) (step! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-sample-program-response response 2 "foo")
              (match-define (session (zipper prev curr next idx) _ _ _) ses^)
              (check-equal? prev (list (step "foo" sample-tree)))
              (check-equal? (step-name curr) "foo")
              (check-equal? next '())
              (check-equal? idx 2))

  (test-case "step! gets next tree in the state if it is cached and updates state"
              (define zip (zipper '() (step "foo" sample-tree) (list (step "bar" sample-tree)) 1))
              (define stepper step/const-tree-output)
              (define ses (session zip stepper 1 default-search-strategy))
              (define-values (response ses^) (step! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-sample-program-response response 2 "bar")
              (match-define (session (zipper prev curr next idx) _ _ _) ses^)
              (check-equal? prev (list (step "foo" sample-tree)))
              (check-equal? curr (step "bar" sample-tree))
              (check-equal? next '())
              (check-equal? idx 2)
              )

  (test-case "step! serializes top-level answer stream ahead of remaining work"
              (define zip (zipper '() (step "foo" sample-tree) '() 1))
              (define ses (session zip step/streamed-answer-output 1 default-search-strategy))
              (define-values (response _ses^) (step! ses))
              (check-equal? (response-code response) 200)
              (define payload (string->jsexpr (response-body->string response)))
              (match-define (hash* ['program program] #:open) payload)
              (define program-json (string->jsexpr program))
              (match-define (hash* ['name name] #:open) program-json)
              (check-equal? name "Emit")
              (check-true (json-contains-name? program-json "Answer")))
)

(define-test-suite INIT!
  #:before (thunk (displayln "Running tests for init!..."))
  #:after (thunk (displayln "Finished running tests for init!."))

  (test-case "init! parses, updates state, and sends response with json and string prog"
              (define sample-req (make-post-init-request "(run* (q) (== 'a 'a))"))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response _ses^) (init! ses sample-req 'testid))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) 
                            APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) 
                            (list (header #"Set-Cookie" #"session-id=testid; Path=/; SameSite=Lax")))
              (define json-response (string->jsexpr (response-body->string response)))
              (match-define (hash* ['stepName step-name]
                                   ['step step]
                                   ['program program]
                                   ['htmlGuids html-guids]
                                   #:open)
                json-response)
              (check-equal? step-name "Initialize Program")
              (check-equal? step 0)
              (check-not-false program)
              (check-not-false html-guids))

  (test-case "init! defaults missing source options to canonical mini profile"
              (define sample-req
                (make-post-init-request
                 "(run* (q) (== 'a 'a))"
                 (hasheq 'text "(run* (q) (== 'a 'a))")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response _ses^) (init! ses sample-req 'defaultid))
              (check-equal? (response-code response) 200)
              (define json-response (string->jsexpr (response-body->string response)))
              (match-define (hash* ['stepName step-name]
                                   ['step step]
                                   ['program program]
                                   ['htmlGuids html-guids]
                                   #:open)
                json-response)
              (check-equal? step-name "Initialize Program")
              (check-equal? step 0)
              (check-not-false program)
              (check-not-false html-guids))

  (test-case "init! serializes direct micro Zzz as goal delay"
              (define sample-req
                (make-post-init-request
                 "(run* (q) (Zzz (== q 'cat)))"
                 (hasheq 'text "(run* (q) (Zzz (== q 'cat)))"
                         'sourceMode "micro")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response _ses^) (init! ses sample-req 'goal-delay-id))
              (check-equal? (response-code response) 200)
              (define payload (string->jsexpr (response-body->string response)))
              (match-define (hash* ['program program] #:open) payload)
              (define program-json (string->jsexpr program))
              (match-define (hash* ['name name] #:open) program-json)
              (check-true (json-contains-name? program-json "Goal-Delay"))
              (check-false (equal? name "Delay")))

  (test-case "init!/step! preserve source ids across tagged source and tree JSON"
              (define sample-req
                (make-post-init-request same-program))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'source-id-test))
              (define init-payload (string->jsexpr (response-body->string response)))
              (match-define (hash* ['program init-program]
                                   ['htmlGuids html-guids]
                                   #:open)
                init-payload)
              (check-true
               (all-json-ids-appear-in-source? html-guids
                                               (string->jsexpr init-program)))
              (define-values (step-response _ses^^) (step! ses^))
              (define step-payload (string->jsexpr (response-body->string step-response)))
              (match-define (hash* ['program step-program] #:open) step-payload)
              (check-true
               (all-json-ids-appear-in-source? html-guids
                                               (string->jsexpr step-program))))

  (test-case "init!/step! preserve source ids across tagged source and tree JSON for direct micro source"
              (define sample-req
                (make-post-init-request
                 hoist-witness-micro-program
                 (hasheq 'text hoist-witness-micro-program
                         'sourceMode "micro")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'micro-source-id-test))
              (define init-payload (string->jsexpr (response-body->string response)))
              (match-define (hash* ['program init-program]
                                   ['htmlGuids html-guids]
                                   #:open)
                init-payload)
              (check-true
               (all-json-ids-appear-in-source? html-guids
                                               (string->jsexpr init-program)))
              (define-values (step-response _ses^^) (step! ses^))
              (define step-payload (string->jsexpr (response-body->string step-response)))
              (match-define (hash* ['program step-program] #:open) step-payload)
              (check-true
               (all-json-ids-appear-in-source? html-guids
                                               (string->jsexpr step-program))))

  (test-case "appendoh 2 produces repeated RHS nodes that share one source UUID"
              (define sample-req
                (make-post-init-request (example-src "appendoh 2")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'repeated-source-id-test))
              (define init-payload (string->jsexpr (response-body->string response)))
              (match-define (hash* ['htmlGuids html-guids] #:open) init-payload)
              (define repeated-source-ids (find-source-duplicate-ids ses^))
              (when repeated-source-ids
                (for ([id (in-list repeated-source-ids)])
                  (check-true
                   (regexp-match? (regexp-quote (format "[[~a]]" id))
                                  html-guids)))))

  (test-case "rendered micro appendoh 2 produces repeated RHS nodes that share one source UUID"
              (define micro-src
                (render-source->micro (example-src "appendoh 2")))
              (define sample-req
                (make-post-init-request
                 micro-src
                 (hasheq 'text micro-src
                         'sourceMode "micro")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'repeated-micro-source-id-test))
              (define init-payload (string->jsexpr (response-body->string response)))
              (match-define (hash* ['htmlGuids html-guids] #:open) init-payload)
              (define repeated-source-ids (find-source-duplicate-ids ses^))
              (when repeated-source-ids
                (for ([id (in-list repeated-source-ids)])
                  (check-true
                   (regexp-match? (regexp-quote (format "[[~a]]" id))
                                  html-guids)))))

  (test-case "micro hoist witness changes visible tree on adjacent rail steps"
              (define sample-req
                (make-post-init-request
                 hoist-witness-micro-program
                 (hasheq 'text hoist-witness-micro-program
                         'sourceMode "micro")
                 #:strategy (search-strategy "early" "rail")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'hoist-witness-id))
              (check-equal? (response-code response) 200)
              (define-values (step1-payload ses1) (nth-step-payload ses^ 0))
              (define-values (step2-payload _ses2) (nth-step-payload ses1 0))
              (check-not-false step1-payload)
              (check-not-false step2-payload)
              (match-define (hash* ['step step1]
                                   ['program program1]
                                   #:open)
                step1-payload)
              (match-define (hash* ['step step2]
                                   ['program program2]
                                   #:open)
                step2-payload)
              (check-equal? step1 1)
              (check-equal? step2 2)
              (check-false (equal? (string->jsexpr program1)
                                   (string->jsexpr program2))))

  (test-case "fives/fours trace contains multiple scoped adjacent UI changes"
              (define sample-req
                (make-post-init-request (example-src "fives/fours")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'fives-fours-visible-id))
              (check-equal? (response-code response) 200)
              (define scoped-changes
                (collect-scoped-visible-changes
                 (collect-step-payloads ses^ SCOPED-VISIBLE-PAIR-CAP)))
              (check-true (>= (length scoped-changes) 2)))

  (test-case "fives/fours keeps scoped visible change across a delay boundary"
              (define sample-req
                (make-post-init-request (example-src "fives/fours")))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'fives-fours-debug-id))
              (check-equal? (response-code response) 200)
              (define scoped-bounced-pair
                (for/first ([pair (in-list (collect-scoped-visible-changes
                                            (collect-step-payloads ses^ SCOPED-VISIBLE-PAIR-CAP)))]
                            #:do [(match-define (list left right) pair)]
                            #:when (or (payload-contains-name? left "Deferred")
                                       (payload-contains-name? right "Deferred")))
                  pair))
              (check-not-false scoped-bounced-pair))

  (test-case "dotted-pair witness eventually serializes dotted-pair reifications"
              (define sample-req
                (make-post-init-request dotted-pair-program))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'dotted-pair-id))
              (check-equal? (response-code response) 200)
              (define pair-payload-count
                (count-pair-payloads
                 (collect-step-payloads ses^ PAIR-WITNESS-CAP)))
              (check-true (positive? pair-payload-count)))

  (test-case "div3o stays JSON-serializable through a deep default trace"
              (define sample-req
                (make-post-init-request (example-src "div3o")))
              (define ses0 (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses1) (init! ses0 sample-req 'div3o-deep-id))
              (check-equal? (response-code response) 200)
              (define (loop ses remaining [seen 0])
                (cond
                  [(zero? remaining)
                   (check-true (>= seen MIN-DEEP-TRACE-STEPS))]
                  [else
                   (define-values (step-response ses^) (step! ses))
                   (define out (response-body->string step-response))
                   (cond
                     [(equal? out "null")
                     (check-true (>= seen MIN-DEEP-TRACE-STEPS))]
                     [else
                      (define payload (string->jsexpr out))
                      (assert-step-payload-shape payload
                                                 (format "div3o deep step ~a" seen))
                      (loop ses^ (sub1 remaining) (add1 seen))])]))
              (loop ses1 DEEP-TRACE-CAP))

  (test-case "init! throws error if program is not syntactically correct"
              (define sample-req (make-post-init-request "(run* (== 'a 'a))"))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (check-exn exn:fail:syntax?
                         (thunk
                          (call-with-values
                           (lambda () (init! ses sample-req 'testid))
                           list))))

  (test-case "init! defaults missing searchStrategy in payload"
              (define sample-req
                (make-post-request "init"
                                   (hasheq 'text "(run* (q) (== q 'ok))"
                                           'sourceMode "mini"
                                           'compileProfile (hash-ref default-source-options 'compileProfile))))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (define-values (response ses^) (init! ses sample-req 'default-strategy-id))
              (check-equal? (response-code response) 200)
              (check-equal? (session-search-strategy ses^) default-search-strategy))

  (test-case "init! rejects invalid searchStrategy hoist in payload"
              (define sample-req
                (make-post-init-request
                 "(run* (q) (== q 'ok))"
                 (hasheq 'text "(run* (q) (== q 'ok))"
                         'sourceMode "mini"
                         'compileProfile (hash-ref default-source-options 'compileProfile)
                         'searchStrategy (hasheq 'hoist "sideways"
                                                 'scheduler "rail"))))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (check-exn exn:fail?
                         (thunk
                          (call-with-values
                           (lambda () (init! ses sample-req 'invalid-hoist-id))
                           list))))

  (test-case "init! rejects invalid searchStrategy scheduler in payload"
              (define sample-req
                (make-post-init-request
                 disj-delay-program
                 (hasheq 'text disj-delay-program
                         'sourceMode "mini"
                         'compileProfile (hash-ref default-source-options 'compileProfile)
                         'searchStrategy (hasheq 'hoist "late"
                                                 'scheduler "zigzag"))))
              (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
              (check-exn exn:fail?
                         (thunk
                          (call-with-values
                           (lambda () (init! ses sample-req 'invalid-scheduler-id))
                           list))))

  (test-case "init! accepts searchStrategy payload and updates session state"
              (define ses (session (make-empty-zipper)
                                   step/const-tree-output
                                   1
                                   default-search-strategy))
              (define-values (response ses^)
                (init!
                 ses
                 (make-post-init-request
                  disj-delay-program
                  #:strategy (search-strategy "late" "flip"))
                 'init-search-strategy-id))
              (check-equal? (response-code response) 200)
              (check-equal? (session-search-strategy ses^)
                            (search-strategy "late" "flip"))
              (define names (collect-step-names ses^ STRATEGY-WITNESS-CAP))
              (check-not-false (member "delay-swap-left" names))
              (check-false (member "enter-right-at-branch" names))
              (check-false (member "enter-right-through-scoped-delay" names)))
  )

(define-test-suite RESET!
  #:before (thunk (displayln "Running tests for reset!..."))
  #:after (thunk (displayln "Finished running tests for reset."))

  (test-case "reset! empties state and sends initial state when it has a prev cache"
             (define zip (zipper (list 'a 'b 'c (step "Initialize Program" sample-tree))
                                   'd '() 5))
             (define stepper identity)
             (define ses (session zip stepper 1 default-search-strategy))
             (define-values (response ses^) (reset! ses))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) 
                           APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response) 
                           (list (header #"X-Is-Start" #"true")))
             (check-sample-program-response response 0 "Initialize Program")
             (match-define (session (zipper prev curr next idx) _ _ _) ses^)
             (check-equal? prev '())
             (check-equal? curr (step "Initialize Program" sample-tree))
             (check-equal? next '())
             (check-equal? idx 0))


  (test-case "reset! empties state and sends current state with header when it doesn't have a prev cache"
             (define zip (zipper '() (step "Initialize Program" sample-tree) '() 0))
             (define stepper identity)
             (define ses (session zip stepper 1 default-search-strategy))
             (define-values (response ses^) (reset! ses))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response)
                           (list (make-header #"X-Is-Start" #"true")))
             (check-sample-program-response response 0 "Initialize Program")
             (check-equal? (session-zipper ses^) zip))

  (test-case "reset! restores the initial visible program after real search steps"
             (define sample-req
               (make-post-init-request (example-src "fives/fours")))
             (define ses (session (make-empty-zipper) identity 1 default-search-strategy))
             (define-values (init-response ses^) (init! ses sample-req 'reset-real-id))
             (define init-payload (string->jsexpr (response-body->string init-response)))
             (match-define (hash* ['program init-program] #:open) init-payload)
             (define-values (_step1 ses1) (step! ses^))
             (define-values (_step2 ses2) (step! ses1))
             (define-values (reset-response _ses3) (reset! ses2))
             (check-equal? (response-code reset-response) 200)
             (check-equal? (response-headers reset-response)
                           (list (make-header #"X-Is-Start" #"true")))
             (define reset-payload (string->jsexpr (response-body->string reset-response)))
             (match-define (hash* ['step step]
                                  ['stepName step-name]
                                  ['program reset-program]
                                  #:open)
               reset-payload)
             (check-equal? step 0)
             (check-equal? step-name "Initialize Program")
             (check-equal? (string->jsexpr reset-program)
                           (string->jsexpr init-program)))
  )

(define-test-suite BACK!
  #:before (thunk (displayln "Running tests for back!..."))
  #:after (thunk (displayln "Finished running tests for back!."))

  (test-case "back! sends initial state with header when only one thing in prev cache and updates state"
             (define zip (zipper (list (step "Initialize Program" sample-tree))
                                 (step "next" sample-tree)
                                 '()
                                 1))
             (define stepper identity)
             (define ses (session zip stepper 1 default-search-strategy))
             (define-values (response ses^) (back! ses))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response)
                           (list (header #"X-Is-Start" #"true")))
             (check-sample-program-response response 0 "Initialize Program")
             (match-define (session (zipper prev curr next idx) _ _ _) ses^)
             (check-equal? prev '())
             (check-equal? curr (step "Initialize Program" sample-tree))
             (check-equal? next (list (step "next" sample-tree)))
             (check-equal? idx 0))

  (test-case "back! sends initial state when multiple things in prev cache and updates state"
              (define zip (zipper (list (step "Initialize Program" sample-tree)
                                        (step "test1" sample-tree))
                                  (step "test2" sample-tree)
                                  '()
                                  2))
              (define stepper identity)
              (define ses (session zip stepper 1 default-search-strategy))
              (define-values (response ses^) (back! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-sample-program-response response 1 "Initialize Program")
              (match-define (session (zipper prev curr next idx) _ _ _) ses^)
              (check-equal? prev (list (step "test1" sample-tree)))
              (check-equal? curr (step "Initialize Program" sample-tree))
              (check-equal? next (list (step "test2" sample-tree)))
              (check-equal? idx 1))
  )

(define-test-suite INIT-SEARCH-STRATEGY!
  #:before (thunk (displayln "Running tests for init search strategy binding!..."))
  #:after (thunk (displayln "Finished running tests for init search strategy binding!."))

  (test-case "late flip strategy emits flip rules and no rail rules"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^) (init! ses (make-post-init-request disj-delay-program #:strategy (search-strategy "late" "flip")) 'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (session-search-strategy ses^) (search-strategy "late" "flip"))
             (define names (collect-step-names ses^ STRATEGY-WITNESS-CAP))
             (check-not-false (member "delay-swap-left" names))
             (check-not-false (member "invoke-delay" names))
             (check-false (member "enter-right-at-branch" names))
             (check-false (member "enter-right-through-scoped-delay" names))
             (check-false (member "return-left-at-branch" names))
             (check-false (member "return-left-through-scoped-delay" names)))

  (test-case "late flip hoist witness continues to a fourth step"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^)
               (init!
                ses
                (make-post-init-request
                 hoist-witness-micro-program
                 (hasheq 'text hoist-witness-micro-program
                         'sourceMode "micro")
                 #:strategy (search-strategy "late" "flip"))
                'hoist-witness-late-flip-id))
             (check-equal? (response-code response) 200)
             (define-values (payload _ses^^) (nth-step-payload ses^ 3))
             (check-not-false payload)
             (match-define (hash* ['step step]
                                  ['stepName step-name]
                                  #:open)
               payload)
             (check-equal? step 4)
             (check-equal? step-name "unify-success"))

  (test-case "early rail strategy binds the session and keeps flip rules absent"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^) (init! ses (make-post-init-request (example-src "fives/fours") #:strategy (search-strategy "early" "rail")) 'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (session-search-strategy ses^) (search-strategy "early" "rail"))
             (define names (collect-step-names ses^ STRATEGY-WITNESS-CAP))
             (check-not-false (member "invoke-delay" names))
             (check-false (member "delay-swap-left" names)))

  (test-case "early rail delayed disjunction expands the right relcall after invoke-delay"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^)
               (init! ses
                      (make-post-init-request disj-delay-program
                                              #:strategy (search-strategy "early" "rail"))
                      'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (session-search-strategy ses^) (search-strategy "early" "rail"))
             (define names (collect-step-names ses^ STRATEGY-WITNESS-CAP))
             (check-not-false (member "enter-right-at-branch" names))
             (check-not-false (member "invoke-delay" names))
             (check-true (>= (length (filter (lambda (name)
                                               (equal? name "expand-relcall"))
                                             names))
                             2)))

  (test-case "late dfs relcall-delay profile expands relcall without eager/lazy resume rules"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^)
               (init!
                ses
                (make-post-init-request
                 disj-delay-program
                 (hasheq 'text disj-delay-program
                         'sourceMode "mini"
                         'compileProfile (hasheq 'conjAssoc "left"
                                                 'disjAssoc "right"
                                                 'delayPlacement "relcall"))
                 #:strategy (search-strategy "late" "dfs"))
                'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (session-search-strategy ses^) (search-strategy "late" "dfs"))
             (define names (collect-step-names ses^ STRATEGY-WITNESS-CAP))
             (check-not-false (member "expand-relcall" names))
             (check-false (ormap (lambda (nm)
                                   (regexp-match? #rx"eager|lazy|proceed" nm))
                                 names)))

  (test-case "disj delay placement does not also suspend plain relcalls"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^)
               (init!
                ses
                (make-post-init-request
                 disj-relcall-program
                 (hasheq 'text disj-relcall-program
                         'sourceMode "mini"
                         'compileProfile (hasheq 'conjAssoc "right"
                                                 'disjAssoc "left"
                                                 'delayPlacement "disj"))
                 #:strategy (search-strategy "early" "rail"))
                'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (session-search-strategy ses^) (search-strategy "early" "rail"))
             (define names (collect-step-names ses^ STRATEGY-WITNESS-CAP))
             (check-not-false (member "suspend-goal" names))
             (check-not-false (member "expand-relcall" names))
             (check-false (ormap (lambda (nm)
                                   (regexp-match? #rx"eager|lazy|proceed" nm))
                                 names))))

(define-test-suite SOURCE-CONVERT!
  (test-case "source-convert! lowers mini source to direct micro source with Zzz"
             (define req
               (make-post-source-convert-request
                "(defrel (same x y) (== x y))
                 (run* (q)
                   (conde
                     [(same q 'cat)]
                     [(same q 'dog)]))"))
             (define response (source-convert! req))
             (check-equal? (response-code response) 200)
             (define body (string->jsexpr (response-body->string response)))
             (match-define (hash* ['source rendered] #:open) body)
             (check-true (string? rendered))
             (check-not-false (regexp-match? #rx"Zzz" rendered)))

  (test-case "source-convert! rejects unsupported target source modes"
             (define req
               (make-post-source-convert-request
                "(run* (q) (== q 'cat))"
                (hasheq 'text "(run* (q) (== q 'cat))"
                        'sourceMode "mini"
                        'compileProfile (hash-ref default-source-options 'compileProfile)
                        'targetSourceMode "mini")))
             (check-exn exn:fail?
                        (lambda () (source-convert! req)))))

(define/provide-test-suite APP
  #:before (thunk (displayln "Running tests for app.rkt..."))
  #:after (thunk (displayln "Finished running tests for app.rkt"))
  STEP!
  INIT!
  RESET!
  BACK!
  INIT-SEARCH-STRATEGY!
  SOURCE-CONVERT!
)

(run-tests APP)
