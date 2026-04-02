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
         "./test-http-helpers.rkt")

(define sample-tree
  '(() ((∃
          (x:q)
          ((sym "tree1") =? (sym "horse") (label "u5"))
          (label "f0"))
        (state () () () () (label "s")))))

(define step/const-tree-output
  (make-stepper (lambda (_) (list (list "foo" sample-tree)))))

(define streamed-answer-tree
  '(() ((succeed (label "ok")) (state () () () () (label "tail")))
       (⊤ (state () () () () (label "answer")))))

(define step/streamed-answer-output
  (make-stepper (lambda (_) (list (list "stream-step" streamed-answer-tree)))))

(define sample-program-jsexpr
  (hasheq 'children
          (list (hasheq 'id "u5"
                        'left (hasheq 'sym "tree1")
                        'name "Unify"
                        'right (hasheq 'sym "horse")))
          'disequalities '()
          'id "f0"
          'name "Fresh"
          'reified "_.0"
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
              (check-equal? name "Answer")
              (check-false (json-contains-name? program-json "Emit")))
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
              (define names (collect-step-names ses^ 24))
              (check-not-false (member "search-flip-fused-calls/delay-swap-left" names))
              (check-false (member "rail-fused-calls/enter-right" names)))
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
             (define names (collect-step-names ses^ 24))
             (check-not-false (member "search-flip-fused-calls/delay-swap-left" names))
             (check-not-false (member "delay/invoke-delay" names))
             (check-false (member "rail-fused-calls/enter-right" names))
             (check-false (member "rail-fused-calls/return-left" names)))

  (test-case "early rail strategy emits railroad rules and no flip rule"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^) (init! ses (make-post-init-request disj-delay-program #:strategy (search-strategy "early" "rail")) 'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (session-search-strategy ses^) (search-strategy "early" "rail"))
             (define names (collect-step-names ses^ 24))
             (check-not-false (member "rail-seq-calls/enter-right" names))
             (check-not-false (member "rail-seq-calls/return-left" names))
             (check-not-false (member "delay/invoke-delay" names))
             (check-false (member "search-flip-seq-calls/delay-swap-left" names)))

  (test-case "late dfs relcall-delay profile expands calls without eager/lazy resume rules"
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
             (define names (collect-step-names ses^ 24))
             (check-not-false (member "search-base-fused-calls/expand" names))
             (check-false (ormap (lambda (nm)
                                   (regexp-match? #rx"eager|lazy|proceed" nm))
                                 names)))

  (test-case "disj delay placement does not also suspend plain relcalls"
             (define ses (session (make-empty-zipper) step/const-tree-output 1 default-search-strategy))
             (define-values (response ses^)
               (init!
                ses
                (make-post-init-request
                 same-program
                 (hasheq 'text same-program
                         'sourceMode "mini"
                         'compileProfile (hasheq 'conjAssoc "right"
                                                 'disjAssoc "left"
                                                 'delayPlacement "disj"))
                 #:strategy (search-strategy "early" "rail"))
                'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (session-search-strategy ses^) (search-strategy "early" "rail"))
             (define names (collect-step-names ses^ 16))
             (check-not-false (member "delay/suspend-goal" names))
             (check-not-false (member "search-base-seq-calls/expand" names))
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
