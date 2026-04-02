#lang racket
(require rackunit
         rackunit/text-ui
         web-server/http/response-structs
         web-server/http/request-structs
         json
         "../src/app.rkt"
         "../src/model-surface-policy.rkt"
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
  (check-equal? (hash-ref payload 'step #f) expected-step)
  (check-equal? (hash-ref payload 'stepName #f) expected-step-name)
  (check-equal? (string->jsexpr (hash-ref payload 'program #f))
                sample-program-jsexpr))

(define (json-contains-name? node target)
  (match node
    [(? hash? h)
     (or (equal? (hash-ref h 'name #f) target)
         (json-contains-name? (hash-ref h 'children '()) target))]
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

(define (collect-step-names ses limit [i 0] [acc '()])
  (cond
    [(>= i limit) (reverse acc)]
    [else
     (define response (step! ses))
     (define out (response-body->string response))
     (if (string=? out "null")
         (reverse acc)
         (collect-step-names ses
                             limit
                             (add1 i)
                             (cons (hash-ref (string->jsexpr out) 'stepName #f)
                                   acc)))]))

(define-test-suite STEP!
  #:before (thunk (displayln "Running tests for step!..."))
  #:after  (thunk (displayln "Finished running tests for step!"))

  (test-case "step! sends null reponse with header if no more reductions and does not affect zipper"
              (define zip (zipper '() (step "foo" '(() ())) '() 1))
              (define stepper (make-stepper (λ (_) '())))
              (define ses (session zip stepper 1))
              (define response (step! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response)
                            (list (make-header #"X-Done" #"true")))
              (check-equal? (response-body->string response) "null")
              (define new-zipper (session-zipper ses))
              (check-equal? zip new-zipper))

  (test-case "step! advances via stepper when no future cache and updates state"
              (define zip (zipper '() (step "foo" sample-tree) '() 1))
              (define stepper step/const-tree-output)
              (define ses (session zip stepper 1))
              (define response (step! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-sample-program-response response 2 "foo")
              (define new-zipper (session-zipper ses))
              (check-equal? (zipper-prev new-zipper) (list (step "foo" sample-tree)))
              (check-equal? (step-name (zipper-curr new-zipper)) "foo")
              (check-equal? (zipper-next new-zipper) '())
              (check-equal? (zipper-idx   new-zipper) 2))

  (test-case "step! gets next tree in the state if it is cached and updates state"
              (define zip (zipper '() (step "foo" sample-tree) (list (step "bar" sample-tree)) 1))
              (define stepper step/const-tree-output)
              (define ses (session zip stepper 1))
              (define response (step! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-sample-program-response response 2 "bar")
              (define new-zipper (session-zipper ses))
              (check-equal? (zipper-prev new-zipper) (list (step "foo" sample-tree)))
              (check-equal? (zipper-curr new-zipper) (step "bar" sample-tree))
              (check-equal? (zipper-next new-zipper) '())
              (check-equal? (zipper-idx   new-zipper) 2)
              )

  (test-case "step! serializes top-level answer stream ahead of remaining work"
              (define zip (zipper '() (step "foo" sample-tree) '() 1))
              (define ses (session zip step/streamed-answer-output 1))
              (define response (step! ses))
              (check-equal? (response-code response) 200)
              (define payload (string->jsexpr (response-body->string response)))
              (define program-json (string->jsexpr (hash-ref payload 'program #f)))
              (check-equal? (hash-ref program-json 'name #f) "Answer")
              (check-false (json-contains-name? program-json "Emit")))
)

(define-test-suite INIT!
  #:before (thunk (displayln "Running tests for init!..."))
  #:after (thunk (displayln "Finished running tests for init!."))

  (test-case "init! parses, updates state, and sends response with json and string prog"
              (define sample-req (make-post-init-request "(run* (q) (== 'a 'a))"))
              (define zip (zipper '() #f '() 0))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (define response (init! ses sample-req 'testid))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) 
                            APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) 
                            (list (header #"Set-Cookie" #"session-id=testid; Path=/; SameSite=Lax")))
              (define json-response (string->jsexpr (response-body->string response)))
              (check-equal? (hash-ref json-response 'stepName #f) "Initialize Program")
              (check-equal? (hash-ref json-response 'step #f) 0)
              (check-not-false (hash-ref json-response 'program #f))
              (check-not-false (hash-ref json-response 'htmlGuids #f)))

  (test-case "init! defaults missing source options to canonical mini profile"
              (define sample-req
                (make-post-init-request
                 "(run* (q) (== 'a 'a))"
                 (hasheq 'text "(run* (q) (== 'a 'a))")))
              (define zip (zipper '() #f '() 0))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (define response (init! ses sample-req 'defaultid))
              (check-equal? (response-code response) 200)
              (define json-response (string->jsexpr (response-body->string response)))
              (check-equal? (hash-ref json-response 'stepName #f) "Initialize Program")
              (check-equal? (hash-ref json-response 'step #f) 0)
              (check-not-false (hash-ref json-response 'program #f))
              (check-not-false (hash-ref json-response 'htmlGuids #f)))

  (test-case "init! serializes direct micro Zzz as goal delay"
              (define sample-req
                (make-post-init-request
                 "(run* (q) (Zzz (== q 'cat)))"
                 (hasheq 'text "(run* (q) (Zzz (== q 'cat)))"
                         'sourceMode "micro")))
              (define zip (zipper '() #f '() 0))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (define response (init! ses sample-req 'goal-delay-id))
              (check-equal? (response-code response) 200)
              (define payload (string->jsexpr (response-body->string response)))
              (define program-json (string->jsexpr (hash-ref payload 'program #f)))
              (check-true (json-contains-name? program-json "Goal-Delay"))
              (check-false (equal? (hash-ref program-json 'name #f) "Delay")))

  (test-case "init! throws error if program is not syntactically correct"
              (define sample-req (make-post-init-request "(run* (== 'a 'a))"))
              (define zip (zipper '() #f '() 0))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (check-exn exn:fail:syntax? (thunk (init! ses sample-req 'testid))))

  (test-case "init! rejects missing model in payload"
              (define sample-req
                (make-post-request "init"
                                   (hasheq 'text "(run* (q) (== q 'ok))"
                                           'sourceMode "mini"
                                           'compileProfile (hash-ref default-source-options 'compileProfile))))
              (define zip (zipper '() #f '() 0))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (check-exn exn:fail?
                         (thunk (init! ses sample-req 'missing-model-id))))

  (test-case "init! rejects unknown model in payload"
              (define sample-req
                (make-post-init-request
                 "(run* (q) (== q 'ok))"
                 #:model "nope"))
              (define zip (zipper '() #f '() 0))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (check-exn exn:fail?
                         (thunk (init! ses sample-req 'unknown-model-id))))

  (test-case "init! rejects program incompatible with selected model payload"
              (define sample-req
                (make-post-init-request disj-delay-program #:model "mk-l0-core"))
              (define zip (zipper '() #f '() 0))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (check-exn exn:fail?
                         (thunk (init! ses sample-req 'incompat-id))))

  (test-case "init! accepts model payload and updates session state"
              (define zip (zipper '() #f '() 0))
              (define ses (session zip step/const-tree-output 1))
              (define response
                (init!
                 ses
                 (make-post-init-request
                  disj-delay-program
                  #:model "mk-l3-flip-lazy")
                 'init-model-id))
              (check-equal? (response-code response) 200)
              (check-equal? (session-model-id ses) "mk-l3-flip-lazy")
              (define names (collect-step-names ses 24))
              (check-not-false (member "flip/delay-swap-left" names))
              (check-false (member "rail/enter-right" names)))
  )

(define-test-suite RESET!
  #:before (thunk (displayln "Running tests for reset!..."))
  #:after (thunk (displayln "Finished running tests for reset."))

  (test-case "reset! empties state and sends initial state when it has a prev cache"
             (define zip (zipper (list 'a 'b 'c (step "Initialize Program" sample-tree))
                                   'd '() 5))
             (define stepper identity)
             (define ses (session zip stepper 1))
             (define ses-table (make-hash))
             (hash-set! ses-table 'testid ses)
             (define response (reset! ses ses-table 'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) 
                           APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response) 
                           (list (header #"X-Is-Last" #"true")))
             (check-sample-program-response response 0 "Initialize Program")
             (check-false (hash-ref ses-table 'testid false)))


  (test-case "reset! empties state and sends current state with header when it doesn't have a prev cache"
             (define zip (zipper '() (step "Initialize Program" sample-tree) '() 0))
             (define stepper identity)
             (define ses (session zip stepper 1))
             (define ses-table (make-hash))
             (hash-set! ses-table 'testid ses)
             (define response (reset! ses ses-table 'testid))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response)
                           (list (make-header #"X-Is-Last" #"true")))
             (check-sample-program-response response 0 "Initialize Program")
             (check-false (hash-ref ses-table 'testid false)))
  )

(define-test-suite BACK!
  #:before (thunk (displayln "Running tests for back!..."))
  #:after (thunk (displayln "Finished running tests for back!."))

  (test-case "back! sends initial state with header when only one thing in prev cache and updates state"
             (define zip (zipper (list (step "Initialize Program" sample-tree)) 'test '() 1))
             (define stepper identity)
             (define ses (session zip stepper 1))
             (define response (back! ses))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response)
                           (list (header #"X-Is-Last" #"true")))
             (check-sample-program-response response 0 "Initialize Program")
             (define new-zipper (session-zipper ses))
             (check-equal? (zipper-prev new-zipper) '())
             (check-equal? (zipper-curr new-zipper) (step "Initialize Program" sample-tree))
             (check-equal? (zipper-next new-zipper) '(test))
             (check-equal? (zipper-idx new-zipper) 0))

  (test-case "back! sends initial state when multiple things in prev cache and updates state"
              (define zip (zipper (list (step "Initialize Program" sample-tree) 'test1) 'test2 '() 2))
              (define stepper identity)
              (define ses (session zip stepper 1))
              (define response (back! ses))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-sample-program-response response 1 "Initialize Program")
              (define new-zipper (session-zipper ses))
              (check-equal? (zipper-prev new-zipper) '(test1))
              (check-equal? (zipper-curr new-zipper) (step "Initialize Program" sample-tree))
              (check-equal? (zipper-next new-zipper) '(test2))
              (check-equal? (zipper-idx new-zipper) 1))
  )

(define-test-suite INIT-MODEL!
  #:before (thunk (displayln "Running tests for init model binding!..."))
  #:after (thunk (displayln "Finished running tests for init model binding!."))

  (test-case "flip model emits flip delay/disjunction rules (no railroad disjunction rules)"
             (define ses (session (zipper '() #f '() 0) step/const-tree-output 1))
             (check-equal? (response-code (init! ses (make-post-init-request disj-delay-program #:model "mk-l3-flip-lazy") 'testid)) 200)
             (check-equal? (session-model-id ses) "mk-l3-flip-lazy")
             (define names (collect-step-names ses 24))
             (check-not-false (member "flip/delay-swap-left" names))
             (check-not-false (member "flip/invoke-delay" names))
             (check-false (member "rail/enter-right" names))
             (check-false (member "rail/return-left" names)))

  (test-case "rail model emits railroad delay/disjunction rules (no flip disjunction rule)"
             (define ses (session (zipper '() #f '() 0) step/const-tree-output 1))
             (check-equal? (response-code (init! ses (make-post-init-request disj-delay-program #:model "mk-l4-rail-lazy") 'testid)) 200)
             (check-equal? (session-model-id ses) "mk-l4-rail-lazy")
             (define names (collect-step-names ses 24))
             (check-not-false (member "rail/enter-right" names))
             (check-not-false (member "rail/return-left" names))
             (check-not-false (member "rail/invoke-delay" names))
             (check-false (member "flip/delay-swap-left" names)))

  (test-case "rail eager model emits eager call rules after init"
             (define ses (session (zipper '() #f '() 0) step/const-tree-output 1))
             (check-equal? (response-code (init! ses (make-post-init-request disj-delay-program #:model "mk-l4-rail-eager") 'testid)) 200)
             (check-equal? (session-model-id ses) "mk-l4-rail-eager")
             (define names (collect-step-names ses 24))
             (check-not-false (member "call/eager-expand" names))
             (check-false (member "call/lazy-expand-on-resume" names)))

  (test-case "rail lazy disjunction-delay profile does not also suspend plain relcalls"
             (define ses (session (zipper '() #f '() 0) step/const-tree-output 1))
             (check-equal?
              (response-code
               (init!
                ses
                (make-post-init-request
                 same-program
                 (hasheq 'text same-program
                         'sourceMode "mini"
                         'compileProfile (hasheq 'conjAssoc "right"
                                                 'disjAssoc "left"
                                                 'delayPlacement "disj"))
                 #:model "mk-l4-rail-lazy")
                'testid))
              200)
             (check-equal? (session-model-id ses) "mk-l4-rail-lazy")
             (define names (collect-step-names ses 16))
             (check-not-false (member "source-delay/bridge" names))
             (check-false (member "call/lazy-suspend-call" names))
             (check-not-false (member "call/lazy-expand" names))))

(define-test-suite LIST-MODELS!
  #:before (thunk (displayln "Running tests for list-models!..."))
  #:after (thunk (displayln "Finished running tests for list-models!."))

  (test-case "list-models! returns known backend models with parser contract"
             (define response (list-models!))
             (check-equal? (response-code response) 200)
             (define models (string->jsexpr (response-body->string response)))
             (check-true (list? models))
             (check-true (>= (length models) (length surfaced-model-ids)))
             (define ids (for/list ([m (in-list models)])
                           (hash-ref m 'id #f)))
             (for ([id (in-list surfaced-model-ids)])
               (check-not-false (member id ids)))
             (check-true (for/and ([m (in-list models)])
                           (and (hash-has-key? m 'parserProfile)
                                (hash-has-key? m 'parserTarget)
                                (hash-has-key? m 'capabilities)
                                (equal? (hash-ref m 'parserTarget #f)
                                        canonical-parser-target-id))))))

(define-test-suite ANALYZE!
  #:before (thunk (displayln "Running tests for analyze!..."))
  #:after (thunk (displayln "Finished running tests for analyze!."))

  (test-case "analyze! returns capability payload for valid source"
             (define req (make-post-analyze-request "(run* (q) (fresh (x) (== q x)))"))
             (define response (analyze! #f req))
             (check-equal? (response-code response) 200)
             (define body (string->jsexpr (response-body->string response)))
             (assert-analyze-payload-shape body "analyze valid source"))

  (test-case "analyze! defaults missing source options to canonical mini profile"
             (define req
               (make-post-request "analyze"
                                  (hasheq 'text "(run* (q) (fresh (x) (== q x)))")))
             (define response (analyze! #f req))
             (check-equal? (response-code response) 200)
             (define body (string->jsexpr (response-body->string response)))
             (assert-analyze-payload-shape body "analyze default source options"))

  (test-case "analyze! returns 400 on syntax error"
             (define req (make-post-analyze-request "(run* (== 'a 'a))"))
             (define response (analyze! #f req))
             (check-equal? (response-code response) 400)
             (define body (string->jsexpr (response-body->string response)))
             (check-false (hash-ref body 'validSyntax #t))
             (check-true (hash-has-key? body 'error)))

  (test-case "analyze! compatibility ids are known model ids"
             (define req
               (make-post-analyze-request
                "(run* (q) (fresh (x) (== q x)))"))
             (define response (analyze! #f req))
             (check-equal? (response-code response) 200)
             (define body (string->jsexpr (response-body->string response)))
             (assert-analyze-payload-shape body "analyze compatibility ids")
             (define models-res (string->jsexpr (response-body->string (list-models!))))
             (define known-ids
               (for/set ([m (in-list models-res)])
                 (hash-ref m 'id #f)))
             (for ([id (in-list (hash-ref body 'compatibleModelIds '()))])
               (check-true (set-member? known-ids id)))
             (for ([id (in-list (hash-ref body 'incompatibleModelIds '()))])
               (check-true (set-member? known-ids id))))

  (test-case "analyze! returns surfaced-compatible payload for appendo"
             (define req
               (make-post-analyze-request
                "(defrel (appendo l s out)
                   (conde
                     [(== l '()) (== s out)]
                     [(fresh (a d res)
                        (== l (cons a d))
                        (== out (cons a res))
                        (appendo d s res))]))
                 (run* (q) (appendo (list 'mini) (list 'kanren) q))"))
             (define response (analyze! #f req))
             (check-equal? (response-code response) 200)
             (define body (string->jsexpr (response-body->string response)))
             (assert-analyze-payload-shape body "analyze appendo")
             (check-not-false (member "mk-l3-dfs-lazy"
                                      (hash-ref body 'compatibleModelIds '())))
             (check-not-false (member "mk-l4-rail-lazy"
                                      (hash-ref body 'compatibleModelIds '())))
             (check-true (null? (hash-ref body 'incompatibleModelIds '())))
             (define reasons-by-model (hash-ref body 'incompatReasonsByModel #hash()))
             (check-equal? (hash-count reasons-by-model) 0))

  (test-case "analyze! rejects compileProfile when sourceMode is micro"
             (define req
               (make-post-analyze-request
                "(run* (q) (Zzz (== q 'cat)))"
                (hasheq 'text "(run* (q) (Zzz (== q 'cat)))"
                        'sourceMode "micro"
                        'compileProfile (hasheq 'conjAssoc "left"
                                                'disjAssoc "right"
                                                'delayPlacement "relbody"))))
             (define response (analyze! #f req))
             (check-equal? (response-code response) 400)
             (define body (string->jsexpr (response-body->string response)))
             (check-false (hash-ref body 'validSyntax #t))
             (check-true (hash-has-key? body 'error))))

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
             (define rendered (hash-ref body 'source #f))
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
  INIT-MODEL!
  LIST-MODELS!
  SOURCE-CONVERT!
  ANALYZE!
)

(run-tests APP)
