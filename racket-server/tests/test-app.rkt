#lang racket
(require rackunit
         rackunit/text-ui
         web-server/http/response-structs
         web-server/http/request-structs
         net/url-structs
         json
         "../src/app.rkt"
         (prefix-in mmk: "../src/reduction-relations/reduction-relations.rkt")
         "../src/zipper.rkt")

(define sample-tree
  '(((∃
      (x:q)
      ((sym "tree1") =? (sym "horse") "u5")
      "f0")
     (state () 0 () "s"))
    ()))

(define step/const-tree-output
  (make-stepper (lambda (_) (list (list "foo" sample-tree)))))

(define (get-response-out response)
  (let ([out (open-output-string)])
    ((response-output response) out)
    (get-output-string out)))

(define-test-suite STEP!
  #:before (thunk (displayln "Running tests for step!..."))
  #:after  (thunk (displayln "Finished running tests for step!"))

  (test-case "step! sends null reponse with header if no more reductions and does not affect state"
              (define state (zipper '() (step "foo" '(() ())) '() 1))
              (define response (step! state (make-stepper (λ (_) '()))))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response)
                            (list (make-header #"X-Done" #"true")))
              (check-equal? (get-response-out response) "null")
              (check-equal? (zipper-prev state) '())
              (check-equal? (zipper-curr state) (step "foo" '(prog () ())))
              (check-equal? (zipper-next state) '())
              (check-equal? (get-index   state) 1))

  (test-case "step! steps the current program if there is no future cache and updates state"
              (define state (zipper '() (step "foo" sample-tree) '() 1))
              (define response (step! state (make-stepper mmk:step-once)))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-equal? (get-response-out response)
                            "{\"program\":\"{\\\"id\\\":\\\"u5\\\",\\\"left\\\":{\\\"sym\\\":\\\"tree1\\\"},\\\"name\\\":\\\"Unify\\\",\\\"reified\\\":[],\\\"right\\\":{\\\"sym\\\":\\\"horse\\\"},\\\"stateId\\\":\\\"s\\\",\\\"sub\\\":[],\\\"trail\\\":[]}\",\"step\":2,\"stepName\":\"Substitute Fresh Variables\"}")
              (check-equal? (zipper-prev state) (list (step "foo" sample-tree)))
              (check-equal? (step-name (zipper-curr state)) "Substitute Fresh Variables")
              (check-equal? (zipper-next state) '())
              (check-equal? (get-index   state) 2))

  (test-case "step! gets next tree in the state if it is cached and updates state"
              (define state (zipper '() (step "foo" sample-tree) (list (step "bar" sample-tree)) 1))
              (define response (step! state step/const-tree-output))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-equal? (get-response-out response)
                            "{\"program\":\"{\\\"children\\\":[{\\\"id\\\":\\\"u5\\\",\\\"left\\\":{\\\"sym\\\":\\\"tree1\\\"},\\\"name\\\":\\\"Unify\\\",\\\"right\\\":{\\\"sym\\\":\\\"horse\\\"}}],\\\"id\\\":\\\"f0\\\",\\\"name\\\":\\\"Fresh\\\",\\\"reified\\\":[],\\\"stateId\\\":\\\"s\\\",\\\"sub\\\":[],\\\"trail\\\":[],\\\"vars\\\":[{\\\"var\\\":\\\"q\\\"}]}\",\"step\":2,\"stepName\":\"bar\"}")
              (check-equal? (zipper-prev state) (list (step "foo" sample-tree)))
              (check-equal? (zipper-curr state) (step "bar" sample-tree))
              (check-equal? (zipper-next state) '())
              (check-equal? (get-index   state) 2)
              )
)

(define-test-suite INIT!
  #:before (thunk (displayln "Running tests for init!..."))
  #:after (thunk (displayln "Finished running tests for init!."))

  (test-case "init! parses, updates state, and sends response with json and string prog"
              (define sample-req
              (make-request
              #"POST"
              (make-url #f #f #f #f #t
                        (list (make-path/param "post" empty)
                              (make-path/param "init" empty))
                        empty
                        #f)
              (list (make-header #"content-type" #"application/json"))
              (delay '())
              (string->bytes/utf-8 "{\"text\":\"(run* (q) (== 'a 'a))\"}")
              "127.0.0.1"
              5000
              "127.0.0.1"))
              (define state (zipper '() #f '() 0))
              (define response (init! state sample-req))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (define json-response (string->jsexpr (get-response-out response)))
     (check-equal? (hash-ref json-response 'stepName #f) "Initialize Program")
              (check-equal? (hash-ref json-response 'step #f) 0)
              (check-not-false (hash-ref json-response 'program #f))
              (check-not-false (hash-ref json-response 'htmlGuids #f)))

  (test-case "init! throws error if program is not syntactically correct"
              (define sample-req
              (make-request
              #"POST"
              (make-url #f #f #f #f #t
                        (list (make-path/param "post" empty)
                              (make-path/param "init" empty))
                        empty
                        #f)
              (list (make-header #"content-type" #"application/json"))
              (delay '())
              (string->bytes/utf-8 "{\"text\":\"(run* (== 'a 'a))\"}")
              "127.0.0.1"
              5000
              "127.0.0.1"))
              (define state (zipper '() #f '() 0))
              (check-exn exn:fail? (thunk (init! state sample-req))))
  )

(define-test-suite RESET!
  #:before (thunk (displayln "Running tests for reset!..."))
  #:after (thunk (displayln "Finished running tests for reset."))

  (test-case "reset! empties state and sends initial state when it has a prev cache"
             (define state (zipper (list 'a 'b 'c (step "Initialize Program" sample-tree))
                                   'd '() 5))
             (define response (reset! state))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response) '())
             (check-equal? (get-response-out response)
                           "{\"program\":\"{\\\"children\\\":[{\\\"id\\\":\\\"u5\\\",\\\"left\\\":{\\\"sym\\\":\\\"tree1\\\"},\\\"name\\\":\\\"Unify\\\",\\\"right\\\":{\\\"sym\\\":\\\"horse\\\"}}],\\\"id\\\":\\\"f0\\\",\\\"name\\\":\\\"Fresh\\\",\\\"reified\\\":[],\\\"stateId\\\":\\\"s\\\",\\\"sub\\\":[],\\\"trail\\\":[],\\\"vars\\\":[{\\\"var\\\":\\\"q\\\"}]}\",\"step\":0,\"stepName\":\"Initialize Program\"}")
             (check-equal? (zipper-prev state) '())
             (check-equal? (zipper-curr state) (step "Initialize Program" sample-tree))
             (check-equal? (zipper-next state) '())
             (check-equal? (get-index   state) 0))


  (test-case "reset! empties state and sends current state with header when it doesn't have a prev cache"
             (define state (zipper '() (step "Initialize Program" sample-tree) '() 0))
             (define response (reset! state))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response) '())
             (check-equal? (get-response-out response)
                           "{\"program\":\"{\\\"children\\\":[{\\\"id\\\":\\\"u5\\\",\\\"left\\\":{\\\"sym\\\":\\\"tree1\\\"},\\\"name\\\":\\\"Unify\\\",\\\"right\\\":{\\\"sym\\\":\\\"horse\\\"}}],\\\"id\\\":\\\"f0\\\",\\\"name\\\":\\\"Fresh\\\",\\\"reified\\\":[],\\\"stateId\\\":\\\"s\\\",\\\"sub\\\":[],\\\"trail\\\":[],\\\"vars\\\":[{\\\"var\\\":\\\"q\\\"}]}\",\"step\":0,\"stepName\":\"Initialize Program\"}")
             (check-equal? (zipper-prev state) '())
             (check-equal? (zipper-curr state) (step "Initialize Program" sample-tree))
             (check-equal? (zipper-next state) '())
             (check-equal? (get-index   state) 0))
  )

(define-test-suite BACK!
  #:before (thunk (displayln "Running tests for back!..."))
  #:after (thunk (displayln "Finished running tests for back!."))

  (test-case "back! sends initial state with header when only one thing in prev cache and updates state"
             (define state (zipper (list (step "Initialize Program" sample-tree)) 'test '() 1))
             (define response (back! state))
             (check-equal? (response-code response) 200)
             (check-equal? (response-message response) #"OK")
             (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
             (check-equal? (response-headers response)
                           (list (make-header #"X-Is-Last" #"true")))
             (check-equal? (get-response-out response)
                           "{\"program\":\"{\\\"children\\\":[{\\\"id\\\":\\\"u5\\\",\\\"left\\\":{\\\"sym\\\":\\\"tree1\\\"},\\\"name\\\":\\\"Unify\\\",\\\"right\\\":{\\\"sym\\\":\\\"horse\\\"}}],\\\"id\\\":\\\"f0\\\",\\\"name\\\":\\\"Fresh\\\",\\\"reified\\\":[],\\\"stateId\\\":\\\"s\\\",\\\"sub\\\":[],\\\"trail\\\":[],\\\"vars\\\":[{\\\"var\\\":\\\"q\\\"}]}\",\"step\":0,\"stepName\":\"Initialize Program\"}")
             (check-equal? (zipper-prev state) '())
             (check-equal? (zipper-curr state) (step "Initialize Program" sample-tree))
             (check-equal? (zipper-next state) '(test))
             (check-equal? (get-index   state) 0))

  (test-case "back! sends initial state when multiple things in prev cache and updates state"
              (define state (zipper (list (step "Initialize Program" sample-tree) 'test1) 'test2 '() 2))
              (define response (back! state))
              (check-equal? (response-code response) 200)
              (check-equal? (response-message response) #"OK")
              (check-equal? (response-mime response) APPLICATION/JSON-MIME-TYPE)
              (check-equal? (response-headers response) '())
              (check-equal? (get-response-out response)
                            "{\"program\":\"{\\\"children\\\":[{\\\"id\\\":\\\"u5\\\",\\\"left\\\":{\\\"sym\\\":\\\"tree1\\\"},\\\"name\\\":\\\"Unify\\\",\\\"right\\\":{\\\"sym\\\":\\\"horse\\\"}}],\\\"id\\\":\\\"f0\\\",\\\"name\\\":\\\"Fresh\\\",\\\"reified\\\":[],\\\"stateId\\\":\\\"s\\\",\\\"sub\\\":[],\\\"trail\\\":[],\\\"vars\\\":[{\\\"var\\\":\\\"q\\\"}]}\",\"step\":1,\"stepName\":\"Initialize Program\"}")
              (check-equal? (zipper-prev state) '(test1))
              (check-equal? (zipper-curr state) (step "Initialize Program" sample-tree))
              (check-equal? (zipper-next state) '(test2))
              (check-equal? (get-index   state) 1))
  )

(define/provide-test-suite APP
  #:before (thunk (displayln "Running tests for app.rkt..."))
  #:after (thunk (displayln "Finished running tests for app.rkt"))
  STEP!
  INIT!
  RESET!
  BACK!
  ;; TODO: switch-model!
)

#; (run-tests APP)
