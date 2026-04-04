#lang racket

(require json
         "../src/app.rkt"
         "../src/search-strategy.rkt"
         "./example-compat-tests.rkt"
         "./test-http-helpers.rkt")

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

(define (response->payload response)
  (string->jsexpr (response-body->string response)))

(define (payload->program payload)
  (string->jsexpr (hash-ref payload 'program)))

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

(define (json-root-name node)
  (match-define (hash* ['name name] #:open) node)
  name)

(define (example-src label)
  (for/first ([pr (in-list (frontend-example-programs))]
              #:do [(match-define (cons example-label src) pr)]
              #:when (equal? example-label label))
    src))

(define (payload-summary payload)
  (define program (payload->program payload))
  (hasheq 'step (hash-ref payload 'step)
          'stepName (hash-ref payload 'stepName)
          'root (json-root-name program)
          'hasFreshened (json-contains-name? program "Freshened")
          'hasBounced (json-contains-name? program "Deferred")))

(define (collect-step-summaries ses remaining)
  (cond
    [(zero? remaining) '()]
    [else
     (define-values (response ses^) (step! ses))
     (match (response-body->string response)
       ["null" '()]
       [out
        (define payload (string->jsexpr out))
        (cons (payload-summary payload)
              (collect-step-summaries ses^ (sub1 remaining)))])]))

(define (init-session-for src [strategy default-search-strategy] [payload #f] [session-id 'ui-smoke-id])
  (define req
    (if payload
        (make-post-init-request src payload #:strategy strategy)
        (make-post-init-request src #:strategy strategy)))
  (init! (make-empty-session) req session-id))

(define (reset-restores-init-summary src)
  (define-values (init-response ses1)
    (init-session-for src))
  (define init-payload (response->payload init-response))
  (define init-program (hash-ref init-payload 'program))
  (define-values (_step-response-1 ses2) (step! ses1))
  (define-values (_step-response-2 ses3) (step! ses2))
  (define-values (reset-response _ses4) (reset! ses3))
  (define reset-payload (response->payload reset-response))
  (hasheq 'initStepName (hash-ref init-payload 'stepName)
          'resetStepName (hash-ref reset-payload 'stepName)
          'programRestored (equal? init-program (hash-ref reset-payload 'program))))

(define (first-visible-steps-summary src [strategy default-search-strategy] [count 8])
  (define-values (init-response ses1)
    (init-session-for src strategy))
  (define init-payload (response->payload init-response))
  (hasheq 'init (payload-summary init-payload)
          'steps (collect-step-summaries ses1 count)))

(define (disj-delay-summary)
  (define payload
    (hasheq 'text disj-relcall-program
            'sourceMode "mini"
            'compileProfile (hasheq 'conjAssoc "right"
                                    'disjAssoc "left"
                                    'delayPlacement "disj")))
  (define-values (_init-response ses1)
    (init-session-for disj-relcall-program
                      (search-strategy "early" "rail")
                      payload
                      'ui-smoke-disj))
  (define steps (collect-step-summaries ses1 24))
  (hasheq 'sawSuspendGoal
          (for/or ([entry (in-list steps)])
            (equal? (hash-ref entry 'stepName) "suspend-goal"))
          'sawExpand
          (for/or ([entry (in-list steps)])
            (equal? (hash-ref entry 'stepName) "expand-relcall"))
          'sampleSteps steps))

(define report
  (hasheq
   'resetRestoresInit
   (reset-restores-init-summary (example-src "fives/fours"))
   'fivesFoursDefault
   (first-visible-steps-summary (example-src "fives/fours"))
   'fivesFoursEarlyRail
   (first-visible-steps-summary (example-src "fives/fours")
                                (search-strategy "early" "rail"))
   'disjDelayPlacement
   (disj-delay-summary)))

(displayln (jsexpr->string report))
