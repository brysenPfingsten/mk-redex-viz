#lang racket

(require rackunit rackunit/text-ui json web-server/http
         "../src/app.rkt" "../src/program-runner.rkt"
         (only-in "../src/minikanren.rkt" run-source)
         "../src/transpiler.rkt" "../src/search-runtime.rkt"
         (only-in "../derivations/shared/kernel-equations.rkt" current-atomic-observer)
         (only-in "../derivations/matrix/stages/full.rkt" SRel)
         (only-in "../derivations/shared/stages/schema.rkt" decompose D)
         "test-http-helpers.rkt")

(define (payload response) (string->jsexpr (response-body->string response)))
(define (response-header response name)
  (for/first ([entry (in-list (response-headers response))]
              #:when (bytes=? name (header-field entry)))
    (header-value entry)))

(define (initialize source [mode "mini"] [profile #f])
  (init! #f
         (make-post-init-request source
          (if profile
              (hasheq 'text source 'sourceMode mode 'compileProfile profile)
              (hasheq 'text source 'sourceMode mode)))
         'api-test))

(define (steps session count)
  (if (zero? count) session
      (let-values ([(response next) (step! session)])
        (assert-step-payload-shape (payload response) 'step)
        (steps next (sub1 count)))))

(define (until session wanted [fuel 150])
  (cond [(eq? (model-session-status session) wanted) session]
        [(zero? fuel) (error 'until "did not reach ~e" wanted)]
        [else
         (define-values (_response next) (step! session))
         (until next wanted (sub1 fuel))]))

(define (check-actual-work session [fuel 100])
  (unless (model-session-done? session)
    (when (zero? fuel) (error 'check-actual-work "finite work witness exhausted its budget"))
    (define before (decompose SRel (model-session-current-config session)))
    (define observed '())
    (define-values (_response next)
      (parameterize ([current-atomic-observer
                      (lambda (goal state) (set! observed (cons (list goal state) observed)))])
        ;; These operations must not attempt reductions or replay a solver.
        (model-session-status session)
        (model-session-current-picture session)
        (model-session-current-answer-nodes session)
        (step! session)))
    (define expected
      (match (model-session-current-step-name next)
        ["eval-atom"
         (match before [(D (list 'eval _ goal state) _) (list (list goal state))])]
        [_ '()]))
    (check-equal? (reverse observed) expected)
    (check-actual-work next (sub1 fuel))))

(define/provide-test-suite APP
  (test-case "init preserves explicit query metadata and returns the compiled matrix picture"
    (define-values (response session)
      (initialize "(defrel (same x y) (== x y)) (run 2 (q r) (same q r))"))
    (check-equal? (response-code response) 200)
    (check-equal? (response-mime response) #"application/json; charset=utf-8")
    (check-equal? (response-header response #"Set-Cookie")
                  #"session-id=api-test; Path=/; SameSite=Lax")
    (check-equal? (response-header response #"X-Is-Start") #"true")
    (check-equal? (query-info-names (model-session-query session)) '(q r))
    (check-equal? (query-info-variables (model-session-query session)) '(u:0 u:1))
    (check-equal? (query-info-limit (model-session-query session)) 2)
    (check-match (model-session-current-config session) (list 'program _ (list 'commit _)))
    (check-equal? (hash-ref (payload response) 'executionStatus) "running")
    (check-equal? (hash-ref (payload response) 'answerCount) 0)
    (check-true (string? (hash-ref (payload response) 'htmlGuids)))
    (check-equal? (string->jsexpr (hash-ref (payload response) 'program))
                  (model-session-current-picture session)))

  (test-case "paused Frontier is observable and next explicitly invokes public advance"
    (define-values (_initial session)
      (initialize "(run* (q) (Zzz (== q 'ready)))" "micro"))
    (define paused (until session 'paused))
    (check-false (model-session-done? paused))
    (check-equal? (model-session-current-host-answers paused) '())
    (define-values (response advancing) (step! paused))
    (check-equal? (hash-ref (payload response) 'stepName) "advance")
    (check-equal? (hash-ref (payload response) 'stepKind) "public-operation")
    (check-false (response-header response #"X-Done"))
    (match-define (list 'program definitions frontier) (model-session-current-config paused))
    (check-equal? (model-session-current-config advancing)
                  (list 'program definitions (list 'advance frontier)))
    (define completed (until advancing 'complete))
    (check-equal? (model-session-current-host-answers completed) '(ready))
    (define-values (done-response same) (step! completed))
    (check-equal? same completed)
    (check-equal? (response-header done-response #"X-Done") #"true"))

  (test-case "back, forward and reset preserve exact configurations, profiles and query metadata"
    (define profile (hasheq 'conjAssoc "right" 'disjAssoc "left" 'delayPlacement "disj"))
    (define-values (initial-response initial)
      (initialize "(run* (q) (conde [(== q 'a)] [(== q 'b)]))" "mini" profile))
    (define-values (first-response first) (step! initial))
    (define-values (back-response back) (back! first))
    (check-equal? (model-session-current-config back) (model-session-current-config initial))
    (check-equal? (hash-ref (payload back-response) 'program)
                  (hash-ref (payload initial-response) 'program))
    (define-values (again-response again) (step! back))
    (check-equal? (payload again-response) (payload first-response))
    (check-equal? (model-session-current-config again) (model-session-current-config first))
    (define-values (reset-response reset) (reset! (steps again 8)))
    (check-equal? (model-session-current-config reset) (model-session-current-config initial))
    (check-equal? (model-session-query reset) (model-session-query initial))
    (check-equal? (hash-ref (payload reset-response) 'step) 0)
    (check-equal? (response-header reset-response #"X-Is-Start") #"true"))

  (test-case "configuration inspection and operation counts follow reductions, advancement and history"
    (define (check-view response session reductions advances kind)
      (define view (payload response))
      (assert-step-payload-shape view 'configuration-inspection)
      ;; Read the server's text as a term: it must preserve the whole native
      ;; configuration, not reconstruct a summary from the rendered tree.
      (define input (open-input-string (hash-ref view 'configuration)))
      (check-equal? (read input) (model-session-current-config session))
      (check-true (eof-object? (read input)))
      (check-equal? (hash-ref view 'stepKind) kind)
      (check-equal? (hash-ref view 'reductionCount) reductions)
      (check-equal? (hash-ref view 'advanceCount) advances)
      (check-equal? (hash-ref view 'step) (+ reductions advances)))
    (define-values (initial-response initial)
      (initialize "(run* (q) (Zzz (fresh (unused) (== q 'ready))))" "micro"))
    (check-view initial-response initial 0 0 "initialization")
    (define-values (first-response first) (step! initial))
    (check-view first-response first 1 0 "reduction")
    (define paused (until first 'paused))
    (define reductions (model-session-step-index paused))
    (define-values (advance-response advancing) (step! paused))
    (check-view advance-response advancing reductions 1 "public-operation")
    (check-equal? (hash-ref (payload advance-response) 'stepName) "advance")
    (define-values (reduction-response resumed) (step! advancing))
    (check-view reduction-response resumed (add1 reductions) 1 "reduction")
    (define-values (back-response back) (back! resumed))
    (check-view back-response back reductions 1 "public-operation")
    (check-equal? (payload back-response) (payload advance-response))
    (define-values (paused-response back-to-pause) (back! back))
    (check-view paused-response back-to-pause reductions 0 "reduction")
    (check-equal? (hash-ref (payload paused-response) 'executionStatus) "paused")
    (define-values (replay-response replay) (step! back-to-pause))
    (check-view replay-response replay reductions 1 "public-operation")
    (check-equal? (payload replay-response) (payload advance-response))
    (define completed (until replay 'complete))
    (define-values (done-response done) (step! completed))
    (check-view done-response done (sub1 (model-session-step-index done)) 1 "reduction")
    (define-values (reset-response reset) (reset! done))
    (check-view reset-response reset 0 0 "initialization")
    (check-equal? (hash-ref (payload reset-response) 'configuration)
                  (hash-ref (payload initial-response) 'configuration)))

  (test-case "Done and Solo are completed terminal structures"
    (for ([source '("(run* (q) fail)" "(run* (q) succeed)")]
          [expected '(() (_.0))])
      (define-values (_response initial) (initialize source "micro"))
      (define final (until initial 'complete))
      (check-equal? (model-session-current-host-answers final) expected)
      (check-true (model-session-done? final))))

  (test-case "undefined calls, wrong arity and free lexical variables reject at initialization"
    (for ([source '("(run* (q) (missing q))"
                    "(defrel (same x y) (== x y)) (run* (q) (same q))"
                    "(defrel (bad x) (== x y)) (run* (q) (bad q))")])
      (check-exn exn:fail? (lambda () (initialize source "micro")))))

  (test-case "unguarded right recursion stays responsive and cannot commit its left candidate"
    (define source
      "(defrel (loopo x) (loopo x))
       (run* (q) (disj (== q 'candidate) (loopo q)))")
    (define-values (_response initial) (initialize source "micro"))
    (define current (steps initial 30))
    (check-equal? (model-session-status current) 'running)
    (check-equal? (model-session-current-step-name current) "eval-call")
    (check-equal? (model-session-current-host-answers current) '())
    (check-exn #rx"step cap" (lambda () (run-source source #:source-mode "micro"
                                                        #:answer-limit 1 #:step-cap 30))))

  (test-case "stuck is distinct from a paused or completed configuration"
    (define malformed (list 'program '() (list 'force '(Empty (Owners)))))
    (check-equal? (configuration-status malformed) 'stuck)
    (define-values (_response initial) (initialize "(run* (q) succeed)" "micro"))
    (define query (model-session-query initial))
    (define stuck (open-compiled malformed query))
    (check-exn #rx"stuck matrix configuration" (lambda () (step! stuck))))

  (test-case "status, rendering and answer extraction perform no extra kernel work"
    (define-values (_response initial)
      (initialize "(run* (q) (conj (disj (== q 'a) (Zzz (== q 'b))) (=/= q 'a)))"
                  "micro"))
    (check-actual-work initial))

  (test-case "source conversion preserves selectable compilation and rejects other targets"
    (define response
      (source-convert! (make-post-source-convert-request
                         "(defrel (same x y) (== x y)) (run* (q) (same q 'cat))")))
    (check-not-false (regexp-match? #rx"Zzz" (hash-ref (payload response) 'source)))
    (check-exn exn:fail?
      (lambda () (source-convert! (make-post-source-convert-request
                                   "(run* (q) (== q 'cat))"
                                   (hasheq 'text "(run* (q) (== q 'cat))"
                                           'targetSourceMode "mini")))))))

(module+ test (run-tests APP))
