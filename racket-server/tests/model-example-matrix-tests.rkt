#lang racket

(require rackunit
         rackunit/text-ui
         racket/match
         racket/string
         json
         web-server/http/response-structs
         "../src/app.rkt"
         "../src/capability-analysis.rkt"
         "../src/model-registry.rkt"
         "../src/model-surface-policy.rkt"
         "../src/transpiler.rkt"
         "../src/zipper.rkt"
         "./test-http-helpers.rkt"
         "./variant-test-support.rkt"
         "./example-compat-tests.rkt")

(provide MODEL-EXAMPLE-MATRIX)

(define MATRIX-STEP-CAP 25)

(define PRIMARY-RAIL-MODELS
  '("mk-l4-rail-lazy" "mk-l4-rail-eager" "mk-l3-dfs-lazy"))

(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()
        (cons expr (read-all port)))))

(define (step1-name+cfg succ)
  (match succ
    [(list name cfg) (values name cfg)]
    [_ (values "<unknown>" succ)]))

(define (domain-error? e)
  (and (exn:fail? e)
       (regexp-match? #px"not in domain" (exn-message e))))

(define (classify-pair model-id src should-compat?)
  (define maybe-step-once (lookup-model-step-once model-id))
  (unless maybe-step-once
    (error 'classify-pair (format "unknown model: ~a" model-id)))

  (if (not should-compat?)
      (hasheq 'status 'incompatible
              'steps 0
              'last-rule "")
      (let ()
        (define sexprs (read-all (open-input-string src)))
        (define-values (cfg0 _html) (parse-prog/canonical sexprs))
        (with-handlers ([domain-error?
                         (lambda (_e)
                           (hasheq 'status 'incompatible
                                   'steps 0
                                   'last-rule ""))])
          (let loop ([cfg cfg0] [steps 0] [last-rule ""])
            (define next* (maybe-step-once cfg))
            (cond
              [(null? next*)
               (hasheq 'status (if (final-config? cfg) 'value 'stuck)
                       'steps steps
                       'last-rule last-rule)]
              [(> (length next*) 1)
               (define rule-names
                 (for/list ([succ (in-list next*)])
                   (match succ
                     [(list name _cfg) (format "~a" name)]
                     [_ "<unknown>"])))
               (hasheq 'status 'nondeterministic
                       'steps steps
                       'last-rule (string-join rule-names " | "))]
              [(>= steps MATRIX-STEP-CAP)
               (hasheq 'status 'cap
                       'steps steps
                       'last-rule last-rule)]
              [else
               (define-values (nm cfg1) (step1-name+cfg (first next*)))
               (loop cfg1 (add1 steps) nm)]))))))

(define (summarize rows)
  (for/fold ([h (hash)])
            ([r (in-list rows)])
    (define k (list (hash-ref r 'model) (hash-ref r 'status)))
    (hash-set h k (add1 (hash-ref h k 0)))))

(define (run-api-steps! ses model-id label)
  (let loop ([i 0] [last-rule ""])
    (if (>= i MATRIX-STEP-CAP)
        (hasheq 'status 'cap
                'steps i
                'last-rule last-rule)
        (let* ([step-resp (step! ses)]
               [step-body (response-body->string step-resp)])
          (if (string=? step-body "null")
              (hasheq 'status 'done
                      'steps i
                      'last-rule last-rule)
              (let ([payload (string->jsexpr step-body)])
                (assert-step-payload-shape payload
                                           (format "~a / ~a step ~a"
                                                   model-id label i))
                (loop (add1 i) (hash-ref payload 'stepName ""))))))))

(define (source-compatible-with-model? src model-id)
  (define requirements (hash-ref (analyze-source-capabilities src) 'requirements '()))
  (member model-id (compatible-model-ids requirements all-model-specs)))

(define (first-compatible-example model-id examples)
  (for/first ([(label src) (in-dict examples)]
              #:when (source-compatible-with-model? src model-id))
    (cons label src)))

(define (make-default-session)
  (define default-step-once (lookup-model-step-once default-model-id))
  (unless default-step-once
    (error 'MODEL-EXAMPLE-MATRIX
           (format "default model missing stepper: ~a" default-model-id)))
  (session (zipper '() #f '() 0)
           (make-stepper default-step-once)
           1))

(define (make-heavy-row model-id label should-compat? result)
  (hasheq 'model model-id
          'label label
          'should-compat? should-compat?
          'status (hash-ref result 'status)
          'steps (hash-ref result 'steps)
          'last-rule (hash-ref result 'last-rule)))

(define (make-smoke-row model-id label result)
  (hasheq 'model model-id
          'label label
          'status (hash-ref result 'status)
          'steps (hash-ref result 'steps)
          'last-rule (hash-ref result 'last-rule)))

(define (collect-heavy-rows specs examples runner)
  (for*/list ([spec (in-list specs)]
              [(label src) (in-dict examples)])
    (define model-id (model-spec-id spec))
    (define should-compat? (and (source-compatible-with-model? src model-id) #t))
    (define result (runner model-id label src should-compat?))
    (make-heavy-row model-id label should-compat? result)))

(define (collect-smoke-rows specs examples runner smoke-kind)
  (for/list ([spec (in-list specs)])
    (define model-id (model-spec-id spec))
    (define maybe-example (first-compatible-example model-id examples))
    (unless maybe-example
      (fail-check (format "no compatible ~a example found for ~a" smoke-kind model-id)))
    (if maybe-example
        (let ()
          (define label (car maybe-example))
          (define src (cdr maybe-example))
          (define result (runner model-id label src #t))
          (make-smoke-row model-id label result))
        (make-smoke-row model-id "<missing>"
                        (hasheq 'status 'missing-example 'steps 0 'last-rule "")))))

(define (assert-heavy-rows rows
                           compat-disallowed
                           incompatible-expected
                           #:forbid-nondeterministic? [forbid-nondeterministic? #f]
                           #:check-primary-rail? [check-primary-rail? #f]
                           #:context [context "heavy"])
  (when forbid-nondeterministic?
    (for ([r (in-list rows)])
      (check-false (eq? (hash-ref r 'status) 'nondeterministic)
                   (format "~a: unexpected nondeterminism for ~a / ~a (choices=~a)"
                           context
                           (hash-ref r 'model)
                           (hash-ref r 'label)
                           (hash-ref r 'last-rule)))))
  (for ([r (in-list rows)])
    (when (hash-ref r 'should-compat? #f)
      (check-false (member (hash-ref r 'status) compat-disallowed)
                   (format "~a: compatible pair failed for ~a / ~a (status=~a last-rule=~a)"
                           context
                           (hash-ref r 'model)
                           (hash-ref r 'label)
                           (hash-ref r 'status)
                           (hash-ref r 'last-rule)))))
  (for ([r (in-list rows)])
    (when (not (hash-ref r 'should-compat? #t))
      (check-equal? (hash-ref r 'status) incompatible-expected
                    (format "~a: expected incompatible pair for ~a / ~a, got ~a"
                            context
                            (hash-ref r 'model)
                            (hash-ref r 'label)
                            (hash-ref r 'status)))))
  (when check-primary-rail?
    (for ([mid (in-list PRIMARY-RAIL-MODELS)])
      (define row
        (for/first ([r (in-list rows)]
                    #:when (and (equal? (hash-ref r 'model) mid)
                                (equal? (hash-ref r 'label) "fives/fours")))
          r))
      (check-not-false row (format "missing matrix row for ~a / fives/fours" mid))
      (check-false (eq? (hash-ref row 'status) 'stuck)
                   (format "stuck regression for ~a / fives/fours (last-rule=~a, steps=~a)"
                           mid
                           (hash-ref row 'last-rule)
                           (hash-ref row 'steps))))))

(define (assert-smoke-rows rows disallowed-statuses context)
  (for ([r (in-list rows)])
    (check-false (member (hash-ref r 'status) disallowed-statuses)
                 (format "~a: regression for ~a / ~a (status=~a last-rule=~a steps=~a)"
                         context
                         (hash-ref r 'model)
                         (hash-ref r 'label)
                         (hash-ref r 'status)
                         (hash-ref r 'last-rule)
                         (hash-ref r 'steps)))))

(define (run-heavy-row/api model-id label src should-compat?)
  (define analyze-resp (analyze! #f (make-post-analyze-request src)))
  (check-equal? (response-code analyze-resp) 200
                (format "analyze failed for ~a" label))
  (define analyze-body (string->jsexpr (response-body->string analyze-resp)))
  (check-true (hash-ref analyze-body 'validSyntax #f)
              (format "analyze returned invalid syntax for ~a" label))
  (define compatible-ids (hash-ref analyze-body 'compatibleModelIds '()))
  (check-equal? (and (member model-id compatible-ids) #t) (and should-compat? #t)
                (format "analyze/model compatibility mismatch for ~a / ~a" model-id label))
  (define ses (make-default-session))
  (define model-resp (switch-model! ses (make-post-model-request model-id) 'matrix-id))
  (check-equal? (response-code model-resp) 200
                (format "switch-model failed for ~a" model-id))
  (if should-compat?
      (with-handlers ([exn:fail?
                       (lambda (e)
                         (hasheq 'status 'init-error
                                 'steps 0
                                 'last-rule (exn-message e)))])
        (define init-resp (init! ses (make-post-init-request src) 'matrix-id))
        (check-equal? (response-code init-resp) 200
                      (format "init failed for compatible pair ~a / ~a"
                              model-id
                              label))
        (assert-step-payload-shape (string->jsexpr (response-body->string init-resp))
                                   (format "~a / ~a init" model-id label))
        (run-api-steps! ses model-id label))
      (let ([failed?
             (with-handlers ([exn:fail? (lambda (_e) #t)])
               (init! ses (make-post-init-request src) 'matrix-id)
               #f)])
        (when (not failed?)
          (fail-check
           (format "expected incompatible init rejection for ~a / ~a"
                   model-id
                   label)))
        (hasheq 'status 'incompatible
                'steps 0
                'last-rule ""))))

(define (run-smoke-row/api model-id label src _should-compat?)
  (define analyze-resp (analyze! #f (make-post-analyze-request src)))
  (check-equal? (response-code analyze-resp) 200
                (format "smoke analyze failed for ~a / ~a" model-id label))
  (define ses (make-default-session))
  (define model-resp (switch-model! ses (make-post-model-request model-id) 'matrix-id))
  (check-equal? (response-code model-resp) 200
                (format "smoke switch-model failed for ~a" model-id))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (hasheq 'status 'init-error
                             'steps 0
                             'last-rule (exn-message e)))])
    (define init-resp (init! ses (make-post-init-request src) 'matrix-id))
    (check-equal? (response-code init-resp) 200
                  (format "smoke init failed for ~a / ~a" model-id label))
    (assert-step-payload-shape (string->jsexpr (response-body->string init-resp))
                               (format "smoke ~a / ~a init" model-id label))
    (run-api-steps! ses model-id label)))

(define/provide-test-suite MODEL-EXAMPLE-MATRIX
  (test-case "matrix lane: heavy L3/L4 full coverage; internal L0/L1/L2 smoke only"
    (define examples (frontend-example-programs))
    (define heavy-rows
      (collect-heavy-rows surfaced-model-specs
                          examples
                          (lambda (model-id _label src should-compat?)
                            (classify-pair model-id src should-compat?))))
    (assert-heavy-rows heavy-rows '(incompatible) 'incompatible
                       #:forbid-nondeterministic? #t
                       #:check-primary-rail? #t
                       #:context "direct matrix")
    (define smoke-rows
      (collect-smoke-rows internal-smoke-model-specs
                          examples
                          (lambda (model-id _label src _should-compat?)
                            (classify-pair model-id src #t))
                          "smoke"))
    (assert-smoke-rows smoke-rows '(stuck incompatible nondeterministic missing-example)
                       "internal smoke")

    (displayln (format "[matrix-tests] heavy summary: ~s" (summarize heavy-rows)))
    (displayln (format "[matrix-tests] internal smoke summary: ~s" (summarize smoke-rows))))

  (test-case "api-flow lane: heavy L3/L4 full matrix; internal L0/L1/L2 bounded smoke"
    (define examples (frontend-example-programs))
    (define heavy-rows (collect-heavy-rows surfaced-model-specs examples run-heavy-row/api))
    (assert-heavy-rows heavy-rows '(incompatible init-error) 'incompatible
                       #:context "api matrix")
    (define smoke-rows
      (collect-smoke-rows internal-smoke-model-specs examples run-smoke-row/api "API-smoke"))
    (assert-smoke-rows smoke-rows '(stuck incompatible init-error missing-example)
                       "internal API smoke")

    (displayln (format "[matrix-tests] heavy api-flow summary: ~s" (summarize heavy-rows)))
    (displayln (format "[matrix-tests] internal smoke api-flow summary: ~s" (summarize smoke-rows)))))

(module+ test
  (run-tests MODEL-EXAMPLE-MATRIX))
