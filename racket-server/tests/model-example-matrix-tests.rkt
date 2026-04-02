#lang racket

(require json
         rackunit
         rackunit/text-ui
         web-server/http/response-structs
         "../src/app.rkt"
         "../src/search-runtime.rkt"
         "../src/search-strategy.rkt"
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         "../src/zipper.rkt"
         "./example-compat-tests.rkt"
         "./runtime-test-support.rkt"
         "./test-http-helpers.rkt")

(provide MODEL-EXAMPLE-MATRIX)

(define MATRIX-STEP-CAP 10)

(define PRIMARY-STRATEGIES
  (list (search-strategy "early" "rail")
        (search-strategy "late" "rail")
        (search-strategy "early" "dfs")))

(define REPRESENTATIVE-LABELS
  '("appendoh 1" "fives/fours" "same"))

(define/match (strategy-label strategy)
  [((search-strategy hoist scheduler))
   (format "~a/~a" hoist scheduler)])

(define (step1-name+cfg succ)
  (match succ
    [(list name cfg) (values name cfg)]
    [_ (values "<unknown>" succ)]))

(define (domain-error? e)
  (and (exn:fail? e)
       (regexp-match? #px"not in domain" (exn-message e))))

(define (incompatible-result)
  (hasheq 'status 'incompatible
          'steps 0
          'last-rule ""))

(define (classify-terminal cfg steps last-rule)
  (hasheq 'status (if (final-config? cfg) 'value 'stuck)
          'steps steps
          'last-rule last-rule))

(define (classify-nondeterministic succs steps)
  (define rule-names
    (for/list ([next-succ (in-list succs)])
      (match next-succ
        [(list name _cfg) (format "~a" name)]
        [_ "<unknown>"])))
  (hasheq 'status 'nondeterministic
          'steps steps
          'last-rule (string-join rule-names " | ")))

(define (classify-config maybe-step-once cfg [steps 0] [last-rule ""])
  (with-handlers ([domain-error?
                   (lambda (_e)
                     (incompatible-result))])
    (match (maybe-step-once cfg)
      ['()
       (classify-terminal cfg steps last-rule)]
      [(list _succ) #:when (>= steps MATRIX-STEP-CAP)
       (hasheq 'status 'cap
               'steps steps
               'last-rule last-rule)]
      [(list succ)
       (define-values (nm cfg1) (step1-name+cfg succ))
       (classify-config maybe-step-once
                        cfg1
                        (add1 steps)
                        nm)]
      [succs
       (classify-nondeterministic succs steps)])))

(define (classify-compatible-pair strategy src)
  (with-handlers ([domain-error?
                   (lambda (_e)
                     (incompatible-result))])
    (define sexprs (read-all-sexprs (open-input-string src)))
    (define-values (cfg0 _html) (parse-prog/canonical sexprs))
    (cond
      [(and (search-config-in-domain? strategy cfg0)
            (search-config-well-formed? strategy cfg0))
       (classify-config (lookup-search-step-once strategy) cfg0)]
      [else
       (incompatible-result)])))

(define (classify-pair strategy src should-compat?)
  (cond
    [(not should-compat?) (incompatible-result)]
    [else (classify-compatible-pair strategy src)]))

(define (summarize rows)
  (for/fold ([h (hash)])
            ([r (in-list rows)])
    (match-define (hash* ['strategy strategy]
                         ['status status]
                         #:open)
      r)
    (define k (list strategy status))
    (hash-set h k (add1 (hash-ref h k 0)))))

(define (run-api-steps! ses strategy label [i 0] [last-rule ""])
  (cond
    [(>= i MATRIX-STEP-CAP)
     (hasheq 'status 'cap
             'steps i
             'last-rule last-rule)]
    [else
     (define-values (step-resp ses^) (step! ses))
     (match (response-body->string step-resp)
       ["null"
        (hasheq 'status 'done
                'steps i
                'last-rule last-rule)]
       [step-body
        (define payload (string->jsexpr step-body))
        (match-define (hash* ['stepName step-name] #:open) payload)
        (assert-step-payload-shape payload
                                   (format "~a / ~a step ~a"
                                           (strategy-label strategy)
                                           label
                                           i))
        (run-api-steps! ses^
                        strategy
                        label
                        (add1 i)
                        step-name)])]))

(define (make-default-session)
  (make-empty-session))

(define/match (make-heavy-row strategy label result)
  [(strategy label
             (hash* ['status status]
                    ['steps steps]
                    ['last-rule last-rule]
                    #:open))
  (hasheq 'strategy (strategy-label strategy)
          'label label
          'status status
          'steps steps
          'last-rule last-rule)])

(define (collect-heavy-rows specs examples runner)
  (for*/list ([spec (in-list specs)]
              [(label src) (in-dict examples)]
              #:when (member label REPRESENTATIVE-LABELS))
    (define result (runner spec label src))
    (make-heavy-row spec label result)))

(define (assert-heavy-rows rows
                           disallowed-statuses
                           #:forbid-nondeterministic? [forbid-nondeterministic? #f]
                           #:check-primary-rail? [check-primary-rail? #f]
                           #:context [context "heavy"])
  (when forbid-nondeterministic?
    (for ([r (in-list rows)])
      (match-define (hash* ['strategy strategy]
                           ['label label]
                           ['status status]
                           ['last-rule last-rule]
                           #:open)
        r)
      (check-false (eq? status 'nondeterministic)
                   (format "~a: unexpected nondeterminism for ~a / ~a (choices=~a)"
                           context
                           strategy
                           label
                           last-rule))))
  (for ([r (in-list rows)])
    (match-define (hash* ['strategy strategy]
                         ['label label]
                         ['status status]
                         ['last-rule last-rule]
                         #:open)
      r)
    (check-false (member status disallowed-statuses)
                 (format "~a: surfaced pair failed for ~a / ~a (status=~a last-rule=~a)"
                         context
                         strategy
                         label
                         status
                         last-rule)))
  (when check-primary-rail?
    (for ([strategy (in-list PRIMARY-STRATEGIES)])
      (define row
        (for/first ([r (in-list rows)]
                    #:do [(match-define (hash* ['strategy row-strategy]
                                               ['label row-label]
                                               #:open)
                             r)]
                    #:when (and (equal? row-strategy
                                        (strategy-label strategy))
                                (equal? row-label "fives/fours")))
          r))
      (check-not-false row
                       (format "missing matrix row for ~a / fives/fours"
                               (strategy-label strategy)))
      (match-define (hash* ['status status]
                           ['last-rule last-rule]
                           ['steps steps]
                           #:open)
        row)
      (check-false (eq? status 'stuck)
                   (format "stuck regression for ~a / fives/fours (last-rule=~a, steps=~a)"
                           (strategy-label strategy)
                           last-rule
                           steps)))))

(define (run-heavy-row/api strategy label src)
  (define ses (make-default-session))
  (with-handlers ([exn:fail?
                   (lambda (e)
                     (hasheq 'status 'init-error
                             'steps 0
                             'last-rule (exn-message e)))])
    (define-values (init-resp ses^)
      (init! ses (make-post-init-request src #:strategy strategy) 'matrix-id))
    (check-equal? (response-code init-resp) 200
                  (format "init failed for ~a / ~a" (strategy-label strategy) label))
    (check-equal? (session-search-strategy ses^) strategy
                  (format "session strategy binding drifted for ~a / ~a"
                          (strategy-label strategy)
                          label))
    (assert-step-payload-shape (string->jsexpr (response-body->string init-resp))
                               (format "~a / ~a init"
                                       (strategy-label strategy)
                                       label))
    (run-api-steps! ses^ strategy label)))

(define/provide-test-suite MODEL-EXAMPLE-MATRIX
  (test-case "matrix lane: structured strategies cover the frontend example corpus"
    (define examples (frontend-example-programs))
    (define heavy-rows
      (collect-heavy-rows all-surfaced-search-strategies
                          examples
                          (lambda (strategy _label src)
                            (classify-pair strategy src #t))))
    (assert-heavy-rows heavy-rows '(incompatible)
                       #:forbid-nondeterministic? #t
                       #:check-primary-rail? #t
                       #:context "direct matrix")
    (displayln (format "[matrix-tests] heavy summary: ~s" (summarize heavy-rows))))

  (test-case "api-flow lane: structured strategies cover the frontend example corpus"
    (define examples (frontend-example-programs))
    (define heavy-rows
      (collect-heavy-rows all-surfaced-search-strategies examples run-heavy-row/api))
    (assert-heavy-rows heavy-rows '(incompatible init-error)
                       #:context "api matrix")
    (displayln (format "[matrix-tests] heavy api-flow summary: ~s" (summarize heavy-rows)))))

(module+ test
  (run-tests MODEL-EXAMPLE-MATRIX))
