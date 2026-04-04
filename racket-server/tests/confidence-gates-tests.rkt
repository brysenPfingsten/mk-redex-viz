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

(provide CONFIDENCE-GATES)

(define TRACE-STEP-CAP 30)
(define PAYLOAD-STEP-CAP 25)

(define (example-src label)
  (for/first ([pr (in-list (frontend-example-programs))]
              #:do [(match-define (cons example-label src) pr)]
              #:when (equal? example-label label))
    src))

(define (example-cfg label [compile-profile #f])
  (define src (example-src label))
  (unless src
    (error 'trace-steps (format "missing example label: ~a" label)))
  (define-values (cfg _html)
    (parse-prog/canonical (read-all-sexprs (open-input-string src))
                          #:compile-profile compile-profile))
  cfg)

(define/match (strategy-label strategy)
  [((search-strategy hoist scheduler))
   (format "~a/~a" hoist scheduler)])

(define (trace-steps strategy
                     label
                     [compile-profile #f]
                     [cfg (example-cfg label compile-profile)]
                     [step-once (lookup-search-step-once strategy)]
                     [i 0]
                     [acc '()])
  (define next* (step-once cfg))
  (match next*
    ['()
     (values (reverse acc) (if (final-config? cfg) 'value 'stuck) cfg)]
    [(list _ ...) #:when (>= i TRACE-STEP-CAP)
     (values (reverse acc) 'cap cfg)]
    [(list (list nm cfg1) _ ...)
     (trace-steps strategy
                  label
                  compile-profile
                  cfg1
                  step-once
                  (add1 i)
                  (cons nm acc))]))

(define (named-step? nm)
  (and (string? nm)
       (> (string-length (string-trim nm)) 0)))

(define (length+last steps)
  (for/fold ([count 0]
             [last-step "<none>"])
            ([nm (in-list steps)])
    (values (add1 count) nm)))

(define (count-non-null-step-payloads strategy label ses [i 0] [seen 0])
  (cond
    [(>= i PAYLOAD-STEP-CAP) seen]
    [else
     (define-values (step-resp next-session) (step! ses))
     (define body (response-body->string step-resp))
     (cond
       [(string=? body "null")
        (count-non-null-step-payloads strategy
                                      label
                                      next-session
                                      (add1 i)
                                      seen)]
       [else
        (assert-step-payload-shape (string->jsexpr body)
                                   (format "~a / ~a step ~a"
                                           (strategy-label strategy)
                                           label
                                           i))
        (count-non-null-step-payloads strategy
                                      label
                                      next-session
                                      (add1 i)
                                      (add1 seen))])]))

(define REPRESENTATIVE-TRACES
  (list
   (list (search-strategy "early" "rail")
         "fives/fours"
         #f
         "enter-right")
   (list (search-strategy "late" "flip")
         "fives/fours"
         #f
         "delay-swap-left")
   (list (search-strategy "late" "dfs")
         "same"
         (hasheq 'conjAssoc "left"
                 'disjAssoc "right"
                 'delayPlacement "relcall")
         "expand-relcall")))

(define/provide-test-suite CONFIDENCE-GATES
  (test-case "representative structured strategies stay live and produce named search-lattice rules"
    (for ([entry (in-list REPRESENTATIVE-TRACES)])
      (match-define (list strategy label compile-profile required-step) entry)
      (define-values (steps status final-cfg) (trace-steps strategy label compile-profile))
      (define-values (step-count last-step) (length+last steps))
      (check-true (or (eq? status 'value) (eq? status 'cap))
                  (format "~a / ~a unexpectedly ~a (steps=~a last=~a cfg=~s)"
                          (strategy-label strategy)
                          label
                          status
                          step-count
                          last-step
                          final-cfg))
      (for ([nm (in-list steps)]
            [idx (in-naturals 1)])
        (check-true (named-step? nm)
                    (format "~a / ~a has unnamed step at position ~a: ~v"
                            (strategy-label strategy) label idx nm)))
      (check-not-false (member required-step steps)
                       (format "~a / ~a missing representative step ~a"
                               (strategy-label strategy)
                               label
                               required-step))))

  (test-case "init/step payloads satisfy UI contract for structured search strategies"
    (define pairs
      (list (list (search-strategy "early" "rail") "appendoh 1")
            (list (search-strategy "late" "flip") "fives/fours")
            (list (search-strategy "late" "dfs") "same")))
    (for ([pr (in-list pairs)])
      (match-define (list strategy label) pr)
      (define src (example-src label))
      (define ses (make-empty-session))
      (define-values (init-resp ses^) (init! ses (make-post-init-request src #:strategy strategy) 'shape-id))
      (check-equal? (response-code init-resp) 200
                    (format "init failed for ~a / ~a" (strategy-label strategy) label))
      (check-equal? (session-search-strategy ses^) strategy
                    (format "session strategy binding drifted for ~a / ~a"
                            (strategy-label strategy)
                            label))
      (assert-step-payload-shape (string->jsexpr (response-body->string init-resp))
                                 (format "~a / ~a init" (strategy-label strategy) label))
      (define seen (count-non-null-step-payloads strategy label ses^))
      (check-true (> seen 0)
                  (format "~a / ~a produced no non-null steps"
                          (strategy-label strategy)
                          label)))))

(module+ test
  (run-tests CONFIDENCE-GATES))
