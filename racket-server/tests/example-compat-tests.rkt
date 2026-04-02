#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         racket/runtime-path
         racket/match
         "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/transpiler.rkt"
         "../src/legacy-variant-adapter.rkt"
         "../src/extensions/l4-railroad-syntax.rkt")

(provide EXAMPLE-COMPAT)

;; Source of truth lives in frontend; tests consume it directly.
(define-runtime-path FRONTEND-EXAMPLES-PATH
  "../../frontend/src/utils/example_programs.js")

(define TEMPLATE-DEF-RX
  #px"const\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*`((?:\\\\`|[^`])*)`\\s*;?")

(define ARRAY-ENTRY-RX
  #px"\\{\\s*value:\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*label:\\s*\"([^\"]+)\"")

(define (decode-template-literal s)
  ;; Frontend examples currently use escaped backticks inside template literals.
  (regexp-replace* #px"\\\\`" s "`"))

(define (extract-template-map js-src)
  (for/hash ([m (in-list (regexp-match* TEMPLATE-DEF-RX
                                        js-src
                                        #:match-select values))])
    (define var-name (second m))
    (define template-body (third m))
    (values var-name (decode-template-literal template-body))))

(define (extract-example-refs js-src)
  (for/list ([m (in-list (regexp-match* ARRAY-ENTRY-RX
                                        js-src
                                        #:match-select values))])
    (list (second m) (third m))))

(define (frontend-example-programs)
  (define js-src (file->string FRONTEND-EXAMPLES-PATH))
  (define templates (extract-template-map js-src))
  (for/list ([entry (in-list (extract-example-refs js-src))])
    (match-define (list value-var label) entry)
    (define maybe-src (hash-ref templates value-var #f))
    (unless maybe-src
      (error 'frontend-example-programs
             (format "example value ~a (label ~a) has no matching template definition"
                     value-var
                     label)))
    (cons label maybe-src)))

(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()
        (cons expr (read-all port)))))

(define (parse-src src)
  (parse-prog (read-all (open-input-string src))))

(define (assert-example-compat! name src)
  (define-values (legacy html) (parse-src src))
  (check-true (string? html) (format "~a should produce html-guid source" name))
  (check-true (redex-match? L p legacy) (format "~a should parse as legacy L program" name))
  (check-true (judgment-holds (closed-program? ,legacy))
              (format "~a should be closed in legacy judgments" name))
  (define lifted (legacy-program->canonical-config legacy))
  (check-true (redex-match? L4 config lifted)
              (format "~a should lift into L4 config syntax" name))
  (check-true (canonical-config? lifted)
              (format "~a should satisfy canonical target predicate (~a)"
                      name
                      canonical-target-id)))

(define/provide-test-suite EXAMPLE-COMPAT
  (test-case "frontend examples parse and lift to canonical target"
    (define examples (frontend-example-programs))
    (check-true (pair? examples)
                "frontend/src/utils/example_programs.js did not yield runnable examples")
    (for ([pr (in-list examples)])
      (match-define (cons label src) pr)
      (assert-example-compat! label src))))

(module+ test
  (run-tests EXAMPLE-COMPAT))
