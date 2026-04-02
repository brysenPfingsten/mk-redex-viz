#lang racket

(require racket/runtime-path
         rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "../src/search-lattice/languages/canonical-lang.rkt"
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt")

(provide EXAMPLE-COMPAT
         frontend-example-programs)

;; Source of truth lives in frontend; tests consume it directly.
(define-runtime-path FRONTEND-EXAMPLES-PATH
  "../../frontend/src/utils/example_programs.js")

(define TEMPLATE-DEF-RX
  #px"const\\s+([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*`((?:\\\\`|[^`])*)`\\s*;?")

(define ARRAY-ENTRY-RX
  #px"\\{\\s*id:\\s*\"([^\"]+)\"\\s*,\\s*label:\\s*\"([^\"]+)\"\\s*,\\s*miniSource:\\s*([A-Za-z_][A-Za-z0-9_]*)")

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
    (list (second m) (third m) (fourth m))))

(define (frontend-example-programs)
  (define js-src (file->string FRONTEND-EXAMPLES-PATH))
  (define templates (extract-template-map js-src))
  (for/list ([entry (in-list (extract-example-refs js-src))])
    (match-define (list _id label value-var) entry)
    (define maybe-src (hash-ref templates value-var #f))
    (unless maybe-src
      (error 'frontend-example-programs
             (format "example value ~a (label ~a) has no matching template definition"
                     value-var
                     label)))
    (cons label maybe-src)))

(define (parse-src/canonical src)
  (parse-prog/canonical (read-all-sexprs (open-input-string src))))

(define (render-src/micro src)
  (render-micro-source (read-all-sexprs (open-input-string src))))

(define (assert-example-compat! name src)
  (define-values (canonical html) (parse-src/canonical src))
  (check-true (string? html) (format "~a should produce html-guid source" name))
  (check-true (redex-match? canonical-lang config canonical)
              (format "~a should lift into canonical config syntax" name))
  (check-true (redex-match? canonical-lang config canonical)
              (format "~a should satisfy canonical target predicate (~a)"
                      name
                      canonical-parser-target-id)))

(define/provide-test-suite EXAMPLE-COMPAT
  (test-case "frontend examples parse and lift to canonical target"
    (define examples (frontend-example-programs))
    (check-true (pair? examples)
                "frontend/src/utils/example_programs.js did not yield runnable examples")
    (for ([pr (in-list examples)])
      (match-define (cons label src) pr)
      (assert-example-compat! label src)))

  (test-case "frontend examples render to direct micro source and lift through micro parser"
    (for ([pr (in-list (frontend-example-programs))])
      (match-define (cons label src) pr)
      (define micro-src (render-src/micro src))
      (define-values (canonical html)
        (parse-prog/canonical (read-all-sexprs (open-input-string micro-src))
                              #:source-mode "micro"))
      (check-true (string? html)
                  (format "~a rendered micro should produce html-guid source" label))
      (check-true (redex-match? canonical-lang config canonical)
                  (format "~a rendered micro should lift into canonical config syntax" label)))))

(module+ test
  (run-tests EXAMPLE-COMPAT))
