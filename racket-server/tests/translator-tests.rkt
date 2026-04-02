#lang racket

(require redex
         redex/reduction-semantics
         rackunit
         rackunit/text-ui)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/transpiler.rkt"
         "../src/extensions/l4-railroad-syntax.rkt"
         "../src/legacy-variant-adapter.rkt")

(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()
        (cons expr (read-all port)))))

(define (parse-src src)
  (parse-prog (read-all (open-input-string src))))

(define/provide-test-suite TRANSLATOR-LEGACY
  (test-case
   "run*-only translation is shape-correct and closed"
   (define-values (model-1 html-1)
     (parse-src "(run* (q) (== 'a 'a))"))
   (check-true (redex-match? L p model-1))
   (check-true (judgment-holds (closed-program? ,model-1)))
   (check-true (string? html-1)))

  (test-case
   "defrel+run* translation is shape-correct and closed"
   (define-values (model-2 html-2)
     (parse-src
      "(defrel (same x y) (== x y))
(run* (q) (same q 'cat))"))
   (check-true (redex-match? L p model-2))
   (check-true (judgment-holds (closed-program? ,model-2)))
   (check-true (string? html-2)))

  (test-case
   "legacy surface translation lifts to L4 config syntax"
   (define-values (legacy html)
     (parse-src
      "(defrel (same x y) (== x y))
(run* (q) (same q 'cat))"))
   (define lifted (legacy-program->l4-config legacy))
   (check-true (redex-match? L4 config lifted))
   (check-true (string? html))))

(module+ test
  (run-tests TRANSLATOR-LEGACY))
