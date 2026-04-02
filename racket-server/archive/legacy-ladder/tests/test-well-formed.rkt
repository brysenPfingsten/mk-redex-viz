#lang racket

(require rackunit
         redex/reduction-semantics
         "../src/transpiler.rkt"
         "../src/languages/l0.rkt"
         "../src/languages/all.rkt"
         "../src/wf/all.rkt")

(define reverso-source
  '((defrel (appendo l s out)
      (conde
        [(== l '()) (== out s)]
        [(fresh (a d res)
           (== l `(,a . ,d))
           (== out `(,a . ,res))
           (appendo d s res))]))
    (defrel (reverseo ls out)
      (conde
        [(== ls '()) (== out '())]
        [(fresh (a d res)
           (== ls `(,a . ,d))
           (reverseo d res)
           (appendo res `(,a) out))]))
    (run* (q) (reverseo '(dog cat bear lion) q))))

(define-values (reverso-cfg _html)
  (parse-prog/canonical reverso-source))

(test-case "Canonical reverso config is L4 well formed"
  (check-true (redex-match? L4 config reverso-cfg))
  (check-true (judgment-holds (wf-config/L4? ,reverso-cfg))))

(test-case "L0 config is accepted by core wf judgment"
  (define core-cfg
    (term (() ((succeed (label "ok"))
               (state () () () () (label "s")))
              (empty-stream))))
  (check-true (redex-match? L0 config core-cfg))
  (check-true (judgment-holds (wf-config? ,core-cfg)))
  (check-true (judgment-holds (core-shape? ,core-cfg))))

(test-case "Relation call arity mismatch is rejected by L4 wf"
  (define bad-arity
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           ((r:id (sym "ok") (sym "extra") (label "call"))
            (state () () () () (label "s")))
           (empty-stream))))
  (check-true (redex-match? L4 config bad-arity))
  (check-false (judgment-holds (wf-config/L4? ,bad-arity))))
