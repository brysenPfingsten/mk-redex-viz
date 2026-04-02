#lang racket

(require redex/reduction-semantics
         "./calls-lang.rkt"
         "./rail-seq-lang.rkt")

(provide rail-seq-calls-lang)

(check-redundancy #t)

(define-union-language rail-seq-calls/join
  calls-lang
  rail-seq-lang)

(define-extended-language rail-seq-calls-lang
  rail-seq-calls/join
  [K ::= hole
        (K × g c)]
  [KCorePath ::= hole
                 (Freshened c tag KCorePath)
                 (KCorePath <-+ f)
                 (f +-> KCorePath)]
  [KScopePath ::= hole
                  (Freshened c tag KScopePath)
                  (KScopePath <-+ f)
                  (f +-> KScopePath)])
