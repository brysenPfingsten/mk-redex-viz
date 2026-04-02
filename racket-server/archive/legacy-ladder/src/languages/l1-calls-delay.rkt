#lang racket

(require redex/reduction-semantics
         "./l0.rkt")

(check-redundancy #t)

(provide L1)

;; L1 delta:
;; - goal form: relation call, delayed goal
;; - search-tree forms: delay, proceed
;; - inherited context: Kconj
(define-extended-language L1 L0
  [g ....
     (r t ... tag)
     (suspend g tag)]
  [pr ((r t ... tag) σ)
      (g σ)]
  [s ....
     (delay s)
     (proceed pr)])
