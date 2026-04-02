#lang racket

(require redex/reduction-semantics
         "./l3-base.rkt")

(check-redundancy #t)

(provide L4)

;; L4 delta:
;; - search-tree form: right railroad branch
;; - context: extend Kdisj through right-rail positions
(define-extended-language L4 L3
  [s .... (s +-> s)]
  [Kdisj .... (s +-> Kdisj)])
