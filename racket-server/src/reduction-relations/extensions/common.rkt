#lang racket

(require racket/match
         redex/reduction-semantics
         "../../core-definitions.rkt")

(provide instantiate-call-host)

;; Relation call helper shared by eager/lazy variants.
(define (instantiate-call-host gamma r ts)
  (define maybe-rel
    (for/first ([clause (in-list gamma)]
                #:when (equal? (first clause) r))
      clause))
  (unless maybe-rel
    (error 'instantiate-call-host "unknown relation ~a in Γ" r))
  (match-define (list _r d g) maybe-rel)
  (unless (= (length d) (length ts))
    (error 'instantiate-call-host
           "arity mismatch for ~a: expected ~a, got ~a"
           r
           (length d)
           (length ts)))
  (term (subst-goal ,g ,(map list d ts))))
