#lang racket

(require redex/reduction-semantics
         (only-in "./core-red.rkt"
                  core-local/base
                  core-shell/base)
         (only-in "./delay-red.rkt"
                  delay-local/delta
                  delay-frontier/delta)
         (only-in "./disj-base-red.rkt"
                  disj-goal-local/base
                  disj-frontier/base))

(provide search-base-local/base
         search-base-shell/base)

(check-redundancy #t)

(require "../languages/search-base-lang.rkt")

;; search-base joins the common core directly, then adds only the true delay
;; and disjunction deltas from each branch.
(define search-base-local/base
  (union-reduction-relations
   (context-closure
    (extend-reduction-relation core-local/base search-base-lang)
    search-base-lang
    KLocal)
   (extend-reduction-relation disj-goal-local/base search-base-lang)
   (context-closure
    (extend-reduction-relation delay-local/delta search-base-lang)
    search-base-lang
    KLocal)))

(define search-base-shell/base
  (union-reduction-relations
   (extend-reduction-relation core-shell/base search-base-lang)
   (extend-reduction-relation disj-frontier/base search-base-lang)
   (extend-reduction-relation delay-frontier/delta search-base-lang)))
