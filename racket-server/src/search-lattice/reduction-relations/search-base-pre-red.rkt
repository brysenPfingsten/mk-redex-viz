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

(provide search-local/base
         search-shell/base)

(check-redundancy #t)

(require "../languages/search-base-lang.rkt")

;; search joins the common core directly, then adds only the true delay
;; and disjunction deltas from each branch.
(define search-local/base
  (union-reduction-relations
   (context-closure
    (extend-reduction-relation core-local/base search-lang)
    search-lang
    LocalCtx)
   (extend-reduction-relation disj-goal-local/base search-lang)
   (context-closure
    (extend-reduction-relation delay-local/delta search-lang)
    search-lang
    LocalCtx)))

(define search-shell/base
  (union-reduction-relations
   (extend-reduction-relation core-shell/base search-lang)
   (extend-reduction-relation disj-frontier/base search-lang)
   (extend-reduction-relation delay-frontier/delta search-lang)))
