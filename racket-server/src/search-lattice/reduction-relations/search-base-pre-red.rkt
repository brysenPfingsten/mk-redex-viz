#lang racket

(require redex/reduction-semantics
         (only-in "./delay-red.rkt"
                  delay-local/base
                  delay-frontier/base)
         (only-in "./disj-base-red.rkt"
                  disj-core-shell/base
                  disj-core-local/base
                  disj-goal-local/base
                  disj-frontier/local-base))

(provide search-base-core-local/base
         search-base-core-shell/base
         search-base-goal-local/base
         search-base-frontier/local-base
         search-base-delay-local/base
         search-base-delay-frontier/base)

(check-redundancy #t)

(require "../languages/search-base-lang.rkt")

(define search-base-core-local/base
  (extend-reduction-relation disj-core-local/base search-base-lang))

(define search-base-core-shell/base
  (extend-reduction-relation disj-core-shell/base search-base-lang))

(define search-base-goal-local/base
  (extend-reduction-relation disj-goal-local/base search-base-lang))

(define search-base-frontier/local-base
  (extend-reduction-relation disj-frontier/local-base search-base-lang))

(define search-base-delay-local/base
  (extend-reduction-relation delay-local/base search-base-lang))

(define search-base-delay-frontier/base
  (extend-reduction-relation delay-frontier/base search-base-lang))
