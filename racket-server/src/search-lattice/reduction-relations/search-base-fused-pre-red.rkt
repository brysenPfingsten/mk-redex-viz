#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-pre-red.rkt")

(provide search-base-fused-pre-red)

(check-redundancy #t)

;; L3 fused keeps the late-hoist cut explicit: QShell ∘ KLate.
(define search-base-core-local/under-late
  (context-closure search-base-core-local/base search-base-lang KLate))

(define search-base-goal-local/under-late
  (context-closure search-base-goal-local/base search-base-lang KLate))

(define search-base-delay-local/under-late
  (context-closure search-base-delay-local/base search-base-lang KLate))

(define search-base-fused-base-core
  (context-closure search-base-core-local/under-late search-base-lang QShell))

(define search-base-goal-local/under-QShell
  (context-closure search-base-goal-local/under-late search-base-lang QShell))

(define search-base-delay-local/under-QShell
  (context-closure search-base-delay-local/under-late search-base-lang QShell))

(define search-base-frontier/under-QShell
  (context-closure search-base-frontier/local-base search-base-lang QShell))

(define search-base-fused-pre-red
  (union-reduction-relations
   search-base-core-shell/base
   search-base-fused-base-core
   search-base-delay-local/under-QShell
   search-base-delay-frontier/base
   search-base-goal-local/under-QShell
   search-base-frontier/under-QShell))
