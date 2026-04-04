#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-pre-red.rkt")

(provide search-base-seq-pre-red)

(check-redundancy #t)

;; L3 seq keeps the same nesting discipline as L2 seq: QShell ∘ KBranch ∘ KLocal.
;; Later layers may widen KBranch when the seq story itself widens.
(define search-base-core-local/under-branch
  (context-closure search-base-core-local/base search-base-lang KBranch))

(define search-base-goal-local/under-branch
  (context-closure search-base-goal-local/base search-base-lang KBranch))

(define search-base-delay-local/under-branch
  (context-closure search-base-delay-local/base search-base-lang KBranch))

(define search-base-seq-base-core
  (context-closure search-base-core-local/under-branch search-base-lang QShell))

(define search-base-goal-local/under-QShell
  (context-closure search-base-goal-local/under-branch search-base-lang QShell))

(define search-base-delay-local/under-QShell
  (context-closure search-base-delay-local/under-branch search-base-lang QShell))

(define search-base-frontier/under-QShell
  (context-closure search-base-frontier/local-base search-base-lang QShell))

(define search-base-seq-pre-red
  (union-reduction-relations
   search-base-core-shell/base
   search-base-seq-base-core
   search-base-delay-local/under-QShell
   search-base-delay-frontier/base
   search-base-goal-local/under-QShell
   search-base-frontier/under-QShell))
