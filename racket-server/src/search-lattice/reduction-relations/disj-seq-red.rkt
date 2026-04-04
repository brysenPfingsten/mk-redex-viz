#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         "./disj-base-red.rkt"
         "./private/step-utils.rkt")

(provide disj-seq-local/under-QShell
         disj-seq-red
         step-once)

(check-redundancy #t)

(define disj-seq-local/base
  (reduction-relation
   disj-lang
   #:domain cfg
   [--> (in-hole KBranch ((search_1 <-+ search_2) × g c))
        (in-hole KBranch ((search_1 × g c) <-+ (search_2 × g c)))
        "disj-seq/distribute-over-conj"]))

(define disj-seq-local/under-QShell
  (context-closure disj-seq-local/base disj-lang QShell))

;; Seq exposes shared local rules under the nested cut QShell ∘ KBranch ∘ KLocal.
(define disj-core-local/under-branch
  (context-closure disj-core-local/base disj-lang KBranch))

(define disj-goal-local/under-branch
  (context-closure disj-goal-local/base disj-lang KBranch))

(define disj-base-core
  (context-closure disj-core-local/under-branch disj-lang QShell))

(define disj-goal-local/under-QShell
  (context-closure disj-goal-local/under-branch disj-lang QShell))

(define disj-frontier/base
  (context-closure disj-frontier/local-base disj-lang QShell))

(define disj-seq-red
  (union-reduction-relations
   disj-core-shell/base
   disj-base-core
   disj-goal-local/under-QShell
   disj-frontier/base
   disj-seq-local/under-QShell))

(define (step-once prog)
  (step-once/deterministic disj-seq-red prog))
