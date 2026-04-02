#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         "./disj-base-red.rkt"
         "./private/step-utils.rkt")

(provide disj-fused-local/under-QShell
         disj-fused-red
         step-once)

(check-redundancy #t)

(define disj-fused-local/base
  (reduction-relation
   disj-lang
   #:domain cfg
   [--> (in-hole KLate (((in-hole QFresh (⊤ σ_new)) <-+ search_rest) × g c))
        (in-hole KLate
                 ((in-hole QFresh (g σ_new)) <-+ (search_rest × g c)))
        "disj-fused/continue-left-answer"]
   [--> (in-hole KLate (((in-hole QFresh (empty-tree)) <-+ search_rest) × g c))
        (in-hole KLate (search_rest × g c))
        "disj-fused/continue-left-fail"]))

(define disj-fused-local/under-QShell
  (context-closure disj-fused-local/base disj-lang QShell))

(define lifted-disj-core-local/base
  (extend-reduction-relation disj-core-local/base disj-lang))

(define lifted-disj-core-shell/base
  (extend-reduction-relation disj-core-shell/base disj-lang))

(define lifted-disj-goal-local/base
  (extend-reduction-relation disj-goal-local/base disj-lang))

(define lifted-disj-frontier/local-base
  (extend-reduction-relation disj-frontier/local-base disj-lang))

;; Fused exposes shared local rules under the nested cut QShell ∘ KLate.
(define disj-core-local/under-late
  (context-closure lifted-disj-core-local/base disj-lang KLate))

(define disj-goal-local/under-late
  (context-closure lifted-disj-goal-local/base disj-lang KLate))

(define disj-base-core
  (context-closure disj-core-local/under-late disj-lang QShell))

(define disj-goal-local/under-QShell
  (context-closure disj-goal-local/under-late disj-lang QShell))

(define disj-frontier/base
  (context-closure lifted-disj-frontier/local-base disj-lang QShell))

(define disj-fused-red
  (union-reduction-relations
   lifted-disj-core-shell/base
   disj-base-core
   disj-goal-local/under-QShell
   disj-frontier/base
   disj-fused-local/under-QShell
   ))

(define (step-once prog)
  (step-once/deterministic disj-fused-red prog))
