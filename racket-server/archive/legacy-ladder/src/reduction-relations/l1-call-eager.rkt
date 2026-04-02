#lang racket

(require redex/reduction-semantics
         "./private/support/common.rkt"
         "../languages/l1-calls-delay.rkt"
         "./private/core/core-l1.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl1-call-eager
         step-once)

(define call-eager-extra/l1
  (reduction-relation
    L1
    #:domain config
    [--> (Γ (in-hole Kconj ((suspend g tag) σ)) as)
         (Γ (in-hole Kconj (delay (g σ))) as)
         "l1/suspend-goal"]

    [--> (Γ (in-hole Kconj ((r t ... tag) σ)) as)
         (Γ (in-hole Kconj (g_new σ)) as)
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "l1/eager-expand"]

    [--> (Γ (delay s_1) as)
         (Γ s_1 as)
         "l1/invoke-delay"]

    [--> (Γ (in-hole Kconj (proceed (g σ))) as)
         (Γ (in-hole Kconj (g σ)) as)
         "l1/eager-resume-goal"]

    [--> (Γ (in-hole Kconj ((delay s_1) × g c)) as)
         (Γ (in-hole Kconj (delay (s_1 × g c))) as)
         "l1/delay-through-conj"]))

(define Rl1-call-eager
  (union-reduction-relations
   call-eager-extra/l1
   core-cfg/l1))

(define (step-once prog)
  (step-once/deterministic Rl1-call-eager prog))
