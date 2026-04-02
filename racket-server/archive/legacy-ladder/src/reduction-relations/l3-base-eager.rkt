#lang racket

(require redex/reduction-semantics
         "./private/support/common.rkt"
         "../languages/l3-base.rkt"
         "./private/features/rdisj-l3-common.rkt"
         "./private/core/core-l3.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl3-base-eager
         step-once)

(define call-eager-extra/l3
  (reduction-relation
    L3
    #:domain config
    [--> (Γ (in-hole Kdisj (in-hole Kconj ((suspend g tag) σ))) as)
         (Γ (in-hole Kdisj (in-hole Kconj (delay (g σ)))) as)
         "l3-base/suspend-goal"]

    [--> (Γ (in-hole Kdisj (in-hole Kconj ((r t ... tag) σ))) as)
         (Γ (in-hole Kdisj (in-hole Kconj (g_new σ))) as)
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "l3-base/eager-expand"]

    [--> (Γ (delay s_1) as)
         (Γ s_1 as)
         "l3-base/invoke-delay"]

    [--> (Γ (in-hole Kdisj (in-hole Kconj (proceed (g σ)))) as)
         (Γ (in-hole Kdisj (in-hole Kconj (g σ))) as)
         "l3-base/eager-resume-goal"]

    [--> (Γ (in-hole Kdisj (in-hole Kconj ((delay s_1) × g c))) as)
         (Γ (in-hole Kdisj (in-hole Kconj (delay (s_1 × g c)))) as)
         "l3-base/delay-through-conj"]))

(define Rl3-base-eager
  (union-reduction-relations
   call-eager-extra/l3
   disj-extra/l3
   core-cfg/l3))

(define (step-once prog)
  (step-once/deterministic Rl3-base-eager prog))
