#lang racket

(require redex/reduction-semantics
         "./common.rkt"
         "./core-l3.rkt")

(check-redundancy #t)

(provide Rbase-e)

(define call-eager-extra/l3
  (reduction-relation
    L3/K
    #:domain config
    [--> (Γ ans* (in-hole K3 ((r t ... tag) σ)))
         (Γ ans* (in-hole K3 (delay (proceed (g_new σ)))))
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/eager-suspend-expanded"]

    [--> (Γ ans* (in-hole K3 (delay (proceed (g σ)))))
         (Γ ans* (in-hole K3 (proceed (g σ))))
         "call/eager-invoke-delay"]

    [--> (Γ ans* (in-hole K3 (proceed (g σ))))
         (Γ ans* (in-hole K3 (g σ)))
         "call/eager-resume-goal"]))

(define disj-extra/l3
  (reduction-relation
    L3/K
    #:domain config
    [--> (Γ ans* (in-hole K3 ((g_1 ∨ g_2 tag) σ)))
         (Γ ans* (in-hole K3 ((g_1 σ) <-+ (g_2 σ))))
         "disj/goal-to-tree"]

    [--> (Γ ans* (in-hole K3 ((s_1 <-+ s_2) × g c)))
         (Γ ans* (in-hole K3 ((s_1 × g c) <-+ (s_2 × g c))))
         "disj/distribute-over-conj"]

    [--> (Γ (σ ...) ((⊤ σ_new) <-+ s_right))
         (Γ (σ ... σ_new) s_right)
         "disj/collect-left-answer"]

    [--> (Γ ans* ((empty-tree) <-+ s_right))
         (Γ ans* s_right)
         "disj/skip-left-fail"]))

(define Rbase-e
  (union-reduction-relations
   call-eager-extra/l3
   disj-extra/l3
   core-base-extra-l3))
