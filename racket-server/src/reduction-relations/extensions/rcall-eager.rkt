#lang racket

(require redex/reduction-semantics
         "./common.rkt"
         "./core-l1.rkt")

(check-redundancy #t)

(provide Rcall-eager)

(define Rcall-eager
  (extend-reduction-relation
    core-base-l1
    L1/K
    [--> (Γ ans* (in-hole K1 ((r t ... tag) σ)))
         (Γ ans* (in-hole K1 (delay (proceed (g_new σ)))))
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/eager-suspend-expanded"]

    [--> (Γ ans* (in-hole K1 (delay (proceed (g σ)))))
         (Γ ans* (in-hole K1 (proceed (g σ))))
         "call/eager-invoke-delay"]

    [--> (Γ ans* (in-hole K1 (proceed (g σ))))
         (Γ ans* (in-hole K1 (g σ)))
         "call/eager-resume-goal"]))
