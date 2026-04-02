#lang racket

(require redex/reduction-semantics
         "./common.rkt"
         "./core-l1.rkt")

(check-redundancy #t)

(provide Rcall-lazy)

(define Rcall-lazy
  (extend-reduction-relation
    core-base-l1
    L1/K
    [--> (Γ ans* (in-hole K1 ((r t ... tag) σ)))
         (Γ ans* (in-hole K1 (delay (proceed ((r t ... tag) σ)))))
         "call/lazy-suspend-call"]

    [--> (Γ ans* (in-hole K1 (delay (proceed ((r t ... tag) σ)))))
         (Γ ans* (in-hole K1 (proceed ((r t ... tag) σ))))
         "call/lazy-invoke-delay"]

    [--> (Γ ans* (in-hole K1 (proceed ((r t ... tag) σ))))
         (Γ ans* (in-hole K1 (g_new σ)))
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/lazy-expand-on-resume"]

    [--> (Γ ans* (in-hole K1 (proceed (g σ))))
         (Γ ans* (in-hole K1 (g σ)))
         "call/lazy-resume-goal"]))
