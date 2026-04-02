#lang racket

(require redex/reduction-semantics
         "./common.rkt"
         "./core-l1.rkt")

(check-redundancy #t)

(provide call-lazy-extra/l1
         Rcall-lazy)

(define call-lazy-extra/l1
  (reduction-relation
    L1/K
    #:domain config
    [--> (Γ ans* (in-hole Kcall ((r t ... tag) σ)))
         (Γ ans* (in-hole Kcall (delay (proceed ((r t ... tag) σ)))))
         "call/lazy-suspend-call"]

    [--> (Γ ans* (in-hole Kcall (delay (proceed ((r t ... tag) σ)))))
         (Γ ans* (in-hole Kcall (proceed ((r t ... tag) σ))))
         "call/lazy-invoke-delay"]

    [--> (Γ ans* (in-hole Kcall (proceed ((r t ... tag) σ))))
         (Γ ans* (in-hole Kcall (g_new σ)))
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/lazy-expand-on-resume"]))

(define base-l1/k
  (extend-reduction-relation
    core-base-l1
    L1/K))

(define Rcall-lazy
  (union-reduction-relations
   call-lazy-extra/l1
   base-l1/k))
