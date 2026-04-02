#lang racket

(require redex/reduction-semantics
         "./common.rkt"
         "./context-l1.rkt"
         "./core-l1.rkt")

(check-redundancy #t)

(provide call-lazy-extra/l1
         Rcall-lazy)

(define call-lazy-extra/l1
  (reduction-relation
    L1/K
    #:domain config
    [--> (Γ (in-hole K ((name goal-src g) σ)) as)
         (Γ (in-hole K s_new) as)
         (where s_new ,(bridge-source-delay/lazy-host (term Γ)
                                                      (term goal-src)
                                                      (term σ)))
         (side-condition (not (equal? (term s_new) #f)))
         "source-delay/bridge"]

    [--> (Γ (in-hole K ((r t ... tag) σ)) as)
         (Γ (in-hole K (g_new σ)) as)
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/lazy-expand"]

    [--> (Γ (delay (proceed ((r t ... tag) σ))) as)
         (Γ (proceed ((r t ... tag) σ)) as)
         "call/lazy-invoke-delay"]

    [--> (Γ (delay s_1) as)
         (Γ s_1 as)
         (side-condition (not (redex-match? L1/K (proceed pr) (term s_1))))
         "call/invoke-delay"]

    [--> (Γ (in-hole K (proceed ((r t ... tag) σ))) as)
         (Γ (in-hole K (g_new σ)) as)
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/lazy-expand-on-resume"]

    [--> (Γ (in-hole K ((delay s_1) × g c)) as)
         (Γ (in-hole K (delay (s_1 × g c))) as)
         "call/delay-through-conj"]))

(define Rcall-lazy
  (union-reduction-relations
   call-lazy-extra/l1
   core-cfg/l1))
