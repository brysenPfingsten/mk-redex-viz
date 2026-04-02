#lang racket

(require redex/reduction-semantics
         "./common.rkt"
         "./rdisj-l3-common.rkt"
         "./core-l3.rkt")

(check-redundancy #t)

(provide Rbase-l)

(define call-lazy-extra/l3
  (reduction-relation
    L3/K
    #:domain config
    ;; Stage 1 (inside active branch): call contexts from L1.
    ;; Stage 2 (outside): left-disjunction scheduler contexts.
    [--> (Γ (in-hole Kleft (in-hole Kcore ((r t ... tag) σ))) as)
         (Γ (in-hole Kleft (in-hole Kcore (delay (proceed ((r t ... tag) σ)))) ) as)
         "call/lazy-suspend-call"]

    [--> (Γ (in-hole Kdelay (delay (proceed ((r t ... tag) σ)))) as)
         (Γ (in-hole Kdelay (proceed ((r t ... tag) σ))) as)
         "call/lazy-invoke-delay"]

    [--> (Γ (in-hole Kleft (in-hole Kcore (proceed ((r t ... tag) σ)))) as)
         (Γ (in-hole Kleft (in-hole Kcore (g_new σ))) as)
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/lazy-expand-on-resume"]

    [--> (Γ (in-hole Kleft (in-hole Kcore ((delay s_1) × g c))) as)
         (Γ (in-hole Kleft (in-hole Kcore (delay (s_1 × g c)))) as)
         "call/delay-through-conj"]))

(define call+core-l3/lazy
  (union-reduction-relations
   call-lazy-extra/l3
   core-cfg/l3))

(define disj-extra/l3
  (make-disj-extra/l3 call+core-l3/lazy))

(define Rbase-l
  (union-reduction-relations
   call-lazy-extra/l3
   disj-extra/l3
   core-cfg/l3))
