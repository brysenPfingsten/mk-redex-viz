#lang racket

(require redex/reduction-semantics
         "./common.rkt"
         "./rdisj-left.rkt"
         "./core-l3.rkt")

(check-redundancy #t)

(provide Rbase-l)

(define call-lazy-extra/l3
  (reduction-relation
    L3/K
    #:domain config
    ;; Stage 1 (inside active branch): call contexts from L1.
    ;; Stage 2 (outside): left-disjunction scheduler contexts.
    [--> (Γ ans* (in-hole Kleft (in-hole Kcall ((r t ... tag) σ))))
         (Γ ans* (in-hole Kleft (in-hole Kcall (delay (proceed ((r t ... tag) σ)))))
                 )
         "call/lazy-suspend-call"]

    [--> (Γ ans* (in-hole Kleft (in-hole Kcall (delay (proceed ((r t ... tag) σ))))))
         (Γ ans* (in-hole Kleft (in-hole Kcall (proceed ((r t ... tag) σ))))
                 )
         "call/lazy-invoke-delay"]

    [--> (Γ ans* (in-hole Kleft (in-hole Kcall (proceed ((r t ... tag) σ)))))
         (Γ ans* (in-hole Kleft (in-hole Kcall (g_new σ))))
         (where g_new ,(instantiate-call-host (term Γ) (term r) (term (t ...))))
         "call/lazy-expand-on-resume"]))

(define disj-extra/l3
  (reduction-relation
    L3/K
    #:domain config
    [--> (Γ ans* (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))))
         (Γ ans* (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))))
         "disj/goal-to-tree"]

    [--> (Γ ans* (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))))
         (Γ ans* (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))))
         "disj/distribute-over-conj"]

    [--> (Γ (σ ...) (in-hole Kleft ((⊤ σ_new) <-+ s_right)))
         (Γ (σ ... σ_new) (in-hole Kleft s_right))
         "disj/collect-left-answer"]

    [--> (Γ ans* (in-hole Kleft ((empty-tree) <-+ s_right)))
         (Γ ans* (in-hole Kleft s_right))
         "disj/skip-left-fail"]))

(define Rbase-l
  (union-reduction-relations
   call-lazy-extra/l3
   disj-extra/l3
   core-base-extra-l3))
