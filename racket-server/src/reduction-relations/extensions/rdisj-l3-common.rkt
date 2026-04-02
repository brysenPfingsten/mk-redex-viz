#lang racket

(require redex/reduction-semantics
         "./context-l3.rkt")

(check-redundancy #t)

(provide disj-distribute-only/l3
         make-disj-extra/l3)

(define (contains-active-right-disj? t)
  (match t
    ;; A right-disjunction under delay is inactive until delay is invoked.
    [`(delay ,_) #f]
    [`(,_ +-> ,_) #t]
    ['() #f]
    [(cons a d) (or (contains-active-right-disj? a)
                    (contains-active-right-disj? d))]
    [_ #f]))

(define disj-distribute-only/l3
  (reduction-relation
   L3/K
   #:domain config
   [--> (Γ (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))) as)
        "disj/goal-to-tree"]
   [--> (Γ (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))) as)
        (side-condition (redex-match? L3/K s (term s_1)))
        (side-condition (redex-match? L3/K s (term s_2)))
        "disj/distribute-over-conj"]))

(define disj-scheduler-only/l3
  (reduction-relation
   L3/K
   #:domain config
   [--> (Γ (in-hole Kleft ((emit σ_new s_left_tail) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new (s_left_tail <-+ s_right))) as)
        "disj/promote-left-stream"]
   [--> (Γ (in-hole Kleft ((emit σ_new (empty-tree)) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new s_right)) as)
        "disj/promote-left-singleton-stream"]
   [--> (Γ (in-hole Kleft ((⊤ σ_new) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new s_right)) as)
        "disj/promote-left-answer"]
   [--> (Γ (in-hole Kleft ((empty-tree) <-+ s_right)) as)
        (Γ (in-hole Kleft s_right) as)
        "disj/skip-left-fail"]))

(define (make-disj-extra/l3 call+core-rel)
  (reduction-relation
   L3/K
   #:domain config
   [--> (Γ (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))) as)
        "disj/goal-to-tree"]
   [--> (Γ (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))) as)
        (side-condition (redex-match? L3/K s (term s_1)))
        (side-condition (redex-match? L3/K s (term s_2)))
        "disj/distribute-over-conj"]
   [--> (Γ (in-hole Kleft ((emit σ_new s_left_tail) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new (s_left_tail <-+ s_right))) as)
        ;; Structural disjointness with railroad promotions:
        ;; left-stream promotion only applies when no active +-> appears in
        ;; the pending left tail.
        (side-condition
         (not (contains-active-right-disj? (term s_left_tail))))
        (side-condition
         (not (redex-match? L3/K ((delay s_1) <-+ s_2) (term s_left_tail))))
        (side-condition (not (redex-match? L3/K (empty-tree) (term s_left_tail))))
        "disj/promote-left-stream"]
   [--> (Γ (in-hole Kleft ((emit σ_new (empty-tree)) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new s_right)) as)
        "disj/promote-left-singleton-stream"]
   [--> (Γ (in-hole Kleft ((⊤ σ_new) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new s_right)) as)
        "disj/promote-left-answer"]
   [--> (Γ (in-hole Kleft ((empty-tree) <-+ s_right)) as)
        (Γ (in-hole Kleft s_right) as)
        "disj/skip-left-fail"]))
