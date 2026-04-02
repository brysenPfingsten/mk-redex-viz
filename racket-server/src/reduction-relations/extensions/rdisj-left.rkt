#lang racket

(require redex/reduction-semantics
         "./context-l2.rkt"
         "./core-l2.rkt")

(check-redundancy #t)

(provide disj-extra/l2
         Rdisj-left)

(define disj-expand-only/l2
  (reduction-relation
   L2/K
   #:domain config
   [--> (Γ (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))) as)
        "disj/goal-to-tree"]
   [--> (Γ (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))) as)
        "disj/distribute-over-conj"]))

(define disj-scheduler-only/l2
  (reduction-relation
   L2/K
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

(define disj-extra/l2
  (reduction-relation
   L2/K
   #:domain config
   ;; Stage 1 (inside active branch): core-conjunction contexts.
   ;; Stage 2 (outside): left-disjunction scheduler contexts.
   [--> (Γ (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))) as)
        "disj/goal-to-tree"]
   [--> (Γ (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))) as)
        "disj/distribute-over-conj"]
   [--> (Γ (in-hole Kleft ((emit σ_new s_left_tail) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new (s_left_tail <-+ s_right))) as)
        (side-condition (not (redex-match? L2/K (empty-tree) (term s_left_tail))))
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

(define Rdisj-left
  (union-reduction-relations
   disj-extra/l2
   core-cfg/l2))
