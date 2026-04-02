#lang racket

(require redex/reduction-semantics
         "./context-l2.rkt"
         "./core-l2.rkt")

(check-redundancy #t)

(provide disj-extra/dfs-nodelay
         Rdfs-nodelay)

;; Explicit DFS/no-delay branch:
;; - built from L2 (disjunction syntax present)
;; - no delay/proceed constructors or rules
;; - deterministic left-biased disjunction scheduling
(define disj-extra/dfs-nodelay
  (reduction-relation
   L2/K
   #:domain config
   [--> (Γ (in-hole Kleft (in-hole Kcore ((g_1 ∨ g_2 tag) σ))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((g_1 σ) <-+ (g_2 σ)))) as)
        "dfsn/goal-to-tree"]
   [--> (Γ (in-hole Kleft (in-hole Kcore ((s_1 <-+ s_2) × g c))) as)
        (Γ (in-hole Kleft (in-hole Kcore ((s_1 × g c) <-+ (s_2 × g c)))) as)
        "dfsn/distribute-over-conj"]
   [--> (Γ (in-hole Kleft ((emit σ_new s_left_tail) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new (s_left_tail <-+ s_right))) as)
        (side-condition (not (redex-match? L2/K (empty-tree) (term s_left_tail))))
        "dfsn/promote-left-stream"]
   [--> (Γ (in-hole Kleft ((emit σ_new (empty-tree)) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new s_right)) as)
        "dfsn/promote-left-singleton-stream"]
   [--> (Γ (in-hole Kleft ((⊤ σ_new) <-+ s_right)) as)
        (Γ (in-hole Kleft (emit σ_new s_right)) as)
        "dfsn/promote-left-answer"]
   [--> (Γ (in-hole Kleft ((empty-tree) <-+ s_right)) as)
        (Γ (in-hole Kleft s_right) as)
        "dfsn/skip-left-fail"]))

(define Rdfs-nodelay
  (union-reduction-relations
   disj-extra/dfs-nodelay
   core-cfg/l2))
