#lang racket

(require redex/reduction-semantics
         "./canonical-core-lang.rkt"
         "../../search-lattice/wf/kernel.rkt")

(provide (all-from-out "../../search-lattice/wf/kernel.rkt")
         core-goal-shape?/canonical
         core-work-shape?/canonical
         core-shape?/canonical
         wf-goal/canonical-core?
         wf-work/canonical-core?
         wf-rel-env/canonical-core?
         wf-config/canonical-core?)

(check-redundancy #t)

(define-judgment-form
  canonical-core-lang
  #:contract (wf-goal/canonical-core? g Gamma (x_1 ...) c)
  #:mode (wf-goal/canonical-core? I I I I)
  [------------------ "trivial success wf/canonical-core"
   (wf-goal/canonical-core? (succeed tag) Gamma (x_1 ...) c)]
  [------------------ "trivial fail wf/canonical-core"
   (wf-goal/canonical-core? (fail tag) Gamma (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/canonical-core? g Gamma (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/canonical-core"
   (wf-goal/canonical-core? (∃ (x_1 ...) g tag) Gamma (x_2 ...) c)]
  [(wf-goal/canonical-core? g_1 Gamma (x_1 ...) c)
   (wf-goal/canonical-core? g_2 Gamma (x_1 ...) c)
   ------------------- "conj-wf/canonical-core"
   (wf-goal/canonical-core? (g_1 ∧ g_2 tag) Gamma (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/canonical-core"
   (wf-goal/canonical-core? (t_1 =? t_2 tag) Gamma (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/canonical-core"
   (wf-goal/canonical-core? (t_1 != t_2 tag) Gamma (x_1 ...) c)])

(define-judgment-form
  canonical-core-lang
  #:contract (core-goal-shape?/canonical g)
  #:mode (core-goal-shape?/canonical I)
  [------------------- "core-succeed-shape/canonical"
   (core-goal-shape?/canonical (succeed tag))]
  [------------------- "core-fail-shape/canonical"
   (core-goal-shape?/canonical (fail tag))]
  [------------------- "core-eq-shape/canonical"
   (core-goal-shape?/canonical (t_1 =? t_2 tag))]
  [------------------- "core-diseq-shape/canonical"
   (core-goal-shape?/canonical (t_1 != t_2 tag))]
  [(core-goal-shape?/canonical g_1)
   (core-goal-shape?/canonical g_2)
   ------------------- "core-conj-shape/canonical"
   (core-goal-shape?/canonical (g_1 ∧ g_2 tag))]
  [(core-goal-shape?/canonical g)
   ------------------- "core-exists-shape/canonical"
   (core-goal-shape?/canonical (∃ d g tag))])

(define-judgment-form
  canonical-core-lang
  #:contract (core-work-shape?/canonical w)
  #:mode (core-work-shape?/canonical I)
  [------------------- "core-empty-tree-shape/canonical"
   (core-work-shape?/canonical (empty-tree))]
  [------------------- "core-answer-shape/canonical"
   (core-work-shape?/canonical (⊤ σ))]
  [(core-goal-shape?/canonical g)
   ------------------- "core-goal-state-shape/canonical"
   (core-work-shape?/canonical (g σ))]
  [(core-work-shape?/canonical w)
   (core-goal-shape?/canonical g)
   ------------------- "core-conj-tree-shape/canonical"
   (core-work-shape?/canonical (w × g c))])

(define-judgment-form
  canonical-core-lang
  #:contract (core-shape?/canonical config)
  #:mode (core-shape?/canonical I)
  [(core-goal-shape?/canonical g) ...
   (core-work-shape?/canonical w)
   ------------------- "core-config-shape/canonical"
   (core-shape?/canonical (((r d g) ...) w))])

(define-judgment-form
  canonical-core-lang
  #:contract (wf-work/canonical-core? w Gamma c)
  #:mode (wf-work/canonical-core? I I I)
  [------------------- "empty tree is wf/canonical-core"
   (wf-work/canonical-core? (empty-tree) Gamma c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "single answer/state wf/canonical-core"
   (wf-work/canonical-core? (⊤ (state sub dis c_i trail tag)) Gamma c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/canonical-core? g Gamma () c_i)
   ------------------- "goal/state wf/canonical-core"
   (wf-work/canonical-core? (g (state sub dis c_i trail tag)) Gamma c)]
  [(lvars-same-members? c c_i)
   (wf-work/canonical-core? w Gamma c_i)
   (wf-goal/canonical-core? g Gamma () c_i)
   ------------------- "conj wf/canonical-core"
   (wf-work/canonical-core? (w × g c_i) Gamma c)])

(define-judgment-form
  canonical-core-lang
  #:contract (wf-rel-env/canonical-core? Gamma)
  #:mode (wf-rel-env/canonical-core? I)
  [(wf-goal/canonical-core? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/canonical-core"
   (wf-rel-env/canonical-core? ((r d g) ...))])

(define-judgment-form
  canonical-core-lang
  #:contract (wf-config/canonical-core? config)
  #:mode (wf-config/canonical-core? I)
  [(wf-rel-env/canonical-core? ((r d g) ...))
   (wf-work/canonical-core? w ((r d g) ...) ())
   ----------------------- "program-wf/canonical-core"
   (wf-config/canonical-core? (((r d g) ...) w))])
