#lang racket

(require redex/reduction-semantics
         "../languages/canonical-core-lang.rkt"
         "./kernel.rkt")

(provide (all-from-out "./kernel.rkt")
         core-goal-shape?/canonical
         core-tree-shape?/canonical
         core-answer-stream-shape?/canonical
         core-shape?/canonical
         wf-goal/canonical-core?
         wf-tree/canonical-core?
         wf-answer-stream/canonical-core?
         wf-rel-env/canonical-core?
         wf-config/canonical-core?)

(check-redundancy #t)

(define-judgment-form
  canonical-core-lang
  #:contract (wf-goal/canonical-core? g Γ (x_1 ...) c)
  #:mode (wf-goal/canonical-core? I I I I)
  [------------------ "trivial success wf/canonical-core"
   (wf-goal/canonical-core? (succeed tag) Γ (x_1 ...) c)]
  [------------------ "trivial fail wf/canonical-core"
   (wf-goal/canonical-core? (fail tag) Γ (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/canonical-core? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/canonical-core"
   (wf-goal/canonical-core? (∃ (x_1 ...) g tag) Γ (x_2 ...) c)]
  [(wf-goal/canonical-core? g_1 Γ (x_1 ...) c)
   (wf-goal/canonical-core? g_2 Γ (x_1 ...) c)
   ------------------- "conj-wf/canonical-core"
   (wf-goal/canonical-core? (g_1 ∧ g_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/canonical-core"
   (wf-goal/canonical-core? (t_1 =? t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/canonical-core"
   (wf-goal/canonical-core? (t_1 != t_2 tag) Γ (x_1 ...) c)])

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
  #:contract (core-tree-shape?/canonical s)
  #:mode (core-tree-shape?/canonical I)
  [------------------- "core-empty-tree-shape/canonical"
   (core-tree-shape?/canonical (empty-tree))]
  [------------------- "core-answer-shape/canonical"
   (core-tree-shape?/canonical (⊤ σ))]
  [(core-goal-shape?/canonical g)
   ------------------- "core-goal-state-shape/canonical"
   (core-tree-shape?/canonical (g σ))]
  [(core-tree-shape?/canonical s)
   (core-goal-shape?/canonical g)
   ------------------- "core-conj-tree-shape/canonical"
   (core-tree-shape?/canonical (s × g c))])

(define-judgment-form
  canonical-core-lang
  #:contract (core-answer-stream-shape?/canonical as)
  #:mode (core-answer-stream-shape?/canonical I)
  [------------------- "core-empty-answer-stream-shape/canonical"
   (core-answer-stream-shape?/canonical (empty-stream))]
  [------------------- "core-single-answer-stream-shape/canonical"
   (core-answer-stream-shape?/canonical (⊤ σ))]
  [(core-answer-stream-shape?/canonical as_tail)
   ------------------- "core-answer-stream-tail-shape/canonical"
   (core-answer-stream-shape?/canonical ((⊤ σ) + as_tail))])

(define-judgment-form
  canonical-core-lang
  #:contract (core-shape?/canonical config)
  #:mode (core-shape?/canonical I)
  [(core-goal-shape?/canonical g) ...
   (core-tree-shape?/canonical s)
   (core-answer-stream-shape?/canonical as)
   ------------------- "core-config-shape/canonical"
   (core-shape?/canonical (((r d g) ...) s as))])

(define-judgment-form
  canonical-core-lang
  #:contract (wf-tree/canonical-core? s Γ c)
  #:mode (wf-tree/canonical-core? I I I)
  [------------------- "empty tree is wf/canonical-core"
   (wf-tree/canonical-core? (empty-tree) Γ c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/canonical-core"
   (wf-tree/canonical-core? (⊤ (state sub dis c_i trail tag)) Γ c)]
  [(lvars-subset? c c_i)
   (wf-goal/canonical-core? g Γ () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/canonical-core"
   (wf-tree/canonical-core? (g (state sub dis c_i trail tag)) Γ c)]
  [(lvars-subset? c c_i)
   (wf-tree/canonical-core? s Γ c_i)
   (wf-goal/canonical-core? g Γ () c_i)
   ------------------- "conj wf/canonical-core"
   (wf-tree/canonical-core? (s × g c_i) Γ c)])

(define-judgment-form
  canonical-core-lang
  #:contract (wf-answer-stream/canonical-core? as c)
  #:mode (wf-answer-stream/canonical-core? I I)
  [------------------- "empty answer stream wf/canonical-core"
   (wf-answer-stream/canonical-core? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/canonical-core"
   (wf-answer-stream/canonical-core? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/canonical-core? as_tail c)
   ------------------- "answer stream wf/canonical-core"
   (wf-answer-stream/canonical-core? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  canonical-core-lang
  #:contract (wf-rel-env/canonical-core? Γ)
  #:mode (wf-rel-env/canonical-core? I)
  [(wf-goal/canonical-core? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/canonical-core"
   (wf-rel-env/canonical-core? ((r d g) ...))])

(define-judgment-form
  canonical-core-lang
  #:contract (wf-config/canonical-core? config)
  #:mode (wf-config/canonical-core? I)
  [(wf-rel-env/canonical-core? ((r d g) ...))
   (wf-tree/canonical-core? s ((r d g) ...) ())
   (wf-answer-stream/canonical-core? as ())
   ----------------------- "program-wf/canonical-core"
   (wf-config/canonical-core? (((r d g) ...) s as))])
