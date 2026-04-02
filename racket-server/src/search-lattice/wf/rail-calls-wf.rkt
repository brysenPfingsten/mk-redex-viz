#lang racket

(require redex/reduction-semantics
         "../languages/rail-seq-calls-lang.rkt"
         "./calls-arity.rkt"
         "./core-wf.rkt")

(provide wf-goal/rail-calls?
         wf-tree/rail-calls?
         wf-answer-stream/rail-calls?
         wf-rel-env/rail-calls?
         wf-config/rail-calls?)

(check-redundancy #t)

(define-metafunction
  rail-seq-calls-lang
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...))
   ,(relcall-arity-ok/host (term r_call)
                           (term (t ...))
                           (term ((r_1 d_1 g_1) ...)))])

(define-judgment-form
  rail-seq-calls-lang
  #:contract (wf-goal/rail-calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/rail-calls? I I I I)
  [------------------ "trivial success wf/rail-calls"
   (wf-goal/rail-calls? (succeed tag) Γ (x_1 ...) c)]
  [------------------ "trivial fail wf/rail-calls"
   (wf-goal/rail-calls? (fail tag) Γ (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/rail-calls? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/rail-calls"
   (wf-goal/rail-calls? (∃ (x_1 ...) g tag) Γ (x_2 ...) c)]
  [(wf-goal/rail-calls? g_1 Γ (x_1 ...) c)
   (wf-goal/rail-calls? g_2 Γ (x_1 ...) c)
   ------------------- "conj-wf/rail-calls"
   (wf-goal/rail-calls? (g_1 ∧ g_2 tag) Γ (x_1 ...) c)]
  [(wf-goal/rail-calls? g_1 Γ (x_1 ...) c)
   (wf-goal/rail-calls? g_2 Γ (x_1 ...) c)
   ------------------- "disj-wf/rail-calls"
   (wf-goal/rail-calls? (g_1 ∨ g_2 tag) Γ (x_1 ...) c)]
  [(wf-goal/rail-calls? g Γ (x_1 ...) c)
   ------------------- "delay-goal-wf/rail-calls"
   (wf-goal/rail-calls? (suspend g tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/rail-calls"
   (wf-goal/rail-calls? (t_1 =? t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/rail-calls"
   (wf-goal/rail-calls? (t_1 != t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   ------------------- "relcall-wf/rail-calls"
   (wf-goal/rail-calls? (r t ... tag)
                        ((r_1 d_1 g_1) ...)
                        (x_1 ...)
                        c)])

(define-judgment-form
  rail-seq-calls-lang
  #:contract (wf-tree/rail-calls? s Γ c)
  #:mode (wf-tree/rail-calls? I I I)
  [------------------- "empty tree is wf/rail-calls"
   (wf-tree/rail-calls? (empty-tree) Γ c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/rail-calls"
   (wf-tree/rail-calls? (⊤ (state sub dis c_i trail tag)) Γ c)]
  [(lvars-subset? c c_i)
   (wf-goal/rail-calls? g Γ () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/rail-calls"
   (wf-tree/rail-calls? (g (state sub dis c_i trail tag)) Γ c)]
  [(lvars-subset? c c_i)
   (wf-tree/rail-calls? s Γ c_i)
   (wf-goal/rail-calls? g Γ () c_i)
   ------------------- "conj wf/rail-calls"
   (wf-tree/rail-calls? (s × g c_i) Γ c)]
  [(wf-tree/rail-calls? s_1 Γ c)
   (wf-tree/rail-calls? s_2 Γ c)
   ------------------- "left disj wf/rail-calls"
   (wf-tree/rail-calls? (s_1 <-+ s_2) Γ c)]
  [(wf-tree/rail-calls? s_1 Γ c)
   (wf-tree/rail-calls? s_2 Γ c)
   ------------------- "right disj wf/rail-calls"
   (wf-tree/rail-calls? (s_1 +-> s_2) Γ c)]
  [(wf-tree/rail-calls? s Γ c)
   ------------------- "delay wf/rail-calls"
   (wf-tree/rail-calls? (delay s) Γ c)])

(define-judgment-form
  rail-seq-calls-lang
  #:contract (wf-answer-stream/rail-calls? as c)
  #:mode (wf-answer-stream/rail-calls? I I)
  [------------------- "empty answer stream wf/rail-calls"
   (wf-answer-stream/rail-calls? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/rail-calls"
   (wf-answer-stream/rail-calls? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/rail-calls? as_tail c)
   ------------------- "answer stream wf/rail-calls"
   (wf-answer-stream/rail-calls? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  rail-seq-calls-lang
  #:contract (wf-rel-env/rail-calls? Γ)
  #:mode (wf-rel-env/rail-calls? I)
  [(wf-goal/rail-calls? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/rail-calls"
   (wf-rel-env/rail-calls? ((r d g) ...))])

(define-judgment-form
  rail-seq-calls-lang
  #:contract (wf-config/rail-calls? config)
  #:mode (wf-config/rail-calls? I)
  [(wf-rel-env/rail-calls? Γ)
   (wf-tree/rail-calls? s Γ ())
   (wf-answer-stream/rail-calls? as ())
   ----------------------- "program-wf/rail-calls"
   (wf-config/rail-calls? (Γ (s as)))])
