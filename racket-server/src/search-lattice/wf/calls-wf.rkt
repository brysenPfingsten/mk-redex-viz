#lang racket

(require redex/reduction-semantics
         "../languages/calls-lang.rkt"
         "./calls-arity.rkt"
         "./core-wf.rkt")

(provide wf-goal/calls?
         wf-tree/calls?
         wf-answer-stream/calls?
         wf-rel-env/calls?
         wf-config/calls?)

(check-redundancy #t)

(define-metafunction
  calls-lang
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...))
   ,(relcall-arity-ok/host (term r_call)
                           (term (t ...))
                           (term ((r_1 d_1 g_1) ...)))])

(define-judgment-form
  calls-lang
  #:contract (wf-goal/calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/calls? I I I I)
  [------------------ "trivial success wf/calls"
   (wf-goal/calls? (succeed tag) Γ (x_1 ...) c)]
  [------------------ "trivial fail wf/calls"
   (wf-goal/calls? (fail tag) Γ (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/calls? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/calls"
   (wf-goal/calls? (∃ (x_1 ...) g tag) Γ (x_2 ...) c)]
  [(wf-goal/calls? g_1 Γ (x_1 ...) c)
   (wf-goal/calls? g_2 Γ (x_1 ...) c)
   ------------------- "conj-wf/calls"
   (wf-goal/calls? (g_1 ∧ g_2 tag) Γ (x_1 ...) c)]
  [(wf-goal/calls? g Γ (x_1 ...) c)
   ------------------- "delay-goal-wf/calls"
   (wf-goal/calls? (suspend g tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/calls"
   (wf-goal/calls? (t_1 =? t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/calls"
   (wf-goal/calls? (t_1 != t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   ------------------- "relcall-wf/calls"
   (wf-goal/calls? (r t ... tag)
                   ((r_1 d_1 g_1) ...)
                   (x_1 ...)
                   c)])

(define-judgment-form
  calls-lang
  #:contract (wf-tree/calls? s Γ c)
  #:mode (wf-tree/calls? I I I)
  [------------------- "empty tree is wf/calls"
   (wf-tree/calls? (empty-tree) Γ c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/calls"
   (wf-tree/calls? (⊤ (state sub dis c_i trail tag)) Γ c)]
  [(lvars-subset? c c_i)
   (wf-goal/calls? g Γ () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/calls"
   (wf-tree/calls? (g (state sub dis c_i trail tag)) Γ c)]
  [(lvars-subset? c c_i)
   (wf-tree/calls? s Γ c_i)
   (wf-goal/calls? g Γ () c_i)
   ------------------- "conj wf/calls"
   (wf-tree/calls? (s × g c_i) Γ c)]
  [(wf-tree/calls? s Γ c)
   ------------------- "delay wf/calls"
   (wf-tree/calls? (delay s) Γ c)])

(define-judgment-form
  calls-lang
  #:contract (wf-answer-stream/calls? as c)
  #:mode (wf-answer-stream/calls? I I)
  [------------------- "empty answer stream wf/calls"
   (wf-answer-stream/calls? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/calls"
   (wf-answer-stream/calls? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/calls? as_tail c)
   ------------------- "answer stream wf/calls"
   (wf-answer-stream/calls? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  calls-lang
  #:contract (wf-rel-env/calls? Γ)
  #:mode (wf-rel-env/calls? I)
  [(wf-goal/calls? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/calls"
   (wf-rel-env/calls? ((r d g) ...))])

(define-judgment-form
  calls-lang
  #:contract (wf-config/calls? config)
  #:mode (wf-config/calls? I)
  [(wf-rel-env/calls? Γ)
   (wf-tree/calls? s Γ ())
   (wf-answer-stream/calls? as ())
   ----------------------- "program-wf/calls"
   (wf-config/calls? (Γ (s as)))])
