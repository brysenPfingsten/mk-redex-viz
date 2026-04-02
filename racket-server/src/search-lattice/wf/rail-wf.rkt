#lang racket

(require redex/reduction-semantics
         "../languages/rail-seq-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/rail?
         wf-tree/rail?
         wf-answer-stream/rail?
         wf-cfg/rail?)

(check-redundancy #t)

(define-judgment-form
  rail-seq-lang
  #:contract (wf-goal/rail? g (x_1 ...) c)
  #:mode (wf-goal/rail? I I I)
  [------------------ "trivial success wf/rail"
   (wf-goal/rail? (succeed tag) (x_1 ...) c)]
  [------------------ "trivial fail wf/rail"
   (wf-goal/rail? (fail tag) (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/rail? g (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/rail"
   (wf-goal/rail? (∃ (x_1 ...) g tag) (x_2 ...) c)]
  [(wf-goal/rail? g_1 (x_1 ...) c)
   (wf-goal/rail? g_2 (x_1 ...) c)
   ------------------- "conj-wf/rail"
   (wf-goal/rail? (g_1 ∧ g_2 tag) (x_1 ...) c)]
  [(wf-goal/rail? g_1 (x_1 ...) c)
   (wf-goal/rail? g_2 (x_1 ...) c)
   ------------------- "disj-wf/rail"
   (wf-goal/rail? (g_1 ∨ g_2 tag) (x_1 ...) c)]
  [(wf-goal/rail? g (x_1 ...) c)
   ------------------- "delay-goal-wf/rail"
   (wf-goal/rail? (suspend g tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/rail"
   (wf-goal/rail? (t_1 =? t_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/rail"
   (wf-goal/rail? (t_1 != t_2 tag) (x_1 ...) c)])

(define-judgment-form
  rail-seq-lang
  #:contract (wf-tree/rail? s c)
  #:mode (wf-tree/rail? I I)
  [------------------- "empty tree is wf/rail"
   (wf-tree/rail? (empty-tree) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/rail"
   (wf-tree/rail? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-goal/rail? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/rail"
   (wf-tree/rail? (g (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-tree/rail? s c_i)
   (wf-goal/rail? g () c_i)
   ------------------- "conj wf/rail"
   (wf-tree/rail? (s × g c_i) c)]
  [(wf-tree/rail? s_1 c)
   (wf-tree/rail? s_2 c)
   ------------------- "left disj wf/rail"
   (wf-tree/rail? (s_1 <-+ s_2) c)]
  [(wf-tree/rail? s_1 c)
   (wf-tree/rail? s_2 c)
   ------------------- "right disj wf/rail"
   (wf-tree/rail? (s_1 +-> s_2) c)]
  [(wf-tree/rail? s c)
   ------------------- "delay wf/rail"
   (wf-tree/rail? (delay s) c)])

(define-judgment-form
  rail-seq-lang
  #:contract (wf-answer-stream/rail? as c)
  #:mode (wf-answer-stream/rail? I I)
  [------------------- "empty answer stream wf/rail"
   (wf-answer-stream/rail? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/rail"
   (wf-answer-stream/rail? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/rail? as_tail c)
   ------------------- "answer stream wf/rail"
   (wf-answer-stream/rail? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  rail-seq-lang
  #:contract (wf-cfg/rail? cfg)
  #:mode (wf-cfg/rail? I)
  [(wf-tree/rail? s ())
   (wf-answer-stream/rail? as ())
   ----------------------- "cfg-wf/rail"
   (wf-cfg/rail? (s as))])
