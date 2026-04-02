#lang racket

(require redex/reduction-semantics
         "../languages/disj-seq-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/disj?
         wf-tree/disj?
         wf-answer-stream/disj?
         wf-cfg/disj?)

(check-redundancy #t)

(define-judgment-form
  disj-seq-lang
  #:contract (wf-goal/disj? g (x_1 ...) c)
  #:mode (wf-goal/disj? I I I)
  [------------------ "trivial success wf/disj"
   (wf-goal/disj? (succeed tag) (x_1 ...) c)]
  [------------------ "trivial fail wf/disj"
   (wf-goal/disj? (fail tag) (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/disj? g (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/disj"
   (wf-goal/disj? (∃ (x_1 ...) g tag) (x_2 ...) c)]
  [(wf-goal/disj? g_1 (x_1 ...) c)
   (wf-goal/disj? g_2 (x_1 ...) c)
   ------------------- "conj-wf/disj"
   (wf-goal/disj? (g_1 ∧ g_2 tag) (x_1 ...) c)]
  [(wf-goal/disj? g_1 (x_1 ...) c)
   (wf-goal/disj? g_2 (x_1 ...) c)
   ------------------- "disj-wf/disj"
   (wf-goal/disj? (g_1 ∨ g_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/disj"
   (wf-goal/disj? (t_1 =? t_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/disj"
   (wf-goal/disj? (t_1 != t_2 tag) (x_1 ...) c)])

(define-judgment-form
  disj-seq-lang
  #:contract (wf-tree/disj? s c)
  #:mode (wf-tree/disj? I I)
  [------------------- "empty tree is wf/disj"
   (wf-tree/disj? (empty-tree) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/disj"
   (wf-tree/disj? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-goal/disj? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/disj"
   (wf-tree/disj? (g (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-tree/disj? s c_i)
   (wf-goal/disj? g () c_i)
   ------------------- "conj wf/disj"
   (wf-tree/disj? (s × g c_i) c)]
  [(wf-tree/disj? s_1 c)
   (wf-tree/disj? s_2 c)
   ------------------- "left disj wf/disj"
   (wf-tree/disj? (s_1 <-+ s_2) c)])

(define-judgment-form
  disj-seq-lang
  #:contract (wf-answer-stream/disj? as c)
  #:mode (wf-answer-stream/disj? I I)
  [------------------- "empty answer stream wf/disj"
   (wf-answer-stream/disj? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/disj"
   (wf-answer-stream/disj? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/disj? as_tail c)
   ------------------- "answer stream wf/disj"
   (wf-answer-stream/disj? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  disj-seq-lang
  #:contract (wf-cfg/disj? cfg)
  #:mode (wf-cfg/disj? I)
  [(wf-tree/disj? s ())
   (wf-answer-stream/disj? as ())
   ----------------------- "cfg-wf/disj"
   (wf-cfg/disj? (s as))])
