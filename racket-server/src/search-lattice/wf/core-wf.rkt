#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "./kernel.rkt")

(provide (all-from-out "./kernel.rkt")
         wf-goal/core?
         wf-tree/core?
         wf-answer-stream/core?
         wf-cfg/core?)

(check-redundancy #t)

(define-judgment-form
  core-lang
  #:contract (wf-goal/core? g (x_1 ...) c)
  #:mode (wf-goal/core? I I I)
  [------------------ "trivial success wf/core"
   (wf-goal/core? (succeed tag) (x_1 ...) c)]
  [------------------ "trivial fail wf/core"
   (wf-goal/core? (fail tag) (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/core? g (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/core"
   (wf-goal/core? (∃ (x_1 ...) g tag) (x_2 ...) c)]
  [(wf-goal/core? g_1 (x_1 ...) c)
   (wf-goal/core? g_2 (x_1 ...) c)
   ---------- "conj-wf/core"
   (wf-goal/core? (g_1 ∧ g_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "==-wf/core"
   (wf-goal/core? (t_1 =? t_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "=/=-wf/core"
   (wf-goal/core? (t_1 != t_2 tag) (x_1 ...) c)])

(define-judgment-form
  core-lang
  #:contract (wf-tree/core? s c)
  #:mode (wf-tree/core? I I)
  [------------------- "empty tree is wf/core"
   (wf-tree/core? (empty-tree) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/core"
   (wf-tree/core? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-goal/core? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/core"
   (wf-tree/core? (g (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-tree/core? s c_i)
   (wf-goal/core? g () c_i)
   ------------------- "conj wf/core"
   (wf-tree/core? (s × g c_i) c)])

(define-judgment-form
  core-lang
  #:contract (wf-answer-stream/core? as c)
  #:mode (wf-answer-stream/core? I I)
  [------------------- "empty answer stream wf/core"
   (wf-answer-stream/core? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/core"
   (wf-answer-stream/core? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/core? as_tail c)
   ------------------- "answer stream wf/core"
   (wf-answer-stream/core? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  core-lang
  #:contract (wf-cfg/core? cfg)
  #:mode (wf-cfg/core? I)
  [(wf-tree/core? s ())
   (wf-answer-stream/core? as ())
   ----------------------- "cfg-wf/core"
   (wf-cfg/core? (s as))])
