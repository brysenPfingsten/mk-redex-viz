#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/delay?
         wf-tree/delay?
         wf-answer-stream/delay?
         wf-cfg/delay?)

(check-redundancy #t)

(define-judgment-form
  delay-lang
  #:contract (wf-goal/delay? g (x_1 ...) c)
  #:mode (wf-goal/delay? I I I)
  [------------------ "trivial success wf/delay"
   (wf-goal/delay? (succeed tag) (x_1 ...) c)]
  [------------------ "trivial fail wf/delay"
   (wf-goal/delay? (fail tag) (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/delay? g (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/delay"
   (wf-goal/delay? (∃ (x_1 ...) g tag) (x_2 ...) c)]
  [(wf-goal/delay? g_1 (x_1 ...) c)
   (wf-goal/delay? g_2 (x_1 ...) c)
   ------------------- "conj-wf/delay"
   (wf-goal/delay? (g_1 ∧ g_2 tag) (x_1 ...) c)]
  [(wf-goal/delay? g (x_1 ...) c)
   ------------------- "delay-goal-wf/delay"
   (wf-goal/delay? (suspend g tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/delay"
   (wf-goal/delay? (t_1 =? t_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/delay"
   (wf-goal/delay? (t_1 != t_2 tag) (x_1 ...) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-tree/delay? s c)
  #:mode (wf-tree/delay? I I)
  [------------------- "empty tree is wf/delay"
   (wf-tree/delay? (empty-tree) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/delay"
   (wf-tree/delay? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-goal/delay? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/delay"
   (wf-tree/delay? (g (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-tree/delay? s c_i)
   (wf-goal/delay? g () c_i)
   ------------------- "conj wf/delay"
   (wf-tree/delay? (s × g c_i) c)]
  [(wf-tree/delay? s c)
   ------------------- "delay wf/delay"
   (wf-tree/delay? (delay s) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-answer-stream/delay? as c)
  #:mode (wf-answer-stream/delay? I I)
  [------------------- "empty answer stream wf/delay"
   (wf-answer-stream/delay? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/delay"
   (wf-answer-stream/delay? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/delay? as_tail c)
   ------------------- "answer stream wf/delay"
   (wf-answer-stream/delay? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-cfg/delay? cfg)
  #:mode (wf-cfg/delay? I)
  [(wf-tree/delay? s ())
   (wf-answer-stream/delay? as ())
   ----------------------- "cfg-wf/delay"
   (wf-cfg/delay? (s as))])
