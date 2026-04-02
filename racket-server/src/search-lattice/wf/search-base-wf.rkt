#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/search-base?
         wf-tree/search-base?
         wf-answer-stream/search-base?
         wf-cfg/search-base?)

(check-redundancy #t)

(define-judgment-form
  search-base-seq-lang
  #:contract (wf-goal/search-base? g (x_1 ...) c)
  #:mode (wf-goal/search-base? I I I)
  [------------------ "trivial success wf/search-base"
   (wf-goal/search-base? (succeed tag) (x_1 ...) c)]
  [------------------ "trivial fail wf/search-base"
   (wf-goal/search-base? (fail tag) (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/search-base? g (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/search-base"
   (wf-goal/search-base? (∃ (x_1 ...) g tag) (x_2 ...) c)]
  [(wf-goal/search-base? g_1 (x_1 ...) c)
   (wf-goal/search-base? g_2 (x_1 ...) c)
   ------------------- "conj-wf/search-base"
   (wf-goal/search-base? (g_1 ∧ g_2 tag) (x_1 ...) c)]
  [(wf-goal/search-base? g_1 (x_1 ...) c)
   (wf-goal/search-base? g_2 (x_1 ...) c)
   ------------------- "disj-wf/search-base"
   (wf-goal/search-base? (g_1 ∨ g_2 tag) (x_1 ...) c)]
  [(wf-goal/search-base? g (x_1 ...) c)
   ------------------- "delay-goal-wf/search-base"
   (wf-goal/search-base? (suspend g tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/search-base"
   (wf-goal/search-base? (t_1 =? t_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/search-base"
   (wf-goal/search-base? (t_1 != t_2 tag) (x_1 ...) c)])

(define-judgment-form
  search-base-seq-lang
  #:contract (wf-tree/search-base? s c)
  #:mode (wf-tree/search-base? I I)
  [------------------- "empty tree is wf/search-base"
   (wf-tree/search-base? (empty-tree) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/search-base"
   (wf-tree/search-base? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-goal/search-base? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/search-base"
   (wf-tree/search-base? (g (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-tree/search-base? s c_i)
   (wf-goal/search-base? g () c_i)
   ------------------- "conj wf/search-base"
   (wf-tree/search-base? (s × g c_i) c)]
  [(wf-tree/search-base? s_1 c)
   (wf-tree/search-base? s_2 c)
   ------------------- "left disj wf/search-base"
   (wf-tree/search-base? (s_1 <-+ s_2) c)]
  [(wf-tree/search-base? s c)
   ------------------- "delay wf/search-base"
   (wf-tree/search-base? (delay s) c)])

(define-judgment-form
  search-base-seq-lang
  #:contract (wf-answer-stream/search-base? as c)
  #:mode (wf-answer-stream/search-base? I I)
  [------------------- "empty answer stream wf/search-base"
   (wf-answer-stream/search-base? (empty-stream) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/search-base"
   (wf-answer-stream/search-base? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/search-base? as_tail c)
   ------------------- "answer stream wf/search-base"
   (wf-answer-stream/search-base? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  search-base-seq-lang
  #:contract (wf-cfg/search-base? cfg)
  #:mode (wf-cfg/search-base? I)
  [(wf-tree/search-base? s ())
   (wf-answer-stream/search-base? as ())
   ----------------------- "cfg-wf/search-base"
   (wf-cfg/search-base? (s as))])
