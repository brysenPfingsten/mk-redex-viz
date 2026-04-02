#lang racket

(require redex/reduction-semantics
         "../languages/rail-seq-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/rail?
         wf-frontier/rail?
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
  #:contract (wf-frontier/rail? cfg c)
  #:mode (wf-frontier/rail? I I)
  [------------------- "empty frontier residual is wf/rail"
   (wf-frontier/rail? (empty-tree) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "raw answer/state wf/rail"
   (wf-frontier/rail? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-frontier/rail? cfg_tail c)
   ------------------- "observable answer prefix wf/rail"
   (wf-frontier/rail? ((⊤ (state sub dis c_i trail tag)) + cfg_tail) c)]
  [(wf-frontier/rail? cfg_tail c)
   ------------------- "bounced prefix wf/rail"
   (wf-frontier/rail? (Bounced + cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/rail? cfg_tail c_2)
   ------------------- "freshened scope wf/rail"
   (wf-frontier/rail? (Freshened c_1 tag_1 cfg_tail) c)]
  [(lvars-same-members? c c_i)
   (wf-goal/rail? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/rail"
   (wf-frontier/rail? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-frontier/rail? f c_i)
   (wf-goal/rail? g () c_i)
   ------------------- "conj wf/rail"
   (wf-frontier/rail? (f × g c_i) c)]
  [(wf-frontier/rail? f_1 c)
   (wf-frontier/rail? f_2 c)
   ------------------- "left disj wf/rail"
   (wf-frontier/rail? (f_1 <-+ f_2) c)]
  [(wf-frontier/rail? f_1 c)
   (wf-frontier/rail? f_2 c)
   ------------------- "right disj wf/rail"
   (wf-frontier/rail? (f_1 +-> f_2) c)]
  [(wf-frontier/rail? f c)
   ------------------- "delay wf/rail"
   (wf-frontier/rail? (delay f) c)])

(define-judgment-form
  rail-seq-lang
  #:contract (wf-cfg/rail? cfg)
  #:mode (wf-cfg/rail? I)
  [(wf-frontier/rail? cfg ())
   ----------------------- "cfg-wf/rail"
   (wf-cfg/rail? cfg)])
