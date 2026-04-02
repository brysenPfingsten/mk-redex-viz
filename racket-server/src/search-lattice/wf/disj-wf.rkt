#lang racket

(require redex/reduction-semantics
         "../languages/disj-seq-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/disj?
         wf-frontier/disj?
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
  #:contract (wf-frontier/disj? cfg c)
  #:mode (wf-frontier/disj? I I)
  [------------------- "empty frontier residual is wf/disj"
   (wf-frontier/disj? (empty-tree) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "raw answer/state wf/disj"
   (wf-frontier/disj? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-frontier/disj? cfg_tail c)
   ------------------- "observable answer prefix wf/disj"
   (wf-frontier/disj? ((⊤ (state sub dis c_i trail tag)) + cfg_tail) c)]
  [(wf-frontier/disj? cfg_tail c)
   ------------------- "bounced prefix wf/disj"
   (wf-frontier/disj? (Bounced + cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/disj? cfg_tail c_2)
   ------------------- "freshened scope wf/disj"
   (wf-frontier/disj? (Freshened c_1 tag_1 cfg_tail) c)]
  [(lvars-same-members? c c_i)
   (wf-goal/disj? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/disj"
   (wf-frontier/disj? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-frontier/disj? f c_i)
   (wf-goal/disj? g () c_i)
   ------------------- "conj wf/disj"
   (wf-frontier/disj? (f × g c_i) c)]
  [(wf-frontier/disj? f_1 c)
   (wf-frontier/disj? f_2 c)
   ------------------- "left disj wf/disj"
   (wf-frontier/disj? (f_1 <-+ f_2) c)])

(define-judgment-form
  disj-seq-lang
  #:contract (wf-cfg/disj? cfg)
  #:mode (wf-cfg/disj? I)
  [(wf-frontier/disj? cfg ())
   ----------------------- "cfg-wf/disj"
   (wf-cfg/disj? cfg)])
