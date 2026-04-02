#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/delay?
         wf-frontier/delay?
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
  #:contract (wf-frontier/delay? cfg c)
  #:mode (wf-frontier/delay? I I)
  [------------------- "empty frontier residual is wf/delay"
   (wf-frontier/delay? (empty-tree) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "raw answer/state wf/delay"
   (wf-frontier/delay? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-frontier/delay? cfg_tail c)
   ------------------- "observable answer prefix wf/delay"
   (wf-frontier/delay? ((⊤ (state sub dis c_i trail tag)) + cfg_tail) c)]
  [(wf-frontier/delay? cfg_tail c)
   ------------------- "bounced prefix wf/delay"
   (wf-frontier/delay? (Bounced + cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/delay? cfg_tail c_2)
   ------------------- "freshened scope wf/delay"
   (wf-frontier/delay? (Freshened c_1 tag_1 cfg_tail) c)]
  [(lvars-same-members? c c_i)
   (wf-goal/delay? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/delay"
   (wf-frontier/delay? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-frontier/delay? f c_i)
   (wf-goal/delay? g () c_i)
   ------------------- "conj wf/delay"
   (wf-frontier/delay? (f × g c_i) c)]
  [(wf-frontier/delay? f c)
   ------------------- "delay wf/delay"
   (wf-frontier/delay? (delay f) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-cfg/delay? cfg)
  #:mode (wf-cfg/delay? I)
  [(wf-frontier/delay? cfg ())
   ----------------------- "cfg-wf/delay"
   (wf-cfg/delay? cfg)])
