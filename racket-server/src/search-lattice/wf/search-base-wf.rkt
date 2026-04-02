#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-lang.rkt"
         "./core-wf.rkt")

(provide wf-goal/search-base?
         wf-frontier/search-base?
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
  #:contract (wf-frontier/search-base? cfg c)
  #:mode (wf-frontier/search-base? I I)
  [------------------- "empty frontier residual is wf/search-base"
   (wf-frontier/search-base? (empty-tree) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "raw answer/state wf/search-base"
   (wf-frontier/search-base? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-frontier/search-base? cfg_tail c)
   ------------------- "observable answer prefix wf/search-base"
   (wf-frontier/search-base? ((⊤ (state sub dis c_i trail tag)) + cfg_tail) c)]
  [(wf-frontier/search-base? cfg_tail c)
   ------------------- "bounced prefix wf/search-base"
   (wf-frontier/search-base? (Bounced + cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/search-base? cfg_tail c_2)
   ------------------- "freshened scope wf/search-base"
   (wf-frontier/search-base? (Freshened c_1 tag_1 cfg_tail) c)]
  [(lvars-same-members? c c_i)
   (wf-goal/search-base? g () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/search-base"
   (wf-frontier/search-base? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-frontier/search-base? f c_i)
   (wf-goal/search-base? g () c_i)
   ------------------- "conj wf/search-base"
   (wf-frontier/search-base? (f × g c_i) c)]
  [(wf-frontier/search-base? f_1 c)
   (wf-frontier/search-base? f_2 c)
   ------------------- "disj wf/search-base"
   (wf-frontier/search-base? (f_1 <-+ f_2) c)]
  [(wf-frontier/search-base? f c)
   ------------------- "delay wf/search-base"
   (wf-frontier/search-base? (delay f) c)])

(define-judgment-form
  search-base-seq-lang
  #:contract (wf-cfg/search-base? cfg)
  #:mode (wf-cfg/search-base? I)
  [(wf-frontier/search-base? cfg ())
   ----------------------- "cfg-wf/search-base"
   (wf-cfg/search-base? cfg)])
