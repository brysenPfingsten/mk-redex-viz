#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-calls-lang.rkt"
         "./calls-arity.rkt"
         "./core-wf.rkt")

(provide wf-goal/search-base-calls?
         wf-frontier/search-base-calls?
         wf-rel-env/search-base-calls?
         wf-config/search-base-calls?)

(check-redundancy #t)

(define-metafunction
  search-base-seq-calls-lang
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...))
   ,(relcall-arity-ok/host (term r_call)
                           (term (t ...))
                           (term ((r_1 d_1 g_1) ...)))])

(define-judgment-form
  search-base-seq-calls-lang
  #:contract (wf-goal/search-base-calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/search-base-calls? I I I I)
  [------------------ "trivial success wf/search-base-calls"
   (wf-goal/search-base-calls? (succeed tag) Γ (x_1 ...) c)]
  [------------------ "trivial fail wf/search-base-calls"
   (wf-goal/search-base-calls? (fail tag) Γ (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/search-base-calls? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/search-base-calls"
   (wf-goal/search-base-calls? (∃ (x_1 ...) g tag) Γ (x_2 ...) c)]
  [(wf-goal/search-base-calls? g_1 Γ (x_1 ...) c)
   (wf-goal/search-base-calls? g_2 Γ (x_1 ...) c)
   ------------------- "conj-wf/search-base-calls"
   (wf-goal/search-base-calls? (g_1 ∧ g_2 tag) Γ (x_1 ...) c)]
  [(wf-goal/search-base-calls? g_1 Γ (x_1 ...) c)
   (wf-goal/search-base-calls? g_2 Γ (x_1 ...) c)
   ------------------- "disj-wf/search-base-calls"
   (wf-goal/search-base-calls? (g_1 ∨ g_2 tag) Γ (x_1 ...) c)]
  [(wf-goal/search-base-calls? g Γ (x_1 ...) c)
   ------------------- "delay-goal-wf/search-base-calls"
   (wf-goal/search-base-calls? (suspend g tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/search-base-calls"
   (wf-goal/search-base-calls? (t_1 =? t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/search-base-calls"
   (wf-goal/search-base-calls? (t_1 != t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   ------------------- "relcall-wf/search-base-calls"
   (wf-goal/search-base-calls? (r t ... tag)
                               ((r_1 d_1 g_1) ...)
                               (x_1 ...)
                               c)])

(define-judgment-form
  search-base-seq-calls-lang
  #:contract (wf-frontier/search-base-calls? cfg Γ c)
  #:mode (wf-frontier/search-base-calls? I I I)
  [------------------- "empty frontier residual is wf/search-base-calls"
   (wf-frontier/search-base-calls? (empty-tree) Γ c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "raw answer/state wf/search-base-calls"
   (wf-frontier/search-base-calls? (⊤ (state sub dis c_i trail tag)) Γ c)]
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-frontier/search-base-calls? cfg_tail Γ c)
   ------------------- "observable answer prefix wf/search-base-calls"
   (wf-frontier/search-base-calls? ((⊤ (state sub dis c_i trail tag)) + cfg_tail) Γ c)]
  [(wf-frontier/search-base-calls? cfg_tail Γ c)
   ------------------- "bounced prefix wf/search-base-calls"
   (wf-frontier/search-base-calls? (Bounced + cfg_tail) Γ c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/search-base-calls? cfg_tail Γ c_2)
   ------------------- "freshened scope wf/search-base-calls"
   (wf-frontier/search-base-calls? (Freshened c_1 tag_1 cfg_tail) Γ c)]
  [(lvars-same-members? c c_i)
   (wf-goal/search-base-calls? g Γ () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/search-base-calls"
   (wf-frontier/search-base-calls? (g (state sub dis c_i trail tag)) Γ c)]
  [(lvars-same-members? c c_i)
   (wf-frontier/search-base-calls? f Γ c_i)
   (wf-goal/search-base-calls? g Γ () c_i)
   ------------------- "conj wf/search-base-calls"
   (wf-frontier/search-base-calls? (f × g c_i) Γ c)]
  [(wf-frontier/search-base-calls? f_1 Γ c)
   (wf-frontier/search-base-calls? f_2 Γ c)
   ------------------- "left disj wf/search-base-calls"
   (wf-frontier/search-base-calls? (f_1 <-+ f_2) Γ c)]
  [(wf-frontier/search-base-calls? f Γ c)
   ------------------- "delay wf/search-base-calls"
   (wf-frontier/search-base-calls? (delay f) Γ c)])

(define-judgment-form
  search-base-seq-calls-lang
  #:contract (wf-rel-env/search-base-calls? Γ)
  #:mode (wf-rel-env/search-base-calls? I)
  [(wf-goal/search-base-calls? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/search-base-calls"
   (wf-rel-env/search-base-calls? ((r d g) ...))])

(define-judgment-form
  search-base-seq-calls-lang
  #:contract (wf-config/search-base-calls? config)
  #:mode (wf-config/search-base-calls? I)
  [(wf-rel-env/search-base-calls? Γ)
   (wf-frontier/search-base-calls? cfg Γ ())
   ----------------------- "program-wf/search-base-calls"
   (wf-config/search-base-calls? (Γ cfg))])
