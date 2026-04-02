#lang racket

(require redex/reduction-semantics
         "../languages/calls-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         "./calls-arity.rkt"
         "./core-wf.rkt")

(provide wf-goal/calls?
         wf-work/calls?
         wf-resolved/calls?
         wf-search/calls?
         wf-frontier/calls?
         wf-rel-env/calls?
         wf-config/calls?)

(check-redundancy #t)

(define-metafunction
  calls-lang
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...))
   ,(relcall-arity-ok/host (term r_call)
                           (term (t ...))
                           (term ((r_1 d_1 g_1) ...)))])

(define-judgment-form
  calls-lang
  #:contract (wf-goal/calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/calls? I I I I)
  [------------------ "trivial success wf/calls"
   (wf-goal/calls? (succeed tag) Γ (x_1 ...) c)]
  [------------------ "trivial fail wf/calls"
   (wf-goal/calls? (fail tag) Γ (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/calls? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/calls"
   (wf-goal/calls? (∃ (x_1 ...) g tag) Γ (x_2 ...) c)]
  [(wf-goal/calls? g_1 Γ (x_1 ...) c)
   (wf-goal/calls? g_2 Γ (x_1 ...) c)
   ------------------- "conj-wf/calls"
   (wf-goal/calls? (g_1 ∧ g_2 tag) Γ (x_1 ...) c)]
  [(wf-goal/calls? g Γ (x_1 ...) c)
   ------------------- "delay-goal-wf/calls"
   (wf-goal/calls? (suspend g tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/calls"
   (wf-goal/calls? (t_1 =? t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/calls"
   (wf-goal/calls? (t_1 != t_2 tag) Γ (x_1 ...) c)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   ------------------- "relcall-wf/calls"
   (wf-goal/calls? (r t ... tag)
                   ((r_1 d_1 g_1) ...)
                   (x_1 ...)
                   c)])

(define-judgment-form
  calls-lang
  #:contract (wf-resolved/calls? search c)
  #:mode (wf-resolved/calls? I I)
  [------------------- "empty frontier residual is wf/calls"
   (wf-resolved/calls? (empty-tree) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/calls"
   (wf-resolved/calls? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/calls? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/calls"
   (wf-resolved/calls? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  calls-lang
  #:contract (wf-work/calls? search Γ c)
  #:mode (wf-work/calls? I I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/calls? search_tail Γ c_2)
   ------------------- "work tree-freshened scope wf/calls"
   (wf-work/calls? (FreshenedTree c_1 search_tail tag_1) Γ c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/calls? g Γ () c_i)
   ------------------- "goal/state wf/calls"
   (wf-work/calls? (g (state sub dis c_i trail tag)) Γ c)]
  [(lvars-same-members? c c_i)
   (wf-search/calls? search_i Γ c_i)
   (wf-goal/calls? g Γ () c_i)
   ------------------- "conj wf/calls"
   (wf-work/calls? (search_i × g c_i) Γ c)])

(define-judgment-form
  calls-lang
  #:contract (wf-search/calls? search Γ c)
  #:mode (wf-search/calls? I I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/calls? search_tail Γ c_2)
   ------------------- "search tree-freshened scope wf/calls"
   (wf-search/calls? (FreshenedTree c_1 search_tail tag_1) Γ c)]
  [(wf-resolved/calls? search_i c)
   ------------------- "resolved search wf/calls"
   (wf-search/calls? search_i Γ c)]
  [(wf-work/calls? search_i Γ c)
   ------------------- "work search wf/calls"
   (wf-search/calls? search_i Γ c)]
  [(wf-work/calls? search_i Γ c)
   ------------------- "delay search wf/calls"
   (wf-search/calls? (delay search_i) Γ c)])

(define-judgment-form
  calls-lang
  #:contract (wf-frontier/calls? cfg Γ c)
  #:mode (wf-frontier/calls? I I I)
  [(wf-search/calls? search_i Γ c)
   ------------------- "search frontier wf/calls"
   (wf-frontier/calls? search_i Γ c)]
  [(wf-search/calls? search_i Γ c)
   ------------------- "bounced search frontier wf/calls"
   (wf-frontier/calls? (Bounced search_i) Γ c)]
  [(wf-frontier/calls? cfg_tail Γ c)
   ------------------- "bounced frontier wf/calls"
   (wf-frontier/calls? (Bounced cfg_tail) Γ c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/calls? cfg_tail Γ c_2)
   ------------------- "cfg shell-freshened scope wf/calls"
   (wf-frontier/calls? (FreshenedShell c_1 cfg_tail tag_1) Γ c)])

(define-judgment-form
  calls-lang
  #:contract (wf-rel-env/calls? Γ)
  #:mode (wf-rel-env/calls? I)
  [(wf-goal/calls? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/calls"
   (wf-rel-env/calls? ((r d g) ...))])

(define-judgment-form
  calls-lang
  #:contract (wf-config/calls? config)
  #:mode (wf-config/calls? I)
  [(wf-rel-env/calls? Γ)
   (wf-frontier/calls? cfg Γ ())
   ----------------------- "program-wf/calls"
   (wf-config/calls? (Γ cfg))])
