#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         "./calls-arity.rkt"
         (rename-in "./search-base-wf.rkt"
                    [wf-promoted/search-base? wf-promoted/search-base/base])
         "./core-wf.rkt")

(provide wf-goal/search-base-calls?
         wf-work/search-base-calls?
         wf-resolved/search-base-calls?
         wf-search/search-base-calls?
         wf-promoted/search-base-calls?
         wf-frontier/search-base-calls?
         wf-rel-env/search-base-calls?
         wf-config/search-base-calls?)

(check-redundancy #t)

(define-metafunction
  search-base-calls-lang
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...))
   ,(relcall-arity-ok/host (term r_call)
                           (term (t ...))
                           (term ((r_1 d_1 g_1) ...)))])

(define-judgment-form
  search-base-calls-lang
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
  search-base-calls-lang
  #:contract (wf-resolved/search-base-calls? search c)
  #:mode (wf-resolved/search-base-calls? I I)
  [------------------- "empty frontier residual is wf/search-base-calls"
   (wf-resolved/search-base-calls? (empty-tree) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/search-base-calls"
   (wf-resolved/search-base-calls? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/search-base-calls? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/search-base-calls"
   (wf-resolved/search-base-calls? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-work/search-base-calls? search Γ c)
  #:mode (wf-work/search-base-calls? I I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/search-base-calls? search_tail Γ c_2)
   ------------------- "work tree-freshened scope wf/search-base-calls"
   (wf-work/search-base-calls? (FreshenedTree c_1 search_tail tag_1) Γ c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/search-base-calls? g Γ () c_i)
   ------------------- "goal/state wf/search-base-calls"
   (wf-work/search-base-calls? (g (state sub dis c_i trail tag)) Γ c)]
  [(lvars-same-members? c c_i)
   (wf-search/search-base-calls? search_i Γ c_i)
   (wf-goal/search-base-calls? g Γ () c_i)
   ------------------- "conj wf/search-base-calls"
   (wf-work/search-base-calls? (search_i × g c_i) Γ c)]
  [(wf-search/search-base-calls? search_1 Γ c)
   (wf-search/search-base-calls? search_2 Γ c)
   ------------------- "disj wf/search-base-calls"
   (wf-work/search-base-calls? (search_1 <-+ search_2) Γ c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-search/search-base-calls? search Γ c)
  #:mode (wf-search/search-base-calls? I I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/search-base-calls? search_tail Γ c_2)
   ------------------- "search tree-freshened scope wf/search-base-calls"
   (wf-search/search-base-calls? (FreshenedTree c_1 search_tail tag_1) Γ c)]
  [(wf-resolved/search-base-calls? search_i c)
   ------------------- "resolved search wf/search-base-calls"
   (wf-search/search-base-calls? search_i Γ c)]
  [(wf-work/search-base-calls? search_i Γ c)
   ------------------- "work search wf/search-base-calls"
   (wf-search/search-base-calls? search_i Γ c)]
  [(wf-work/search-base-calls? search_i Γ c)
   ------------------- "delay search wf/search-base-calls"
   (wf-search/search-base-calls? (delay search_i) Γ c)])

(define-extended-judgment-form
  search-base-calls-lang
  wf-promoted/search-base/base
  #:contract (wf-promoted/search-base-calls? promoted c)
  #:mode (wf-promoted/search-base-calls? I I))

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-frontier/search-base-calls? cfg Γ c)
  #:mode (wf-frontier/search-base-calls? I I I)
  [(wf-search/search-base-calls? search_i Γ c)
   ------------------- "search frontier wf/search-base-calls"
   (wf-frontier/search-base-calls? search_i Γ c)]
  [(wf-search/search-base-calls? search_i Γ c)
   ------------------- "bounced search frontier wf/search-base-calls"
   (wf-frontier/search-base-calls? (Bounced search_i) Γ c)]
  [(wf-promoted/search-base-calls? promoted_i c)
   (wf-frontier/search-base-calls? cfg_tail Γ c)
   ------------------- "promoted stream node wf/search-base-calls"
   (wf-frontier/search-base-calls? (promoted_i + cfg_tail) Γ c)]
  [(wf-frontier/search-base-calls? cfg_tail Γ c)
   ------------------- "bounced frontier wf/search-base-calls"
   (wf-frontier/search-base-calls? (Bounced cfg_tail) Γ c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/search-base-calls? cfg_tail Γ c_2)
   ------------------- "cfg shell-freshened scope wf/search-base-calls"
   (wf-frontier/search-base-calls? (FreshenedShell c_1 cfg_tail tag_1) Γ c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-rel-env/search-base-calls? Γ)
  #:mode (wf-rel-env/search-base-calls? I)
  [(wf-goal/search-base-calls? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/search-base-calls"
   (wf-rel-env/search-base-calls? ((r d g) ...))])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-config/search-base-calls? config)
  #:mode (wf-config/search-base-calls? I)
  [(wf-rel-env/search-base-calls? Γ)
   (wf-frontier/search-base-calls? cfg Γ ())
   ----------------------- "program-wf/search-base-calls"
   (wf-config/search-base-calls? (Γ cfg))])
