#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./search-base-calls-wf.rkt"
                    [wf-goal/search-base-calls? wf-goal/search-base-calls/base]
                    [wf-promoted/search-base-calls? wf-promoted/search-base-calls/base])
         "./core-wf.rkt")

(provide wf-goal/rail-calls?
         wf-work/rail-calls?
         wf-resolved/rail-calls?
         wf-search/rail-calls?
         wf-promoted/rail-calls?
         wf-frontier/rail-calls?
         wf-rel-env/rail-calls?
         wf-config/rail-calls?)

(check-redundancy #t)

(define-extended-judgment-form
  rail-calls-lang
  wf-goal/search-base-calls/base
  #:contract (wf-goal/rail-calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/rail-calls? I I I I))

(define-judgment-form
  rail-calls-lang
  #:contract (wf-resolved/rail-calls? search c)
  #:mode (wf-resolved/rail-calls? I I)
  [------------------- "empty frontier residual is wf/rail-calls"
   (wf-resolved/rail-calls? (empty-tree) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/rail-calls"
   (wf-resolved/rail-calls? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/rail-calls? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/rail-calls"
   (wf-resolved/rail-calls? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-work/rail-calls? search Γ c)
  #:mode (wf-work/rail-calls? I I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/rail-calls? search_tail Γ c_2)
   ------------------- "work tree-freshened scope wf/rail-calls"
   (wf-work/rail-calls? (FreshenedTree c_1 search_tail tag_1) Γ c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/rail-calls? g Γ () c_i)
   ------------------- "goal/state wf/rail-calls"
   (wf-work/rail-calls? (g (state sub dis c_i trail tag)) Γ c)]
  [(lvars-same-members? c c_i)
   (wf-search/rail-calls? search_i Γ c_i)
   (wf-goal/rail-calls? g Γ () c_i)
   ------------------- "conj wf/rail-calls"
   (wf-work/rail-calls? (search_i × g c_i) Γ c)]
  [(wf-search/rail-calls? search_1 Γ c)
   (wf-search/rail-calls? search_2 Γ c)
   ------------------- "left disj wf/rail-calls"
   (wf-work/rail-calls? (search_1 <-+ search_2) Γ c)]
  [(wf-search/rail-calls? search_1 Γ c)
   (wf-search/rail-calls? search_2 Γ c)
   ------------------- "right disj wf/rail-calls"
   (wf-work/rail-calls? (search_1 +-> search_2) Γ c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-search/rail-calls? search Γ c)
  #:mode (wf-search/rail-calls? I I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/rail-calls? search_tail Γ c_2)
   ------------------- "search tree-freshened scope wf/rail-calls"
   (wf-search/rail-calls? (FreshenedTree c_1 search_tail tag_1) Γ c)]
  [(wf-resolved/rail-calls? search_i c)
   ------------------- "resolved search wf/rail-calls"
   (wf-search/rail-calls? search_i Γ c)]
  [(wf-work/rail-calls? search_i Γ c)
   ------------------- "work search wf/rail-calls"
   (wf-search/rail-calls? search_i Γ c)]
  [(wf-work/rail-calls? search_i Γ c)
   ------------------- "delay search wf/rail-calls"
   (wf-search/rail-calls? (delay search_i) Γ c)])

(define-extended-judgment-form
  rail-calls-lang
  wf-promoted/search-base-calls/base
  #:contract (wf-promoted/rail-calls? promoted c)
  #:mode (wf-promoted/rail-calls? I I))

(define-judgment-form
  rail-calls-lang
  #:contract (wf-frontier/rail-calls? cfg Γ c)
  #:mode (wf-frontier/rail-calls? I I I)
  [(wf-search/rail-calls? search_i Γ c)
   ------------------- "search frontier wf/rail-calls"
   (wf-frontier/rail-calls? search_i Γ c)]
  [(wf-search/rail-calls? search_i Γ c)
   ------------------- "bounced search frontier wf/rail-calls"
   (wf-frontier/rail-calls? (Bounced search_i) Γ c)]
  [(wf-promoted/rail-calls? promoted_i c)
   (wf-frontier/rail-calls? cfg_tail Γ c)
   ------------------- "promoted stream node wf/rail-calls"
   (wf-frontier/rail-calls? (promoted_i + cfg_tail) Γ c)]
  [(wf-frontier/rail-calls? cfg_tail Γ c)
   ------------------- "bounced frontier wf/rail-calls"
   (wf-frontier/rail-calls? (Bounced cfg_tail) Γ c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/rail-calls? cfg_tail Γ c_2)
   ------------------- "cfg shell-freshened scope wf/rail-calls"
   (wf-frontier/rail-calls? (FreshenedShell c_1 cfg_tail tag_1) Γ c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-rel-env/rail-calls? Γ)
  #:mode (wf-rel-env/rail-calls? I)
  [(wf-goal/rail-calls? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/rail-calls"
   (wf-rel-env/rail-calls? ((r d g) ...))])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-config/rail-calls? config)
  #:mode (wf-config/rail-calls? I)
  [(wf-rel-env/rail-calls? Γ)
   (wf-frontier/rail-calls? cfg Γ ())
   ----------------------- "program-wf/rail-calls"
   (wf-config/rail-calls? (Γ cfg))])
