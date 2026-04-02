#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./search-base-calls-wf.rkt"
                    [wf-summary-goal/search-base-calls? wf-summary-goal/search-base-calls/base]
                    [wf-summary-promoted/search-base-calls? wf-summary-promoted/search-base-calls/base])
         "./core-wf.rkt")

(provide wf-summary-goal/rail-calls?
         wf-summary-work/rail-calls?
         wf-summary-resolved/rail-calls?
         wf-summary-search/rail-calls?
         wf-summary-promoted/rail-calls?
         wf-summary-frontier/rail-calls?
         wf-summary-rel-env/rail-calls?
         wf-summary-config/rail-calls?
         wf-goal/rail-calls?
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
  wf-summary-goal/search-base-calls/base
  #:contract (wf-summary-goal/rail-calls? g Γ (x_1 ...) c summary)
  #:mode (wf-summary-goal/rail-calls? I I I I O))

(define-judgment-form
  rail-calls-lang
  #:contract (wf-summary-resolved/rail-calls? search c summary)
  #:mode (wf-summary-resolved/rail-calls? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/rail-calls"
   (wf-summary-resolved/rail-calls? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/rail-calls"
   (wf-summary-resolved/rail-calls? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/rail-calls? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/rail-calls"
   (wf-summary-resolved/rail-calls? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-summary-work/rail-calls? search Γ c summary)
  #:mode (wf-summary-work/rail-calls? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/rail-calls? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/rail-calls"
   (wf-summary-work/rail-calls? (FreshenedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/rail-calls? g Γ () c_i summary_1)
   ------------------- "goal/state wf/rail-calls"
   (wf-summary-work/rail-calls? (g (state sub dis c_i trail tag)) Γ c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/rail-calls? search_i Γ c_i summary_1)
   (wf-summary-goal/rail-calls? g Γ () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/rail-calls"
   (wf-summary-work/rail-calls? (search_i × g c_i) Γ c summary_3)]
  [(wf-summary-search/rail-calls? search_1 Γ c summary_1)
   (wf-summary-search/rail-calls? search_2 Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "left disj wf/rail-calls"
   (wf-summary-work/rail-calls? (search_1 <-+ search_2) Γ c summary_3)]
  [(wf-summary-search/rail-calls? search_1 Γ c summary_1)
   (wf-summary-search/rail-calls? search_2 Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "right disj wf/rail-calls"
   (wf-summary-work/rail-calls? (search_1 +-> search_2) Γ c summary_3)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-summary-search/rail-calls? search Γ c summary)
  #:mode (wf-summary-search/rail-calls? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/rail-calls? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/rail-calls"
   (wf-summary-search/rail-calls? (FreshenedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-summary-resolved/rail-calls? search_i c summary_1)
   ------------------- "resolved search wf/rail-calls"
   (wf-summary-search/rail-calls? search_i Γ c summary_1)]
  [(wf-summary-work/rail-calls? search_i Γ c summary_1)
   ------------------- "work search wf/rail-calls"
   (wf-summary-search/rail-calls? search_i Γ c summary_1)]
  [(wf-summary-work/rail-calls? search_i Γ c summary_1)
   ------------------- "delay search wf/rail-calls"
   (wf-summary-search/rail-calls? (delay search_i) Γ c summary_1)])

(define-extended-judgment-form
  rail-calls-lang
  wf-summary-promoted/search-base-calls/base
  #:contract (wf-summary-promoted/rail-calls? promoted c summary)
  #:mode (wf-summary-promoted/rail-calls? I I O))

(define-judgment-form
  rail-calls-lang
  #:contract (wf-summary-frontier/rail-calls? cfg Γ c summary)
  #:mode (wf-summary-frontier/rail-calls? I I I O)
  [(wf-summary-search/rail-calls? search_i Γ c summary_1)
   ------------------- "search frontier wf/rail-calls"
   (wf-summary-frontier/rail-calls? search_i Γ c summary_1)]
  [(wf-summary-search/rail-calls? search_i Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/rail-calls"
   (wf-summary-frontier/rail-calls? (Bounced search_i) Γ c summary_2)]
  [(wf-summary-promoted/rail-calls? promoted_i c summary_1)
   (wf-summary-frontier/rail-calls? cfg_tail Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "promoted stream node wf/rail-calls"
   (wf-summary-frontier/rail-calls? (promoted_i + cfg_tail) Γ c summary_3)]
  [(wf-summary-frontier/rail-calls? cfg_tail Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/rail-calls"
   (wf-summary-frontier/rail-calls? (Bounced cfg_tail) Γ c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/rail-calls? cfg_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/rail-calls"
   (wf-summary-frontier/rail-calls? (FreshenedShell c_1 cfg_tail tag_1) Γ c summary_2)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-summary-rel-env/rail-calls? Γ summary)
  #:mode (wf-summary-rel-env/rail-calls? I O)
  [(wf-summary-goal/rail-calls? g ((r d g) ...) d () summary_1) ...
   (where summary_2 (summary-zero))
   ----------------------- "relation-env-wf/rail-calls"
   (wf-summary-rel-env/rail-calls? ((r d g) ...) summary_2)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-summary-config/rail-calls? config summary)
  #:mode (wf-summary-config/rail-calls? I O)
  [(wf-summary-rel-env/rail-calls? Γ summary_1)
   (wf-summary-frontier/rail-calls? cfg Γ () summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ----------------------- "program-wf/rail-calls"
   (wf-summary-config/rail-calls? (Γ cfg) summary_3)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-goal/rail-calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/rail-calls? I I I I)
  [(wf-summary-goal/rail-calls? g Γ (x_1 ...) c summary_1)
   ----------------------- "goal-wf/rail-calls via summary"
   (wf-goal/rail-calls? g Γ (x_1 ...) c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-work/rail-calls? search Γ c)
  #:mode (wf-work/rail-calls? I I I)
  [(wf-summary-work/rail-calls? search Γ c summary_1)
   ----------------------- "work-wf/rail-calls via summary"
   (wf-work/rail-calls? search Γ c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-resolved/rail-calls? search c)
  #:mode (wf-resolved/rail-calls? I I)
  [(wf-summary-resolved/rail-calls? search c summary_1)
   ----------------------- "resolved-wf/rail-calls via summary"
   (wf-resolved/rail-calls? search c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-search/rail-calls? search Γ c)
  #:mode (wf-search/rail-calls? I I I)
  [(wf-summary-search/rail-calls? search Γ c summary_1)
   ----------------------- "search-wf/rail-calls via summary"
   (wf-search/rail-calls? search Γ c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-promoted/rail-calls? promoted c)
  #:mode (wf-promoted/rail-calls? I I)
  [(wf-summary-promoted/rail-calls? promoted c summary_1)
   ----------------------- "promoted-wf/rail-calls via summary"
   (wf-promoted/rail-calls? promoted c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-frontier/rail-calls? cfg Γ c)
  #:mode (wf-frontier/rail-calls? I I I)
  [(wf-summary-frontier/rail-calls? cfg Γ c summary_1)
   ----------------------- "frontier-wf/rail-calls via summary"
   (wf-frontier/rail-calls? cfg Γ c)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-rel-env/rail-calls? Γ)
  #:mode (wf-rel-env/rail-calls? I)
  [(wf-summary-rel-env/rail-calls? Γ summary_1)
   ----------------------- "relation-env-wf/rail-calls via summary"
   (wf-rel-env/rail-calls? Γ)])

(define-judgment-form
  rail-calls-lang
  #:contract (wf-config/rail-calls? config)
  #:mode (wf-config/rail-calls? I)
  [(wf-summary-config/rail-calls? config summary_1)
   ----------------------- "program-wf/rail-calls via summary"
   (wf-config/rail-calls? config)])
