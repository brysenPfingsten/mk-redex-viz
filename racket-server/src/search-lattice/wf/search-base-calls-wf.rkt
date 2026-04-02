#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         "./calls-arity.rkt"
         (rename-in "./search-base-wf.rkt"
                    [wf-summary-promoted/search-base? wf-summary-promoted/search-base/base])
         "./core-wf.rkt")

(provide wf-summary-goal/search-base-calls?
         wf-summary-work/search-base-calls?
         wf-summary-resolved/search-base-calls?
         wf-summary-search/search-base-calls?
         wf-summary-promoted/search-base-calls?
         wf-summary-frontier/search-base-calls?
         wf-summary-rel-env/search-base-calls?
         wf-summary-config/search-base-calls?
         wf-goal/search-base-calls?
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
  #:contract (wf-summary-goal/search-base-calls? g Γ (x_1 ...) c summary)
  #:mode (wf-summary-goal/search-base-calls? I I I I O)
  [(where summary_1 (summary-zero))
   ------------------ "trivial success wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (succeed tag) Γ (x_1 ...) c summary_1)]
  [(where summary_1 (summary-zero))
   ------------------ "trivial fail wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (fail tag) Γ (x_1 ...) c summary_1)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-summary-goal/search-base-calls? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...) summary_1)
   ------------------- "fresh-wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (∃ (x_1 ...) g tag) Γ (x_2 ...) c summary_1)]
  [(wf-summary-goal/search-base-calls? g_1 Γ (x_1 ...) c summary_1)
   (wf-summary-goal/search-base-calls? g_2 Γ (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj-wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (g_1 ∧ g_2 tag) Γ (x_1 ...) c summary_3)]
  [(wf-summary-goal/search-base-calls? g_1 Γ (x_1 ...) c summary_1)
   (wf-summary-goal/search-base-calls? g_2 Γ (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "disj-wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (g_1 ∨ g_2 tag) Γ (x_1 ...) c summary_3)]
  [(wf-summary-goal/search-base-calls? g Γ (x_1 ...) c summary_1)
   ------------------- "delay-goal-wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (suspend g tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "==-wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (t_1 =? t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "=/=-wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (t_1 != t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   (where summary_1 (summary-zero))
   ------------------- "relcall-wf/search-base-calls"
   (wf-summary-goal/search-base-calls? (r t ... tag)
                                       ((r_1 d_1 g_1) ...)
                                       (x_1 ...)
                                       c
                                       summary_1)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-summary-resolved/search-base-calls? search c summary)
  #:mode (wf-summary-resolved/search-base-calls? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/search-base-calls"
   (wf-summary-resolved/search-base-calls? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/search-base-calls"
   (wf-summary-resolved/search-base-calls? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/search-base-calls? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/search-base-calls"
   (wf-summary-resolved/search-base-calls? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-summary-work/search-base-calls? search Γ c summary)
  #:mode (wf-summary-work/search-base-calls? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/search-base-calls? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/search-base-calls"
   (wf-summary-work/search-base-calls? (FreshenedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/search-base-calls? g Γ () c_i summary_1)
   ------------------- "goal/state wf/search-base-calls"
   (wf-summary-work/search-base-calls? (g (state sub dis c_i trail tag)) Γ c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/search-base-calls? search_i Γ c_i summary_1)
   (wf-summary-goal/search-base-calls? g Γ () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/search-base-calls"
   (wf-summary-work/search-base-calls? (search_i × g c_i) Γ c summary_3)]
  [(wf-summary-search/search-base-calls? search_1 Γ c summary_1)
   (wf-summary-search/search-base-calls? search_2 Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "disj wf/search-base-calls"
   (wf-summary-work/search-base-calls? (search_1 <-+ search_2) Γ c summary_3)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-summary-search/search-base-calls? search Γ c summary)
  #:mode (wf-summary-search/search-base-calls? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/search-base-calls? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/search-base-calls"
   (wf-summary-search/search-base-calls? (FreshenedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-summary-resolved/search-base-calls? search_i c summary_1)
   ------------------- "resolved search wf/search-base-calls"
   (wf-summary-search/search-base-calls? search_i Γ c summary_1)]
  [(wf-summary-work/search-base-calls? search_i Γ c summary_1)
   ------------------- "work search wf/search-base-calls"
   (wf-summary-search/search-base-calls? search_i Γ c summary_1)]
  [(wf-summary-work/search-base-calls? search_i Γ c summary_1)
   ------------------- "delay search wf/search-base-calls"
   (wf-summary-search/search-base-calls? (delay search_i) Γ c summary_1)])

(define-extended-judgment-form
  search-base-calls-lang
  wf-summary-promoted/search-base/base
  #:contract (wf-summary-promoted/search-base-calls? promoted c summary)
  #:mode (wf-summary-promoted/search-base-calls? I I O))

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-summary-frontier/search-base-calls? cfg Γ c summary)
  #:mode (wf-summary-frontier/search-base-calls? I I I O)
  [(wf-summary-search/search-base-calls? search_i Γ c summary_1)
   ------------------- "search frontier wf/search-base-calls"
   (wf-summary-frontier/search-base-calls? search_i Γ c summary_1)]
  [(wf-summary-search/search-base-calls? search_i Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/search-base-calls"
   (wf-summary-frontier/search-base-calls? (Bounced search_i) Γ c summary_2)]
  [(wf-summary-promoted/search-base-calls? promoted_i c summary_1)
   (wf-summary-frontier/search-base-calls? cfg_tail Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "promoted stream node wf/search-base-calls"
   (wf-summary-frontier/search-base-calls? (promoted_i + cfg_tail) Γ c summary_3)]
  [(wf-summary-frontier/search-base-calls? cfg_tail Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/search-base-calls"
   (wf-summary-frontier/search-base-calls? (Bounced cfg_tail) Γ c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/search-base-calls? cfg_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/search-base-calls"
   (wf-summary-frontier/search-base-calls? (FreshenedShell c_1 cfg_tail tag_1) Γ c summary_2)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-summary-rel-env/search-base-calls? Γ summary)
  #:mode (wf-summary-rel-env/search-base-calls? I O)
  [(wf-summary-goal/search-base-calls? g ((r d g) ...) d () summary_1) ...
   (where summary_2 (summary-zero))
   ----------------------- "relation-env-wf/search-base-calls"
   (wf-summary-rel-env/search-base-calls? ((r d g) ...) summary_2)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-summary-config/search-base-calls? config summary)
  #:mode (wf-summary-config/search-base-calls? I O)
  [(wf-summary-rel-env/search-base-calls? Γ summary_1)
   (wf-summary-frontier/search-base-calls? cfg Γ () summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ----------------------- "program-wf/search-base-calls"
   (wf-summary-config/search-base-calls? (Γ cfg) summary_3)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-goal/search-base-calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/search-base-calls? I I I I)
  [(wf-summary-goal/search-base-calls? g Γ (x_1 ...) c summary_1)
   ----------------------- "goal-wf/search-base-calls via summary"
   (wf-goal/search-base-calls? g Γ (x_1 ...) c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-work/search-base-calls? search Γ c)
  #:mode (wf-work/search-base-calls? I I I)
  [(wf-summary-work/search-base-calls? search Γ c summary_1)
   ----------------------- "work-wf/search-base-calls via summary"
   (wf-work/search-base-calls? search Γ c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-resolved/search-base-calls? search c)
  #:mode (wf-resolved/search-base-calls? I I)
  [(wf-summary-resolved/search-base-calls? search c summary_1)
   ----------------------- "resolved-wf/search-base-calls via summary"
   (wf-resolved/search-base-calls? search c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-search/search-base-calls? search Γ c)
  #:mode (wf-search/search-base-calls? I I I)
  [(wf-summary-search/search-base-calls? search Γ c summary_1)
   ----------------------- "search-wf/search-base-calls via summary"
   (wf-search/search-base-calls? search Γ c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-promoted/search-base-calls? promoted c)
  #:mode (wf-promoted/search-base-calls? I I)
  [(wf-summary-promoted/search-base-calls? promoted c summary_1)
   ----------------------- "promoted-wf/search-base-calls via summary"
   (wf-promoted/search-base-calls? promoted c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-frontier/search-base-calls? cfg Γ c)
  #:mode (wf-frontier/search-base-calls? I I I)
  [(wf-summary-frontier/search-base-calls? cfg Γ c summary_1)
   ----------------------- "frontier-wf/search-base-calls via summary"
   (wf-frontier/search-base-calls? cfg Γ c)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-rel-env/search-base-calls? Γ)
  #:mode (wf-rel-env/search-base-calls? I)
  [(wf-summary-rel-env/search-base-calls? Γ summary_1)
   ----------------------- "relation-env-wf/search-base-calls via summary"
   (wf-rel-env/search-base-calls? Γ)])

(define-judgment-form
  search-base-calls-lang
  #:contract (wf-config/search-base-calls? config)
  #:mode (wf-config/search-base-calls? I)
  [(wf-summary-config/search-base-calls? config summary_1)
   ----------------------- "program-wf/search-base-calls via summary"
   (wf-config/search-base-calls? config)])
