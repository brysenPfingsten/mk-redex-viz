#lang racket

(require redex/reduction-semantics
         "../languages/calls-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         "./calls-arity.rkt"
         "./core-wf.rkt")

(provide wf-summary-goal/calls?
         wf-summary-work/calls?
         wf-summary-resolved/calls?
         wf-summary-search/calls?
         wf-summary-frontier/calls?
         wf-summary-rel-env/calls?
         wf-summary-config/calls?
         wf-goal/calls?
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
  #:contract (wf-summary-goal/calls? g Γ (x_1 ...) c summary)
  #:mode (wf-summary-goal/calls? I I I I O)
  [(where summary_1 (summary-zero))
   ------------------ "trivial success wf/calls"
   (wf-summary-goal/calls? (succeed tag) Γ (x_1 ...) c summary_1)]
  [(where summary_1 (summary-zero))
   ------------------ "trivial fail wf/calls"
   (wf-summary-goal/calls? (fail tag) Γ (x_1 ...) c summary_1)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-summary-goal/calls? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...) summary_1)
   ------------------- "fresh-wf/calls"
   (wf-summary-goal/calls? (∃ (x_1 ...) g tag) Γ (x_2 ...) c summary_1)]
  [(wf-summary-goal/calls? g_1 Γ (x_1 ...) c summary_1)
   (wf-summary-goal/calls? g_2 Γ (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj-wf/calls"
   (wf-summary-goal/calls? (g_1 ∧ g_2 tag) Γ (x_1 ...) c summary_3)]
  [(wf-summary-goal/calls? g Γ (x_1 ...) c summary_1)
   ------------------- "delay-goal-wf/calls"
   (wf-summary-goal/calls? (suspend g tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "==-wf/calls"
   (wf-summary-goal/calls? (t_1 =? t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "=/=-wf/calls"
   (wf-summary-goal/calls? (t_1 != t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   (where summary_1 (summary-zero))
   ------------------- "relcall-wf/calls"
   (wf-summary-goal/calls? (r t ... tag)
                           ((r_1 d_1 g_1) ...)
                           (x_1 ...)
                           c
                           summary_1)])

(define-judgment-form
  calls-lang
  #:contract (wf-summary-resolved/calls? search c summary)
  #:mode (wf-summary-resolved/calls? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/calls"
   (wf-summary-resolved/calls? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/calls"
   (wf-summary-resolved/calls? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/calls? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/calls"
   (wf-summary-resolved/calls? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  calls-lang
  #:contract (wf-summary-work/calls? search Γ c summary)
  #:mode (wf-summary-work/calls? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/calls? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/calls"
   (wf-summary-work/calls? (FreshenedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/calls? g Γ () c_i summary_1)
   ------------------- "goal/state wf/calls"
   (wf-summary-work/calls? (g (state sub dis c_i trail tag)) Γ c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/calls? search_i Γ c_i summary_1)
   (wf-summary-goal/calls? g Γ () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/calls"
   (wf-summary-work/calls? (search_i × g c_i) Γ c summary_3)])

(define-judgment-form
  calls-lang
  #:contract (wf-summary-search/calls? search Γ c summary)
  #:mode (wf-summary-search/calls? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/calls? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/calls"
   (wf-summary-search/calls? (FreshenedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-summary-resolved/calls? search_i c summary_1)
   ------------------- "resolved search wf/calls"
   (wf-summary-search/calls? search_i Γ c summary_1)]
  [(wf-summary-work/calls? search_i Γ c summary_1)
   ------------------- "work search wf/calls"
   (wf-summary-search/calls? search_i Γ c summary_1)]
  [(wf-summary-work/calls? search_i Γ c summary_1)
   ------------------- "delay search wf/calls"
   (wf-summary-search/calls? (delay search_i) Γ c summary_1)])

(define-judgment-form
  calls-lang
  #:contract (wf-summary-frontier/calls? cfg Γ c summary)
  #:mode (wf-summary-frontier/calls? I I I O)
  [(wf-summary-search/calls? search_i Γ c summary_1)
   ------------------- "search frontier wf/calls"
   (wf-summary-frontier/calls? search_i Γ c summary_1)]
  [(wf-summary-search/calls? search_i Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/calls"
   (wf-summary-frontier/calls? (Bounced search_i) Γ c summary_2)]
  [(wf-summary-frontier/calls? cfg_tail Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/calls"
   (wf-summary-frontier/calls? (Bounced cfg_tail) Γ c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/calls? cfg_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/calls"
   (wf-summary-frontier/calls? (FreshenedShell c_1 cfg_tail tag_1) Γ c summary_2)])

(define-judgment-form
  calls-lang
  #:contract (wf-summary-rel-env/calls? Γ summary)
  #:mode (wf-summary-rel-env/calls? I O)
  [(wf-summary-goal/calls? g ((r d g) ...) d () summary_1) ...
   (where summary_2 (summary-zero))
   ----------------------- "relation-env-wf/calls"
   (wf-summary-rel-env/calls? ((r d g) ...) summary_2)])

(define-judgment-form
  calls-lang
  #:contract (wf-summary-config/calls? config summary)
  #:mode (wf-summary-config/calls? I O)
  [(wf-summary-rel-env/calls? Γ summary_1)
   (wf-summary-frontier/calls? cfg Γ () summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ----------------------- "program-wf/calls"
   (wf-summary-config/calls? (Γ cfg) summary_3)])

(define-judgment-form
  calls-lang
  #:contract (wf-goal/calls? g Γ (x_1 ...) c)
  #:mode (wf-goal/calls? I I I I)
  [(wf-summary-goal/calls? g Γ (x_1 ...) c summary_1)
   ----------------------- "goal-wf/calls via summary"
   (wf-goal/calls? g Γ (x_1 ...) c)])

(define-judgment-form
  calls-lang
  #:contract (wf-work/calls? search Γ c)
  #:mode (wf-work/calls? I I I)
  [(wf-summary-work/calls? search Γ c summary_1)
   ----------------------- "work-wf/calls via summary"
   (wf-work/calls? search Γ c)])

(define-judgment-form
  calls-lang
  #:contract (wf-resolved/calls? search c)
  #:mode (wf-resolved/calls? I I)
  [(wf-summary-resolved/calls? search c summary_1)
   ----------------------- "resolved-wf/calls via summary"
   (wf-resolved/calls? search c)])

(define-judgment-form
  calls-lang
  #:contract (wf-search/calls? search Γ c)
  #:mode (wf-search/calls? I I I)
  [(wf-summary-search/calls? search Γ c summary_1)
   ----------------------- "search-wf/calls via summary"
   (wf-search/calls? search Γ c)])

(define-judgment-form
  calls-lang
  #:contract (wf-frontier/calls? cfg Γ c)
  #:mode (wf-frontier/calls? I I I)
  [(wf-summary-frontier/calls? cfg Γ c summary_1)
   ----------------------- "frontier-wf/calls via summary"
   (wf-frontier/calls? cfg Γ c)])

(define-judgment-form
  calls-lang
  #:contract (wf-rel-env/calls? Γ)
  #:mode (wf-rel-env/calls? I)
  [(wf-summary-rel-env/calls? Γ summary_1)
   ----------------------- "relation-env-wf/calls via summary"
   (wf-rel-env/calls? Γ)])

(define-judgment-form
  calls-lang
  #:contract (wf-config/calls? config)
  #:mode (wf-config/calls? I)
  [(wf-summary-config/calls? config summary_1)
   ----------------------- "program-wf/calls via summary"
   (wf-config/calls? config)])
