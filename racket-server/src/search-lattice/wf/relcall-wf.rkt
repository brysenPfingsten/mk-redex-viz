#lang racket

(require redex/reduction-semantics
         "../languages/relcall-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         "./relcall-arity.rkt"
         "./core-wf.rkt")

(provide wf-summary-goal/relcall?
         wf-summary-work/relcall?
         wf-summary-resolved/relcall?
         wf-summary-search/relcall?
         wf-summary-frontier/relcall?
         wf-summary-rel-env/relcall?
         wf-summary-config/relcall?
         wf-goal/relcall?
         wf-work/relcall?
         wf-resolved/relcall?
         wf-search/relcall?
         wf-frontier/relcall?
         wf-rel-env/relcall?
         wf-config/relcall?)

(check-redundancy #t)

(define-metafunction
  relcall-lang
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...))
   ,(relcall-arity-ok/host (term r_call)
                           (term (t ...))
                           (term ((r_1 d_1 g_1) ...)))])

(define-judgment-form
  relcall-lang
  #:contract (wf-summary-goal/relcall? g Γ (x_1 ...) c summary)
  #:mode (wf-summary-goal/relcall? I I I I O)
  [(where summary_1 (summary-zero))
   ------------------ "trivial success wf/relcall"
   (wf-summary-goal/relcall? (succeed tag) Γ (x_1 ...) c summary_1)]
  [(where summary_1 (summary-zero))
   ------------------ "trivial fail wf/relcall"
   (wf-summary-goal/relcall? (fail tag) Γ (x_1 ...) c summary_1)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-summary-goal/relcall? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...) summary_1)
   ------------------- "fresh-wf/relcall"
   (wf-summary-goal/relcall? (∃ (x_1 ...) g tag) Γ (x_2 ...) c summary_1)]
  [(wf-summary-goal/relcall? g_1 Γ (x_1 ...) c summary_1)
   (wf-summary-goal/relcall? g_2 Γ (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj-wf/relcall"
   (wf-summary-goal/relcall? (g_1 ∧ g_2 tag) Γ (x_1 ...) c summary_3)]
  [(wf-summary-goal/relcall? g Γ (x_1 ...) c summary_1)
   ------------------- "delay-goal-wf/relcall"
   (wf-summary-goal/relcall? (suspend g tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "==-wf/relcall"
   (wf-summary-goal/relcall? (t_1 =? t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "=/=-wf/relcall"
   (wf-summary-goal/relcall? (t_1 != t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   (where summary_1 (summary-zero))
   ------------------- "relcall-wf/relcall"
   (wf-summary-goal/relcall? (r t ... tag)
                           ((r_1 d_1 g_1) ...)
                           (x_1 ...)
                           c
                           summary_1)])

(define-judgment-form
  relcall-lang
  #:contract (wf-summary-resolved/relcall? search c summary)
  #:mode (wf-summary-resolved/relcall? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/relcall"
   (wf-summary-resolved/relcall? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/relcall"
   (wf-summary-resolved/relcall? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/relcall? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/relcall"
   (wf-summary-resolved/relcall? (ScopedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  relcall-lang
  #:contract (wf-summary-work/relcall? search Γ c summary)
  #:mode (wf-summary-work/relcall? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/relcall? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/relcall"
   (wf-summary-work/relcall? (ScopedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/relcall? g Γ () c_i summary_1)
   ------------------- "goal/state wf/relcall"
   (wf-summary-work/relcall? (g (state sub dis c_i trail tag)) Γ c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/relcall? search_i Γ c_i summary_1)
   (wf-summary-goal/relcall? g Γ () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/relcall"
   (wf-summary-work/relcall? (search_i × g c_i) Γ c summary_3)])

(define-judgment-form
  relcall-lang
  #:contract (wf-summary-search/relcall? search Γ c summary)
  #:mode (wf-summary-search/relcall? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/relcall? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/relcall"
   (wf-summary-search/relcall? (ScopedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-summary-resolved/relcall? search_i c summary_1)
   ------------------- "resolved search wf/relcall"
   (wf-summary-search/relcall? search_i Γ c summary_1)]
  [(wf-summary-work/relcall? search_i Γ c summary_1)
   ------------------- "work search wf/relcall"
   (wf-summary-search/relcall? search_i Γ c summary_1)]
  [(wf-summary-work/relcall? search_i Γ c summary_1)
   ------------------- "delay search wf/relcall"
   (wf-summary-search/relcall? (delay search_i) Γ c summary_1)])

(define-judgment-form
  relcall-lang
  #:contract (wf-summary-frontier/relcall? cfg Γ c summary)
  #:mode (wf-summary-frontier/relcall? I I I O)
  [(wf-summary-search/relcall? search_i Γ c summary_1)
   ------------------- "search frontier wf/relcall"
   (wf-summary-frontier/relcall? search_i Γ c summary_1)]
  [(wf-summary-search/relcall? search_i Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/relcall"
   (wf-summary-frontier/relcall? (Deferred search_i) Γ c summary_2)]
  [(wf-summary-frontier/relcall? cfg_tail Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/relcall"
   (wf-summary-frontier/relcall? (Deferred cfg_tail) Γ c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/relcall? cfg_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/relcall"
   (wf-summary-frontier/relcall? (ScopedShell c_1 cfg_tail tag_1) Γ c summary_2)])

(define-judgment-form
  relcall-lang
  #:contract (wf-summary-rel-env/relcall? Γ summary)
  #:mode (wf-summary-rel-env/relcall? I O)
  [(wf-summary-goal/relcall? g ((r d g) ...) d () summary_1) ...
   (where summary_2 (summary-zero))
   ----------------------- "relation-env-wf/relcall"
   (wf-summary-rel-env/relcall? ((r d g) ...) summary_2)])

(define-judgment-form
  relcall-lang
  #:contract (wf-summary-config/relcall? config summary)
  #:mode (wf-summary-config/relcall? I O)
  [(wf-summary-rel-env/relcall? Γ summary_1)
   (wf-summary-frontier/relcall? cfg Γ () summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ----------------------- "program-wf/relcall"
   (wf-summary-config/relcall? (Γ cfg) summary_3)])

(define-judgment-form
  relcall-lang
  #:contract (wf-goal/relcall? g Γ (x_1 ...) c)
  #:mode (wf-goal/relcall? I I I I)
  [(wf-summary-goal/relcall? g Γ (x_1 ...) c summary_1)
   ----------------------- "goal-wf/relcall via summary"
   (wf-goal/relcall? g Γ (x_1 ...) c)])

(define-judgment-form
  relcall-lang
  #:contract (wf-work/relcall? search Γ c)
  #:mode (wf-work/relcall? I I I)
  [(wf-summary-work/relcall? search Γ c summary_1)
   ----------------------- "work-wf/relcall via summary"
   (wf-work/relcall? search Γ c)])

(define-judgment-form
  relcall-lang
  #:contract (wf-resolved/relcall? search c)
  #:mode (wf-resolved/relcall? I I)
  [(wf-summary-resolved/relcall? search c summary_1)
   ----------------------- "resolved-wf/relcall via summary"
   (wf-resolved/relcall? search c)])

(define-judgment-form
  relcall-lang
  #:contract (wf-search/relcall? search Γ c)
  #:mode (wf-search/relcall? I I I)
  [(wf-summary-search/relcall? search Γ c summary_1)
   ----------------------- "search-wf/relcall via summary"
   (wf-search/relcall? search Γ c)])

(define-judgment-form
  relcall-lang
  #:contract (wf-frontier/relcall? cfg Γ c)
  #:mode (wf-frontier/relcall? I I I)
  [(wf-summary-frontier/relcall? cfg Γ c summary_1)
   ----------------------- "frontier-wf/relcall via summary"
   (wf-frontier/relcall? cfg Γ c)])

(define-judgment-form
  relcall-lang
  #:contract (wf-rel-env/relcall? Γ)
  #:mode (wf-rel-env/relcall? I)
  [(wf-summary-rel-env/relcall? Γ summary_1)
   ----------------------- "relation-env-wf/relcall via summary"
   (wf-rel-env/relcall? Γ)])

(define-judgment-form
  relcall-lang
  #:contract (wf-config/relcall? config)
  #:mode (wf-config/relcall? I)
  [(wf-summary-config/relcall? config summary_1)
   ----------------------- "program-wf/relcall via summary"
   (wf-config/relcall? config)])
