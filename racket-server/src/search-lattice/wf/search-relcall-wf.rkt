#lang racket

(require redex/reduction-semantics
         "../languages/search-relcall-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         "./relcall-arity.rkt"
         (rename-in "./search-wf.rkt"
                    [wf-summary-answers/search? wf-summary-answers/search/base])
         "./core-wf.rkt")

(provide wf-summary-goal/search-relcall?
         wf-summary-work/search-relcall?
         wf-summary-resolved/search-relcall?
         wf-summary-search/search-relcall?
         wf-summary-answers/search-relcall?
         wf-summary-frontier/search-relcall?
         wf-summary-rel-env/search-relcall?
         wf-summary-config/search-relcall?
         wf-goal/search-relcall?
         wf-work/search-relcall?
         wf-resolved/search-relcall?
         wf-search/search-relcall?
         wf-answers/search-relcall?
         wf-frontier/search-relcall?
         wf-rel-env/search-relcall?
         wf-config/search-relcall?)

(check-redundancy #t)

(define-metafunction
  search-relcall-lang
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...))
   ,(relcall-arity-ok/host (term r_call)
                           (term (t ...))
                           (term ((r_1 d_1 g_1) ...)))])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-summary-goal/search-relcall? g Γ (x_1 ...) c summary)
  #:mode (wf-summary-goal/search-relcall? I I I I O)
  [(where summary_1 (summary-zero))
   ------------------ "trivial success wf/search-relcall"
   (wf-summary-goal/search-relcall? (succeed tag) Γ (x_1 ...) c summary_1)]
  [(where summary_1 (summary-zero))
   ------------------ "trivial fail wf/search-relcall"
   (wf-summary-goal/search-relcall? (fail tag) Γ (x_1 ...) c summary_1)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-summary-goal/search-relcall? g Γ (x_1 ... x_2 ...) (u_new ... u_old ...) summary_1)
   ------------------- "fresh-wf/search-relcall"
   (wf-summary-goal/search-relcall? (∃ (x_1 ...) g tag) Γ (x_2 ...) c summary_1)]
  [(wf-summary-goal/search-relcall? g_1 Γ (x_1 ...) c summary_1)
   (wf-summary-goal/search-relcall? g_2 Γ (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj-wf/search-relcall"
   (wf-summary-goal/search-relcall? (g_1 ∧ g_2 tag) Γ (x_1 ...) c summary_3)]
  [(wf-summary-goal/search-relcall? g_1 Γ (x_1 ...) c summary_1)
   (wf-summary-goal/search-relcall? g_2 Γ (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "disj-wf/search-relcall"
   (wf-summary-goal/search-relcall? (g_1 ∨ g_2 tag) Γ (x_1 ...) c summary_3)]
  [(wf-summary-goal/search-relcall? g Γ (x_1 ...) c summary_1)
   ------------------- "delay-goal-wf/search-relcall"
   (wf-summary-goal/search-relcall? (suspend g tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "==-wf/search-relcall"
   (wf-summary-goal/search-relcall? (t_1 =? t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ------------------- "=/=-wf/search-relcall"
   (wf-summary-goal/search-relcall? (t_1 != t_2 tag) Γ (x_1 ...) c summary_1)]
  [(wf-term? t (x_1 ...) c) ...
   (where #t (relcall-arity-ok? r (t ...) ((r_1 d_1 g_1) ...)))
   (where summary_1 (summary-zero))
   ------------------- "relcall-wf/search-relcall"
   (wf-summary-goal/search-relcall? (r t ... tag)
                                       ((r_1 d_1 g_1) ...)
                                       (x_1 ...)
                                       c
                                       summary_1)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-summary-resolved/search-relcall? search c summary)
  #:mode (wf-summary-resolved/search-relcall? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/search-relcall"
   (wf-summary-resolved/search-relcall? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/search-relcall"
   (wf-summary-resolved/search-relcall? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/search-relcall? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/search-relcall"
   (wf-summary-resolved/search-relcall? (ScopedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-summary-work/search-relcall? search Γ c summary)
  #:mode (wf-summary-work/search-relcall? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/search-relcall? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/search-relcall"
   (wf-summary-work/search-relcall? (ScopedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/search-relcall? g Γ () c_i summary_1)
   ------------------- "goal/state wf/search-relcall"
   (wf-summary-work/search-relcall? (g (state sub dis c_i trail tag)) Γ c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/search-relcall? search_i Γ c_i summary_1)
   (wf-summary-goal/search-relcall? g Γ () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/search-relcall"
   (wf-summary-work/search-relcall? (search_i × g c_i) Γ c summary_3)]
  [(wf-summary-search/search-relcall? search_1 Γ c summary_1)
   (wf-summary-search/search-relcall? search_2 Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "disj wf/search-relcall"
   (wf-summary-work/search-relcall? (search_1 <-+ search_2) Γ c summary_3)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-summary-search/search-relcall? search Γ c summary)
  #:mode (wf-summary-search/search-relcall? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/search-relcall? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/search-relcall"
   (wf-summary-search/search-relcall? (ScopedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-summary-resolved/search-relcall? search_i c summary_1)
   ------------------- "resolved search wf/search-relcall"
   (wf-summary-search/search-relcall? search_i Γ c summary_1)]
  [(wf-summary-work/search-relcall? search_i Γ c summary_1)
   ------------------- "work search wf/search-relcall"
   (wf-summary-search/search-relcall? search_i Γ c summary_1)]
  [(wf-summary-work/search-relcall? search_i Γ c summary_1)
   ------------------- "delay search wf/search-relcall"
   (wf-summary-search/search-relcall? (delay search_i) Γ c summary_1)])

(define-extended-judgment-form
  search-relcall-lang
  wf-summary-answers/search/base
  #:contract (wf-summary-answers/search-relcall? answers c summary)
  #:mode (wf-summary-answers/search-relcall? I I O))

(define-judgment-form
  search-relcall-lang
  #:contract (wf-summary-frontier/search-relcall? cfg Γ c summary)
  #:mode (wf-summary-frontier/search-relcall? I I I O)
  [(wf-summary-search/search-relcall? search_i Γ c summary_1)
   ------------------- "search frontier wf/search-relcall"
   (wf-summary-frontier/search-relcall? search_i Γ c summary_1)]
  [(wf-summary-search/search-relcall? search_i Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/search-relcall"
   (wf-summary-frontier/search-relcall? (Deferred search_i) Γ c summary_2)]
  [(wf-summary-answers/search-relcall? answers_i c summary_1)
   (wf-summary-frontier/search-relcall? cfg_tail Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "answers stream node wf/search-relcall"
   (wf-summary-frontier/search-relcall? (answers_i + cfg_tail) Γ c summary_3)]
  [(wf-summary-frontier/search-relcall? cfg_tail Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/search-relcall"
   (wf-summary-frontier/search-relcall? (Deferred cfg_tail) Γ c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/search-relcall? cfg_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/search-relcall"
   (wf-summary-frontier/search-relcall? (ScopedShell c_1 cfg_tail tag_1) Γ c summary_2)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-summary-rel-env/search-relcall? Γ summary)
  #:mode (wf-summary-rel-env/search-relcall? I O)
  [(wf-summary-goal/search-relcall? g ((r d g) ...) d () summary_1) ...
   (where summary_2 (summary-zero))
   ----------------------- "relation-env-wf/search-relcall"
   (wf-summary-rel-env/search-relcall? ((r d g) ...) summary_2)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-summary-config/search-relcall? config summary)
  #:mode (wf-summary-config/search-relcall? I O)
  [(wf-summary-rel-env/search-relcall? Γ summary_1)
   (wf-summary-frontier/search-relcall? cfg Γ () summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ----------------------- "program-wf/search-relcall"
   (wf-summary-config/search-relcall? (Γ cfg) summary_3)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-goal/search-relcall? g Γ (x_1 ...) c)
  #:mode (wf-goal/search-relcall? I I I I)
  [(wf-summary-goal/search-relcall? g Γ (x_1 ...) c summary_1)
   ----------------------- "goal-wf/search-relcall via summary"
   (wf-goal/search-relcall? g Γ (x_1 ...) c)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-work/search-relcall? search Γ c)
  #:mode (wf-work/search-relcall? I I I)
  [(wf-summary-work/search-relcall? search Γ c summary_1)
   ----------------------- "work-wf/search-relcall via summary"
   (wf-work/search-relcall? search Γ c)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-resolved/search-relcall? search c)
  #:mode (wf-resolved/search-relcall? I I)
  [(wf-summary-resolved/search-relcall? search c summary_1)
   ----------------------- "resolved-wf/search-relcall via summary"
   (wf-resolved/search-relcall? search c)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-search/search-relcall? search Γ c)
  #:mode (wf-search/search-relcall? I I I)
  [(wf-summary-search/search-relcall? search Γ c summary_1)
   ----------------------- "search-wf/search-relcall via summary"
   (wf-search/search-relcall? search Γ c)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-answers/search-relcall? answers c)
  #:mode (wf-answers/search-relcall? I I)
  [(wf-summary-answers/search-relcall? answers c summary_1)
   ----------------------- "answers-wf/search-relcall via summary"
   (wf-answers/search-relcall? answers c)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-frontier/search-relcall? cfg Γ c)
  #:mode (wf-frontier/search-relcall? I I I)
  [(wf-summary-frontier/search-relcall? cfg Γ c summary_1)
   ----------------------- "frontier-wf/search-relcall via summary"
   (wf-frontier/search-relcall? cfg Γ c)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-rel-env/search-relcall? Γ)
  #:mode (wf-rel-env/search-relcall? I)
  [(wf-summary-rel-env/search-relcall? Γ summary_1)
   ----------------------- "relation-env-wf/search-relcall via summary"
   (wf-rel-env/search-relcall? Γ)])

(define-judgment-form
  search-relcall-lang
  #:contract (wf-config/search-relcall? config)
  #:mode (wf-config/search-relcall? I)
  [(wf-summary-config/search-relcall? config summary_1)
   ----------------------- "program-wf/search-relcall via summary"
   (wf-config/search-relcall? config)])
