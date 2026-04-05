#lang racket

(require redex/reduction-semantics
         "../languages/rail-relcall-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./search-relcall-wf.rkt"
                    [wf-summary-goal/search-relcall? wf-summary-goal/search-relcall/base]
                    [wf-summary-answers/search-relcall? wf-summary-answers/search-relcall/base])
         "./core-wf.rkt")

(provide wf-summary-goal/rail-relcall?
         wf-summary-work/rail-relcall?
         wf-summary-resolved/rail-relcall?
         wf-summary-search/rail-relcall?
         wf-summary-answers/rail-relcall?
         wf-summary-frontier/rail-relcall?
         wf-summary-rel-env/rail-relcall?
         wf-summary-config/rail-relcall?
         wf-goal/rail-relcall?
         wf-work/rail-relcall?
         wf-resolved/rail-relcall?
         wf-search/rail-relcall?
         wf-answers/rail-relcall?
         wf-frontier/rail-relcall?
         wf-rel-env/rail-relcall?
         wf-config/rail-relcall?)

(check-redundancy #t)

(define-extended-judgment-form
  rail-relcall-lang
  wf-summary-goal/search-relcall/base
  #:contract (wf-summary-goal/rail-relcall? g Γ (x_1 ...) c summary)
  #:mode (wf-summary-goal/rail-relcall? I I I I O))

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-summary-resolved/rail-relcall? search c summary)
  #:mode (wf-summary-resolved/rail-relcall? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/rail-relcall"
   (wf-summary-resolved/rail-relcall? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/rail-relcall"
   (wf-summary-resolved/rail-relcall? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/rail-relcall? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/rail-relcall"
   (wf-summary-resolved/rail-relcall? (ScopedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-summary-work/rail-relcall? search Γ c summary)
  #:mode (wf-summary-work/rail-relcall? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/rail-relcall? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/rail-relcall"
   (wf-summary-work/rail-relcall? (ScopedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/rail-relcall? g Γ () c_i summary_1)
   ------------------- "goal/state wf/rail-relcall"
   (wf-summary-work/rail-relcall? (g (state sub dis c_i trail tag)) Γ c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/rail-relcall? search_i Γ c_i summary_1)
   (wf-summary-goal/rail-relcall? g Γ () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/rail-relcall"
   (wf-summary-work/rail-relcall? (search_i × g c_i) Γ c summary_3)]
  [(wf-summary-search/rail-relcall? search_1 Γ c summary_1)
   (wf-summary-search/rail-relcall? search_2 Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "left disj wf/rail-relcall"
   (wf-summary-work/rail-relcall? (search_1 <-+ search_2) Γ c summary_3)]
  [(wf-summary-search/rail-relcall? search_1 Γ c summary_1)
   (wf-summary-search/rail-relcall? search_2 Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "right disj wf/rail-relcall"
   (wf-summary-work/rail-relcall? (search_1 +-> search_2) Γ c summary_3)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-summary-search/rail-relcall? search Γ c summary)
  #:mode (wf-summary-search/rail-relcall? I I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/rail-relcall? search_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/rail-relcall"
   (wf-summary-search/rail-relcall? (ScopedTree c_1 search_tail tag_1) Γ c summary_2)]
  [(wf-summary-resolved/rail-relcall? search_i c summary_1)
   ------------------- "resolved search wf/rail-relcall"
   (wf-summary-search/rail-relcall? search_i Γ c summary_1)]
  [(wf-summary-work/rail-relcall? search_i Γ c summary_1)
   ------------------- "work search wf/rail-relcall"
   (wf-summary-search/rail-relcall? search_i Γ c summary_1)]
  [(wf-summary-work/rail-relcall? search_i Γ c summary_1)
   ------------------- "delay search wf/rail-relcall"
   (wf-summary-search/rail-relcall? (delay search_i) Γ c summary_1)])

(define-extended-judgment-form
  rail-relcall-lang
  wf-summary-answers/search-relcall/base
  #:contract (wf-summary-answers/rail-relcall? answers c summary)
  #:mode (wf-summary-answers/rail-relcall? I I O))

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-summary-frontier/rail-relcall? cfg Γ c summary)
  #:mode (wf-summary-frontier/rail-relcall? I I I O)
  [(wf-summary-search/rail-relcall? search_i Γ c summary_1)
   ------------------- "search frontier wf/rail-relcall"
   (wf-summary-frontier/rail-relcall? search_i Γ c summary_1)]
  [(wf-summary-search/rail-relcall? search_i Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/rail-relcall"
   (wf-summary-frontier/rail-relcall? (Deferred search_i) Γ c summary_2)]
  [(wf-summary-answers/rail-relcall? answers_i c summary_1)
   (wf-summary-frontier/rail-relcall? cfg_tail Γ c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "answers stream node wf/rail-relcall"
   (wf-summary-frontier/rail-relcall? (answers_i + cfg_tail) Γ c summary_3)]
  [(wf-summary-frontier/rail-relcall? cfg_tail Γ c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/rail-relcall"
   (wf-summary-frontier/rail-relcall? (Deferred cfg_tail) Γ c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/rail-relcall? cfg_tail Γ c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/rail-relcall"
   (wf-summary-frontier/rail-relcall? (ScopedShell c_1 cfg_tail tag_1) Γ c summary_2)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-summary-rel-env/rail-relcall? Γ summary)
  #:mode (wf-summary-rel-env/rail-relcall? I O)
  [(wf-summary-goal/rail-relcall? g ((r d g) ...) d () summary_1) ...
   (where summary_2 (summary-zero))
   ----------------------- "relation-env-wf/rail-relcall"
   (wf-summary-rel-env/rail-relcall? ((r d g) ...) summary_2)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-summary-config/rail-relcall? config summary)
  #:mode (wf-summary-config/rail-relcall? I O)
  [(wf-summary-rel-env/rail-relcall? Γ summary_1)
   (wf-summary-frontier/rail-relcall? cfg Γ () summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ----------------------- "program-wf/rail-relcall"
   (wf-summary-config/rail-relcall? (Γ cfg) summary_3)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-goal/rail-relcall? g Γ (x_1 ...) c)
  #:mode (wf-goal/rail-relcall? I I I I)
  [(wf-summary-goal/rail-relcall? g Γ (x_1 ...) c summary_1)
   ----------------------- "goal-wf/rail-relcall via summary"
   (wf-goal/rail-relcall? g Γ (x_1 ...) c)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-work/rail-relcall? search Γ c)
  #:mode (wf-work/rail-relcall? I I I)
  [(wf-summary-work/rail-relcall? search Γ c summary_1)
   ----------------------- "work-wf/rail-relcall via summary"
   (wf-work/rail-relcall? search Γ c)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-resolved/rail-relcall? search c)
  #:mode (wf-resolved/rail-relcall? I I)
  [(wf-summary-resolved/rail-relcall? search c summary_1)
   ----------------------- "resolved-wf/rail-relcall via summary"
   (wf-resolved/rail-relcall? search c)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-search/rail-relcall? search Γ c)
  #:mode (wf-search/rail-relcall? I I I)
  [(wf-summary-search/rail-relcall? search Γ c summary_1)
   ----------------------- "search-wf/rail-relcall via summary"
   (wf-search/rail-relcall? search Γ c)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-answers/rail-relcall? answers c)
  #:mode (wf-answers/rail-relcall? I I)
  [(wf-summary-answers/rail-relcall? answers c summary_1)
   ----------------------- "answers-wf/rail-relcall via summary"
   (wf-answers/rail-relcall? answers c)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-frontier/rail-relcall? cfg Γ c)
  #:mode (wf-frontier/rail-relcall? I I I)
  [(wf-summary-frontier/rail-relcall? cfg Γ c summary_1)
   ----------------------- "frontier-wf/rail-relcall via summary"
   (wf-frontier/rail-relcall? cfg Γ c)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-rel-env/rail-relcall? Γ)
  #:mode (wf-rel-env/rail-relcall? I)
  [(wf-summary-rel-env/rail-relcall? Γ summary_1)
   ----------------------- "relation-env-wf/rail-relcall via summary"
   (wf-rel-env/rail-relcall? Γ)])

(define-judgment-form
  rail-relcall-lang
  #:contract (wf-config/rail-relcall? config)
  #:mode (wf-config/rail-relcall? I)
  [(wf-summary-config/rail-relcall? config summary_1)
   ----------------------- "program-wf/rail-relcall via summary"
   (wf-config/rail-relcall? config)])
