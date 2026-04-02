#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./core-wf.rkt"
                    [wf-summary-goal/core? wf-summary-goal/core/base])
         "./core-wf.rkt")

(provide wf-summary-goal/disj?
         wf-summary-work/disj?
         wf-summary-resolved/disj?
         wf-summary-search/disj?
         wf-summary-promoted/disj?
         wf-summary-frontier/disj?
         wf-summary-cfg/disj?
         wf-goal/disj?
         wf-work/disj?
         wf-resolved/disj?
         wf-search/disj?
         wf-promoted/disj?
         wf-frontier/disj?
         wf-cfg/disj?)

(check-redundancy #t)

(define-extended-judgment-form
  disj-lang
  wf-summary-goal/core/base
  #:contract (wf-summary-goal/disj? g (x_1 ...) c summary)
  #:mode (wf-summary-goal/disj? I I I O)
  [(wf-summary-goal/disj? g_1 (x_1 ...) c summary_1)
   (wf-summary-goal/disj? g_2 (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "disj-wf/disj"
   (wf-summary-goal/disj? (g_1 ∨ g_2 tag) (x_1 ...) c summary_3)])

(define-judgment-form
  disj-lang
  #:contract (wf-summary-resolved/disj? search c summary)
  #:mode (wf-summary-resolved/disj? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/disj"
   (wf-summary-resolved/disj? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/disj"
   (wf-summary-resolved/disj? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/disj? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/disj"
   (wf-summary-resolved/disj? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  disj-lang
  #:contract (wf-summary-work/disj? runnable-search c summary)
  #:mode (wf-summary-work/disj? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/disj? runnable-search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/disj"
   (wf-summary-work/disj? (FreshenedTree c_1 runnable-search_tail tag_1) c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/disj? g () c_i summary_1)
   ------------------- "goal/state wf/disj"
   (wf-summary-work/disj? (g (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/disj? search_i c_i summary_1)
   (wf-summary-goal/disj? g () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/disj"
   (wf-summary-work/disj? (search_i × g c_i) c summary_3)]
  [(wf-summary-search/disj? search_1 c summary_1)
   (wf-summary-search/disj? search_2 c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "branch work wf/disj"
   (wf-summary-work/disj? (search_1 <-+ search_2) c summary_3)])

(define-judgment-form
  disj-lang
  #:contract (wf-summary-search/disj? search c summary)
  #:mode (wf-summary-search/disj? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/disj? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/disj"
   (wf-summary-search/disj? (FreshenedTree c_1 search_tail tag_1) c summary_2)]
  [(wf-summary-resolved/disj? search_i c summary_1)
   ------------------- "resolved search wf/disj"
   (wf-summary-search/disj? search_i c summary_1)]
  [(wf-summary-work/disj? runnable-search_i c summary_1)
   ------------------- "work search wf/disj"
   (wf-summary-search/disj? runnable-search_i c summary_1)])

(define-judgment-form
  disj-lang
  #:contract (wf-summary-promoted/disj? promoted c summary)
  #:mode (wf-summary-promoted/disj? I I O)
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw promoted/state wf/disj"
   (wf-summary-promoted/disj? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-promoted/disj? promoted_tail c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "promoted shell-freshened scope wf/disj"
   (wf-summary-promoted/disj? (FreshenedShell c_1 promoted_tail tag_1) c summary_2)])

(define-judgment-form
  disj-lang
  #:contract (wf-summary-frontier/disj? cfg c summary)
  #:mode (wf-summary-frontier/disj? I I O)
  [(wf-summary-search/disj? search_i c summary_1)
   ------------------- "search frontier wf/disj"
   (wf-summary-frontier/disj? search_i c summary_1)]
  [(wf-summary-promoted/disj? promoted_i c summary_1)
   (wf-summary-frontier/disj? cfg_tail c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "promoted stream node wf/disj"
   (wf-summary-frontier/disj? (promoted_i + cfg_tail) c summary_3)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/disj? cfg_tail c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/disj"
   (wf-summary-frontier/disj? (FreshenedShell c_1 cfg_tail tag_1) c summary_2)])

(define-judgment-form
  disj-lang
  #:contract (wf-summary-cfg/disj? cfg summary)
  #:mode (wf-summary-cfg/disj? I O)
  [(wf-summary-frontier/disj? cfg () summary_1)
   ----------------------- "cfg-wf/disj"
   (wf-summary-cfg/disj? cfg summary_1)])

(define-judgment-form
  disj-lang
  #:contract (wf-goal/disj? g (x_1 ...) c)
  #:mode (wf-goal/disj? I I I)
  [(wf-summary-goal/disj? g (x_1 ...) c summary_1)
   ----------------------- "goal-wf/disj via summary"
   (wf-goal/disj? g (x_1 ...) c)])

(define-judgment-form
  disj-lang
  #:contract (wf-work/disj? runnable-search c)
  #:mode (wf-work/disj? I I)
  [(wf-summary-work/disj? runnable-search c summary_1)
   ----------------------- "work-wf/disj via summary"
   (wf-work/disj? runnable-search c)])

(define-judgment-form
  disj-lang
  #:contract (wf-resolved/disj? search c)
  #:mode (wf-resolved/disj? I I)
  [(wf-summary-resolved/disj? search c summary_1)
   ----------------------- "resolved-wf/disj via summary"
   (wf-resolved/disj? search c)])

(define-judgment-form
  disj-lang
  #:contract (wf-search/disj? search c)
  #:mode (wf-search/disj? I I)
  [(wf-summary-search/disj? search c summary_1)
   ----------------------- "search-wf/disj via summary"
   (wf-search/disj? search c)])

(define-judgment-form
  disj-lang
  #:contract (wf-promoted/disj? promoted c)
  #:mode (wf-promoted/disj? I I)
  [(wf-summary-promoted/disj? promoted c summary_1)
   ----------------------- "promoted-wf/disj via summary"
   (wf-promoted/disj? promoted c)])

(define-judgment-form
  disj-lang
  #:contract (wf-frontier/disj? cfg c)
  #:mode (wf-frontier/disj? I I)
  [(wf-summary-frontier/disj? cfg c summary_1)
   ----------------------- "frontier-wf/disj via summary"
   (wf-frontier/disj? cfg c)])

(define-judgment-form
  disj-lang
  #:contract (wf-cfg/disj? cfg)
  #:mode (wf-cfg/disj? I)
  [(wf-summary-cfg/disj? cfg summary_1)
   ----------------------- "cfg-wf/disj via summary"
   (wf-cfg/disj? cfg)])
