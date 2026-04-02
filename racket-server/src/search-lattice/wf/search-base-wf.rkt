#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./disj-wf.rkt"
                    [wf-summary-goal/disj? wf-summary-goal/disj/base]
                    [wf-summary-promoted/disj? wf-summary-promoted/disj/base])
         "./core-wf.rkt")

(provide wf-summary-goal/search-base?
         wf-summary-work/search-base?
         wf-summary-resolved/search-base?
         wf-summary-search/search-base?
         wf-summary-promoted/search-base?
         wf-summary-frontier/search-base?
         wf-summary-cfg/search-base?
         wf-goal/search-base?
         wf-work/search-base?
         wf-resolved/search-base?
         wf-search/search-base?
         wf-promoted/search-base?
         wf-frontier/search-base?
         wf-cfg/search-base?)

(check-redundancy #t)

(define-extended-judgment-form
  search-base-lang
  wf-summary-goal/disj/base
  #:contract (wf-summary-goal/search-base? g (x_1 ...) c summary)
  #:mode (wf-summary-goal/search-base? I I I O)
  [(wf-summary-goal/search-base? g (x_1 ...) c summary_1)
   ------------------- "delay-goal-wf/search-base"
   (wf-summary-goal/search-base? (suspend g tag) (x_1 ...) c summary_1)])

(define-judgment-form
  search-base-lang
  #:contract (wf-summary-resolved/search-base? search c summary)
  #:mode (wf-summary-resolved/search-base? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/search-base"
   (wf-summary-resolved/search-base? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/search-base"
   (wf-summary-resolved/search-base? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/search-base? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/search-base"
   (wf-summary-resolved/search-base? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  search-base-lang
  #:contract (wf-summary-work/search-base? runnable-search c summary)
  #:mode (wf-summary-work/search-base? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/search-base? runnable-search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/search-base"
   (wf-summary-work/search-base? (FreshenedTree c_1 runnable-search_tail tag_1) c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/search-base? g () c_i summary_1)
   ------------------- "goal/state wf/search-base"
   (wf-summary-work/search-base? (g (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/search-base? search_i c_i summary_1)
   (wf-summary-goal/search-base? g () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/search-base"
   (wf-summary-work/search-base? (search_i × g c_i) c summary_3)]
  [(wf-summary-search/search-base? search_1 c summary_1)
   (wf-summary-search/search-base? search_2 c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "disj wf/search-base"
   (wf-summary-work/search-base? (search_1 <-+ search_2) c summary_3)])

(define-judgment-form
  search-base-lang
  #:contract (wf-summary-search/search-base? search c summary)
  #:mode (wf-summary-search/search-base? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/search-base? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/search-base"
   (wf-summary-search/search-base? (FreshenedTree c_1 search_tail tag_1) c summary_2)]
  [(wf-summary-resolved/search-base? search_i c summary_1)
   ------------------- "resolved search wf/search-base"
   (wf-summary-search/search-base? search_i c summary_1)]
  [(wf-summary-work/search-base? runnable-search_i c summary_1)
   ------------------- "work search wf/search-base"
   (wf-summary-search/search-base? runnable-search_i c summary_1)]
  [(wf-summary-work/search-base? runnable-search_i c summary_1)
   ------------------- "delay search wf/search-base"
   (wf-summary-search/search-base? (delay runnable-search_i) c summary_1)])

(define-extended-judgment-form
  search-base-lang
  wf-summary-promoted/disj/base
  #:contract (wf-summary-promoted/search-base? promoted c summary)
  #:mode (wf-summary-promoted/search-base? I I O))

(define-judgment-form
  search-base-lang
  #:contract (wf-summary-frontier/search-base? cfg c summary)
  #:mode (wf-summary-frontier/search-base? I I O)
  [(wf-summary-search/search-base? search_i c summary_1)
   ------------------- "search frontier wf/search-base"
   (wf-summary-frontier/search-base? search_i c summary_1)]
  [(wf-summary-search/search-base? search_i c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/search-base"
   (wf-summary-frontier/search-base? (Bounced search_i) c summary_2)]
  [(wf-summary-promoted/search-base? promoted_i c summary_1)
   (wf-summary-frontier/search-base? cfg_tail c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "promoted stream node wf/search-base"
   (wf-summary-frontier/search-base? (promoted_i + cfg_tail) c summary_3)]
  [(wf-summary-frontier/search-base? cfg_tail c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/search-base"
   (wf-summary-frontier/search-base? (Bounced cfg_tail) c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/search-base? cfg_tail c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/search-base"
   (wf-summary-frontier/search-base? (FreshenedShell c_1 cfg_tail tag_1) c summary_2)])

(define-judgment-form
  search-base-lang
  #:contract (wf-summary-cfg/search-base? cfg summary)
  #:mode (wf-summary-cfg/search-base? I O)
  [(wf-summary-frontier/search-base? cfg () summary_1)
   ----------------------- "cfg-wf/search-base"
   (wf-summary-cfg/search-base? cfg summary_1)])

(define-judgment-form
  search-base-lang
  #:contract (wf-goal/search-base? g (x_1 ...) c)
  #:mode (wf-goal/search-base? I I I)
  [(wf-summary-goal/search-base? g (x_1 ...) c summary_1)
   ----------------------- "goal-wf/search-base via summary"
   (wf-goal/search-base? g (x_1 ...) c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-work/search-base? runnable-search c)
  #:mode (wf-work/search-base? I I)
  [(wf-summary-work/search-base? runnable-search c summary_1)
   ----------------------- "work-wf/search-base via summary"
   (wf-work/search-base? runnable-search c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-resolved/search-base? search c)
  #:mode (wf-resolved/search-base? I I)
  [(wf-summary-resolved/search-base? search c summary_1)
   ----------------------- "resolved-wf/search-base via summary"
   (wf-resolved/search-base? search c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-search/search-base? search c)
  #:mode (wf-search/search-base? I I)
  [(wf-summary-search/search-base? search c summary_1)
   ----------------------- "search-wf/search-base via summary"
   (wf-search/search-base? search c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-promoted/search-base? promoted c)
  #:mode (wf-promoted/search-base? I I)
  [(wf-summary-promoted/search-base? promoted c summary_1)
   ----------------------- "promoted-wf/search-base via summary"
   (wf-promoted/search-base? promoted c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-frontier/search-base? cfg c)
  #:mode (wf-frontier/search-base? I I)
  [(wf-summary-frontier/search-base? cfg c summary_1)
   ----------------------- "frontier-wf/search-base via summary"
   (wf-frontier/search-base? cfg c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-cfg/search-base? cfg)
  #:mode (wf-cfg/search-base? I)
  [(wf-summary-cfg/search-base? cfg summary_1)
   ----------------------- "cfg-wf/search-base via summary"
   (wf-cfg/search-base? cfg)])
