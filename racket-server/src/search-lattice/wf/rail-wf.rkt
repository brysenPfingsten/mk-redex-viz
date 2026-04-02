#lang racket

(require redex/reduction-semantics
         "../languages/rail-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./search-base-wf.rkt"
                    [wf-summary-goal/search-base? wf-summary-goal/search-base/base]
                    [wf-summary-promoted/search-base? wf-summary-promoted/search-base/base])
         "./core-wf.rkt")

(provide wf-summary-goal/rail?
         wf-summary-work/rail?
         wf-summary-resolved/rail?
         wf-summary-search/rail?
         wf-summary-promoted/rail?
         wf-summary-frontier/rail?
         wf-summary-cfg/rail?
         wf-goal/rail?
         wf-work/rail?
         wf-resolved/rail?
         wf-search/rail?
         wf-promoted/rail?
         wf-frontier/rail?
         wf-cfg/rail?)

(check-redundancy #t)

(define-extended-judgment-form
  rail-lang
  wf-summary-goal/search-base/base
  #:contract (wf-summary-goal/rail? g (x_1 ...) c summary)
  #:mode (wf-summary-goal/rail? I I I O))

(define-judgment-form
  rail-lang
  #:contract (wf-summary-resolved/rail? search c summary)
  #:mode (wf-summary-resolved/rail? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/rail"
   (wf-summary-resolved/rail? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/rail"
   (wf-summary-resolved/rail? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/rail? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/rail"
   (wf-summary-resolved/rail? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  rail-lang
  #:contract (wf-summary-work/rail? search c summary)
  #:mode (wf-summary-work/rail? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/rail? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/rail"
   (wf-summary-work/rail? (FreshenedTree c_1 search_tail tag_1) c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/rail? g () c_i summary_1)
   ------------------- "goal/state wf/rail"
   (wf-summary-work/rail? (g (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/rail? search_i c_i summary_1)
   (wf-summary-goal/rail? g () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/rail"
   (wf-summary-work/rail? (search_i × g c_i) c summary_3)]
  [(wf-summary-search/rail? search_1 c summary_1)
   (wf-summary-search/rail? search_2 c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "left disj wf/rail"
   (wf-summary-work/rail? (search_1 <-+ search_2) c summary_3)]
  [(wf-summary-search/rail? search_1 c summary_1)
   (wf-summary-search/rail? search_2 c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "right disj wf/rail"
   (wf-summary-work/rail? (search_1 +-> search_2) c summary_3)])

(define-judgment-form
  rail-lang
  #:contract (wf-summary-search/rail? search c summary)
  #:mode (wf-summary-search/rail? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/rail? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/rail"
   (wf-summary-search/rail? (FreshenedTree c_1 search_tail tag_1) c summary_2)]
  [(wf-summary-resolved/rail? search_i c summary_1)
   ------------------- "resolved search wf/rail"
   (wf-summary-search/rail? search_i c summary_1)]
  [(wf-summary-work/rail? search_i c summary_1)
   ------------------- "work search wf/rail"
   (wf-summary-search/rail? search_i c summary_1)]
  [(wf-summary-work/rail? search_i c summary_1)
   ------------------- "delay search wf/rail"
   (wf-summary-search/rail? (delay search_i) c summary_1)])

(define-extended-judgment-form
  rail-lang
  wf-summary-promoted/search-base/base
  #:contract (wf-summary-promoted/rail? promoted c summary)
  #:mode (wf-summary-promoted/rail? I I O))

(define-judgment-form
  rail-lang
  #:contract (wf-summary-frontier/rail? cfg c summary)
  #:mode (wf-summary-frontier/rail? I I O)
  [(wf-summary-search/rail? search_i c summary_1)
   ------------------- "search frontier wf/rail"
   (wf-summary-frontier/rail? search_i c summary_1)]
  [(wf-summary-search/rail? search_i c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/rail"
   (wf-summary-frontier/rail? (Bounced search_i) c summary_2)]
  [(wf-summary-promoted/rail? promoted_i c summary_1)
   (wf-summary-frontier/rail? cfg_tail c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "promoted stream node wf/rail"
   (wf-summary-frontier/rail? (promoted_i + cfg_tail) c summary_3)]
  [(wf-summary-frontier/rail? cfg_tail c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/rail"
   (wf-summary-frontier/rail? (Bounced cfg_tail) c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/rail? cfg_tail c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/rail"
   (wf-summary-frontier/rail? (FreshenedShell c_1 cfg_tail tag_1) c summary_2)])

(define-judgment-form
  rail-lang
  #:contract (wf-summary-cfg/rail? cfg summary)
  #:mode (wf-summary-cfg/rail? I O)
  [(wf-summary-frontier/rail? cfg () summary_1)
   ----------------------- "cfg-wf/rail"
   (wf-summary-cfg/rail? cfg summary_1)])

(define-judgment-form
  rail-lang
  #:contract (wf-goal/rail? g (x_1 ...) c)
  #:mode (wf-goal/rail? I I I)
  [(wf-summary-goal/rail? g (x_1 ...) c summary_1)
   ----------------------- "goal-wf/rail via summary"
   (wf-goal/rail? g (x_1 ...) c)])

(define-judgment-form
  rail-lang
  #:contract (wf-work/rail? search c)
  #:mode (wf-work/rail? I I)
  [(wf-summary-work/rail? search c summary_1)
   ----------------------- "work-wf/rail via summary"
   (wf-work/rail? search c)])

(define-judgment-form
  rail-lang
  #:contract (wf-resolved/rail? search c)
  #:mode (wf-resolved/rail? I I)
  [(wf-summary-resolved/rail? search c summary_1)
   ----------------------- "resolved-wf/rail via summary"
   (wf-resolved/rail? search c)])

(define-judgment-form
  rail-lang
  #:contract (wf-search/rail? search c)
  #:mode (wf-search/rail? I I)
  [(wf-summary-search/rail? search c summary_1)
   ----------------------- "search-wf/rail via summary"
   (wf-search/rail? search c)])

(define-judgment-form
  rail-lang
  #:contract (wf-promoted/rail? promoted c)
  #:mode (wf-promoted/rail? I I)
  [(wf-summary-promoted/rail? promoted c summary_1)
   ----------------------- "promoted-wf/rail via summary"
   (wf-promoted/rail? promoted c)])

(define-judgment-form
  rail-lang
  #:contract (wf-frontier/rail? cfg c)
  #:mode (wf-frontier/rail? I I)
  [(wf-summary-frontier/rail? cfg c summary_1)
   ----------------------- "frontier-wf/rail via summary"
   (wf-frontier/rail? cfg c)])

(define-judgment-form
  rail-lang
  #:contract (wf-cfg/rail? cfg)
  #:mode (wf-cfg/rail? I)
  [(wf-summary-cfg/rail? cfg summary_1)
   ----------------------- "cfg-wf/rail via summary"
   (wf-cfg/rail? cfg)])
