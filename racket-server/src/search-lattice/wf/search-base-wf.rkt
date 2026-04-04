#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./disj-wf.rkt"
                    [wf-summary-goal/disj? wf-summary-goal/disj/base]
                    [wf-summary-answers/disj? wf-summary-answers/disj/base])
         "./core-wf.rkt")

(provide wf-summary-goal/search?
         wf-summary-work/search?
         wf-summary-resolved/search?
         wf-summary-search/search?
         wf-summary-answers/search?
         wf-summary-frontier/search?
         wf-summary-cfg/search?
         wf-goal/search?
         wf-work/search?
         wf-resolved/search?
         wf-search/search?
         wf-answers/search?
         wf-frontier/search?
         wf-cfg/search?)

(check-redundancy #t)

(define-extended-judgment-form
  search-lang
  wf-summary-goal/disj/base
  #:contract (wf-summary-goal/search? g (x_1 ...) c summary)
  #:mode (wf-summary-goal/search? I I I O)
  [(wf-summary-goal/search? g (x_1 ...) c summary_1)
   ------------------- "delay-goal-wf/search"
   (wf-summary-goal/search? (suspend g tag) (x_1 ...) c summary_1)])

(define-judgment-form
  search-lang
  #:contract (wf-summary-resolved/search? search c summary)
  #:mode (wf-summary-resolved/search? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/search"
   (wf-summary-resolved/search? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/search"
   (wf-summary-resolved/search? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/search? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/search"
   (wf-summary-resolved/search? (ScopedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  search-lang
  #:contract (wf-summary-work/search? runnable-search c summary)
  #:mode (wf-summary-work/search? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/search? runnable-search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/search"
   (wf-summary-work/search? (ScopedTree c_1 runnable-search_tail tag_1) c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/search? g () c_i summary_1)
   ------------------- "goal/state wf/search"
   (wf-summary-work/search? (g (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/search? search_i c_i summary_1)
   (wf-summary-goal/search? g () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/search"
   (wf-summary-work/search? (search_i × g c_i) c summary_3)]
  [(wf-summary-search/search? search_1 c summary_1)
   (wf-summary-search/search? search_2 c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "disj wf/search"
   (wf-summary-work/search? (search_1 <-+ search_2) c summary_3)])

(define-judgment-form
  search-lang
  #:contract (wf-summary-search/search? search c summary)
  #:mode (wf-summary-search/search? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/search? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/search"
   (wf-summary-search/search? (ScopedTree c_1 search_tail tag_1) c summary_2)]
  [(wf-summary-resolved/search? search_i c summary_1)
   ------------------- "resolved search wf/search"
   (wf-summary-search/search? search_i c summary_1)]
  [(wf-summary-work/search? runnable-search_i c summary_1)
   ------------------- "work search wf/search"
   (wf-summary-search/search? runnable-search_i c summary_1)]
  [(wf-summary-work/search? runnable-search_i c summary_1)
   ------------------- "delay search wf/search"
   (wf-summary-search/search? (delay runnable-search_i) c summary_1)])

(define-extended-judgment-form
  search-lang
  wf-summary-answers/disj/base
  #:contract (wf-summary-answers/search? answers c summary)
  #:mode (wf-summary-answers/search? I I O))

(define-judgment-form
  search-lang
  #:contract (wf-summary-frontier/search? cfg c summary)
  #:mode (wf-summary-frontier/search? I I O)
  [(wf-summary-search/search? search_i c summary_1)
   ------------------- "search frontier wf/search"
   (wf-summary-frontier/search? search_i c summary_1)]
  [(wf-summary-search/search? search_i c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/search"
   (wf-summary-frontier/search? (Deferred search_i) c summary_2)]
  [(wf-summary-answers/search? answers_i c summary_1)
   (wf-summary-frontier/search? cfg_tail c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "answers stream node wf/search"
   (wf-summary-frontier/search? (answers_i + cfg_tail) c summary_3)]
  [(wf-summary-frontier/search? cfg_tail c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/search"
   (wf-summary-frontier/search? (Deferred cfg_tail) c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/search? cfg_tail c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/search"
   (wf-summary-frontier/search? (ScopedShell c_1 cfg_tail tag_1) c summary_2)])

(define-judgment-form
  search-lang
  #:contract (wf-summary-cfg/search? cfg summary)
  #:mode (wf-summary-cfg/search? I O)
  [(wf-summary-frontier/search? cfg () summary_1)
   ----------------------- "cfg-wf/search"
   (wf-summary-cfg/search? cfg summary_1)])

(define-judgment-form
  search-lang
  #:contract (wf-goal/search? g (x_1 ...) c)
  #:mode (wf-goal/search? I I I)
  [(wf-summary-goal/search? g (x_1 ...) c summary_1)
   ----------------------- "goal-wf/search via summary"
   (wf-goal/search? g (x_1 ...) c)])

(define-judgment-form
  search-lang
  #:contract (wf-work/search? runnable-search c)
  #:mode (wf-work/search? I I)
  [(wf-summary-work/search? runnable-search c summary_1)
   ----------------------- "work-wf/search via summary"
   (wf-work/search? runnable-search c)])

(define-judgment-form
  search-lang
  #:contract (wf-resolved/search? search c)
  #:mode (wf-resolved/search? I I)
  [(wf-summary-resolved/search? search c summary_1)
   ----------------------- "resolved-wf/search via summary"
   (wf-resolved/search? search c)])

(define-judgment-form
  search-lang
  #:contract (wf-search/search? search c)
  #:mode (wf-search/search? I I)
  [(wf-summary-search/search? search c summary_1)
   ----------------------- "search-wf/search via summary"
   (wf-search/search? search c)])

(define-judgment-form
  search-lang
  #:contract (wf-answers/search? answers c)
  #:mode (wf-answers/search? I I)
  [(wf-summary-answers/search? answers c summary_1)
   ----------------------- "answers-wf/search via summary"
   (wf-answers/search? answers c)])

(define-judgment-form
  search-lang
  #:contract (wf-frontier/search? cfg c)
  #:mode (wf-frontier/search? I I)
  [(wf-summary-frontier/search? cfg c summary_1)
   ----------------------- "frontier-wf/search via summary"
   (wf-frontier/search? cfg c)])

(define-judgment-form
  search-lang
  #:contract (wf-cfg/search? cfg)
  #:mode (wf-cfg/search? I)
  [(wf-summary-cfg/search? cfg summary_1)
   ----------------------- "cfg-wf/search via summary"
   (wf-cfg/search? cfg)])
