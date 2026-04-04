#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./core-wf.rkt"
                    [wf-summary-goal/core? wf-summary-goal/core/base])
         "./core-wf.rkt")

(provide wf-summary-goal/delay?
         wf-summary-work/delay?
         wf-summary-resolved/delay?
         wf-summary-search/delay?
         wf-summary-frontier/delay?
         wf-summary-cfg/delay?
         wf-goal/delay?
         wf-work/delay?
         wf-resolved/delay?
         wf-search/delay?
         wf-frontier/delay?
         wf-cfg/delay?)

(check-redundancy #t)

(define-extended-judgment-form
  delay-lang
  wf-summary-goal/core/base
  #:contract (wf-summary-goal/delay? g (x_1 ...) c summary)
  #:mode (wf-summary-goal/delay? I I I O)
  [(wf-summary-goal/delay? g (x_1 ...) c summary_1)
   ------------------- "delay-goal-wf/delay"
   (wf-summary-goal/delay? (suspend g tag) (x_1 ...) c summary_1)])

(define-judgment-form
  delay-lang
  #:contract (wf-summary-resolved/delay? search c summary)
  #:mode (wf-summary-resolved/delay? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/delay"
   (wf-summary-resolved/delay? (empty-tree) c summary_1)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/delay"
   (wf-summary-resolved/delay? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/delay? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/delay"
   (wf-summary-resolved/delay? (ScopedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  delay-lang
  #:contract (wf-summary-work/delay? runnable-search c summary)
  #:mode (wf-summary-work/delay? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/delay? runnable-search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/delay"
   (wf-summary-work/delay? (ScopedTree c_1 runnable-search_tail tag_1) c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/delay? g () c_i summary_1)
   ------------------- "goal/state wf/delay"
   (wf-summary-work/delay? (g (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/delay? search_i c_i summary_1)
   (wf-summary-goal/delay? g () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj wf/delay"
   (wf-summary-work/delay? (search_i × g c_i) c summary_3)])

(define-judgment-form
  delay-lang
  #:contract (wf-summary-search/delay? search c summary)
  #:mode (wf-summary-search/delay? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/delay? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/delay"
   (wf-summary-search/delay? (ScopedTree c_1 search_tail tag_1) c summary_2)]
  [(wf-summary-resolved/delay? search_i c summary_1)
   ------------------- "resolved search wf/delay"
   (wf-summary-search/delay? search_i c summary_1)]
  [(wf-summary-work/delay? runnable-search_i c summary_1)
   ------------------- "work search wf/delay"
   (wf-summary-search/delay? runnable-search_i c summary_1)]
  [(wf-summary-work/delay? runnable-search_i c summary_1)
   ------------------- "delay search wf/delay"
   (wf-summary-search/delay? (delay runnable-search_i) c summary_1)])

(define-judgment-form
  delay-lang
  #:contract (wf-summary-frontier/delay? cfg c summary)
  #:mode (wf-summary-frontier/delay? I I O)
  [(wf-summary-search/delay? search_i c summary_1)
   ------------------- "search frontier wf/delay"
   (wf-summary-frontier/delay? search_i c summary_1)]
  [(wf-summary-search/delay? search_i c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced search frontier wf/delay"
   (wf-summary-frontier/delay? (Deferred search_i) c summary_2)]
  [(wf-summary-frontier/delay? cfg_tail c summary_1)
   (where summary_2 (summary-add-bounced summary_1))
   ------------------- "bounced frontier wf/delay"
   (wf-summary-frontier/delay? (Deferred cfg_tail) c summary_2)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/delay? cfg_tail c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/delay"
   (wf-summary-frontier/delay? (ScopedShell c_1 cfg_tail tag_1) c summary_2)])

(define-judgment-form
  delay-lang
  #:contract (wf-summary-cfg/delay? cfg summary)
  #:mode (wf-summary-cfg/delay? I O)
  [(wf-summary-frontier/delay? cfg () summary_1)
   ----------------------- "cfg-wf/delay"
   (wf-summary-cfg/delay? cfg summary_1)])

(define-judgment-form
  delay-lang
  #:contract (wf-goal/delay? g (x_1 ...) c)
  #:mode (wf-goal/delay? I I I)
  [(wf-summary-goal/delay? g (x_1 ...) c summary_1)
   ----------------------- "goal-wf/delay via summary"
   (wf-goal/delay? g (x_1 ...) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-work/delay? runnable-search c)
  #:mode (wf-work/delay? I I)
  [(wf-summary-work/delay? runnable-search c summary_1)
   ----------------------- "work-wf/delay via summary"
   (wf-work/delay? runnable-search c)])

(define-judgment-form
  delay-lang
  #:contract (wf-resolved/delay? search c)
  #:mode (wf-resolved/delay? I I)
  [(wf-summary-resolved/delay? search c summary_1)
   ----------------------- "resolved-wf/delay via summary"
   (wf-resolved/delay? search c)])

(define-judgment-form
  delay-lang
  #:contract (wf-search/delay? search c)
  #:mode (wf-search/delay? I I)
  [(wf-summary-search/delay? search c summary_1)
   ----------------------- "search-wf/delay via summary"
   (wf-search/delay? search c)])

(define-judgment-form
  delay-lang
  #:contract (wf-frontier/delay? cfg c)
  #:mode (wf-frontier/delay? I I)
  [(wf-summary-frontier/delay? cfg c summary_1)
   ----------------------- "frontier-wf/delay via summary"
   (wf-frontier/delay? cfg c)])

(define-judgment-form
  delay-lang
  #:contract (wf-cfg/delay? cfg)
  #:mode (wf-cfg/delay? I)
  [(wf-summary-cfg/delay? cfg summary_1)
   ----------------------- "cfg-wf/delay via summary"
   (wf-cfg/delay? cfg)])
