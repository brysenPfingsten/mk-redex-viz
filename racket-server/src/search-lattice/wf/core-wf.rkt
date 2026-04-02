#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "./kernel.rkt")

(provide (all-from-out "./kernel.rkt")
         wf-summary-goal/core?
         wf-summary-answer/core?
         wf-summary-resolved/core?
         wf-summary-work/core?
         wf-summary-search/core?
         wf-summary-frontier/core?
         wf-summary-cfg/core?
         wf-goal/core?
         wf-answer/core?
         wf-resolved/core?
         wf-work/core?
         wf-search/core?
         wf-frontier/core?
         wf-cfg/core?)

(check-redundancy #t)

(define-judgment-form
  core-lang
  #:contract (wf-summary-goal/core? g (x_1 ...) c summary)
  #:mode (wf-summary-goal/core? I I I O)
  [(where summary_1 (summary-zero))
   ------------------ "trivial success wf/core"
   (wf-summary-goal/core? (succeed tag) (x_1 ...) c summary_1)]
  [(where summary_1 (summary-zero))
   ------------------ "trivial fail wf/core"
   (wf-summary-goal/core? (fail tag) (x_1 ...) c summary_1)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-summary-goal/core? g (x_1 ... x_2 ...) (u_new ... u_old ...) summary_1)
   ------------------- "fresh-wf/core"
   (wf-summary-goal/core? (∃ (x_1 ...) g tag) (x_2 ...) c summary_1)]
  [(wf-summary-goal/core? g_1 (x_1 ...) c summary_1)
   (wf-summary-goal/core? g_2 (x_1 ...) c summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ---------- "conj-wf/core"
   (wf-summary-goal/core? (g_1 ∧ g_2 tag) (x_1 ...) c summary_3)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ---------- "==-wf/core"
   (wf-summary-goal/core? (t_1 =? t_2 tag) (x_1 ...) c summary_1)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   (where summary_1 (summary-zero))
   ---------- "=/=-wf/core"
   (wf-summary-goal/core? (t_1 != t_2 tag) (x_1 ...) c summary_1)])

(define-judgment-form
  core-lang
  #:contract (wf-summary-answer/core? search c summary)
  #:mode (wf-summary-answer/core? I I O)
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (where summary_1 (summary-add-answer (summary-zero)))
   ------------------- "raw answer/state wf/core"
   (wf-summary-answer/core? (⊤ (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-answer/core? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "answer-wrapper tree-freshened wf/core"
   (wf-summary-answer/core? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  core-lang
  #:contract (wf-summary-resolved/core? search c summary)
  #:mode (wf-summary-resolved/core? I I O)
  [(where summary_1 (summary-zero))
   ------------------- "empty frontier residual is wf/core"
   (wf-summary-resolved/core? (empty-tree) c summary_1)]
  [(wf-summary-answer/core? search_i c summary_1)
   ------------------- "answer is resolved wf/core"
   (wf-summary-resolved/core? search_i c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-resolved/core? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "resolved tree-freshened scope wf/core"
   (wf-summary-resolved/core? (FreshenedTree c_1 search_tail tag_1) c summary_2)])

(define-judgment-form
  core-lang
  #:contract (wf-summary-work/core? runnable-search c summary)
  #:mode (wf-summary-work/core? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-work/core? runnable-search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "work tree-freshened scope wf/core"
   (wf-summary-work/core? (FreshenedTree c_1 runnable-search_tail tag_1) c summary_2)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-summary-goal/core? g () c_i summary_1)
   ------------------- "goal/state frontier wf/core"
   (wf-summary-work/core? (g (state sub dis c_i trail tag)) c summary_1)]
  [(lvars-same-members? c c_i)
   (wf-summary-search/core? search_i c_i summary_1)
   (wf-summary-goal/core? g () c_i summary_2)
   (where summary_3 (summary+ summary_1 summary_2))
   ------------------- "conj frontier wf/core"
   (wf-summary-work/core? (search_i × g c_i) c summary_3)])

(define-judgment-form
  core-lang
  #:contract (wf-summary-search/core? search c summary)
  #:mode (wf-summary-search/core? I I O)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-search/core? search_tail c_2 summary_1)
   (where summary_2 (summary-add-tree summary_1))
   ------------------- "search tree-freshened scope wf/core"
   (wf-summary-search/core? (FreshenedTree c_1 search_tail tag_1) c summary_2)]
  [(wf-summary-resolved/core? search_i c summary_1)
   ------------------- "resolved search wf/core"
   (wf-summary-search/core? search_i c summary_1)]
  [(wf-summary-work/core? runnable-search_i c summary_1)
   ------------------- "work search wf/core"
   (wf-summary-search/core? runnable-search_i c summary_1)])

(define-judgment-form
  core-lang
  #:contract (wf-summary-frontier/core? cfg c summary)
  #:mode (wf-summary-frontier/core? I I O)
  [(wf-summary-search/core? search_i c summary_1)
   ------------------- "search frontier wf/core"
   (wf-summary-frontier/core? search_i c summary_1)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-summary-frontier/core? cfg_tail c_2 summary_1)
   (where summary_2 (summary-add-shell summary_1))
   ------------------- "cfg shell-freshened scope wf/core"
   (wf-summary-frontier/core? (FreshenedShell c_1 cfg_tail tag_1) c summary_2)])

(define-judgment-form
  core-lang
  #:contract (wf-summary-cfg/core? cfg summary)
  #:mode (wf-summary-cfg/core? I O)
  [(wf-summary-frontier/core? cfg () summary_1)
   ----------------------- "cfg-wf/core"
   (wf-summary-cfg/core? cfg summary_1)])

(define-judgment-form
  core-lang
  #:contract (wf-goal/core? g (x_1 ...) c)
  #:mode (wf-goal/core? I I I)
  [(wf-summary-goal/core? g (x_1 ...) c summary_1)
   ----------------------- "goal-wf/core via summary"
   (wf-goal/core? g (x_1 ...) c)])

(define-judgment-form
  core-lang
  #:contract (wf-answer/core? search c)
  #:mode (wf-answer/core? I I)
  [(wf-summary-answer/core? search c summary_1)
   ----------------------- "answer-wf/core via summary"
   (wf-answer/core? search c)])

(define-judgment-form
  core-lang
  #:contract (wf-resolved/core? search c)
  #:mode (wf-resolved/core? I I)
  [(wf-summary-resolved/core? search c summary_1)
   ----------------------- "resolved-wf/core via summary"
   (wf-resolved/core? search c)])

(define-judgment-form
  core-lang
  #:contract (wf-work/core? runnable-search c)
  #:mode (wf-work/core? I I)
  [(wf-summary-work/core? runnable-search c summary_1)
   ----------------------- "work-wf/core via summary"
   (wf-work/core? runnable-search c)])

(define-judgment-form
  core-lang
  #:contract (wf-search/core? search c)
  #:mode (wf-search/core? I I)
  [(wf-summary-search/core? search c summary_1)
   ----------------------- "search-wf/core via summary"
   (wf-search/core? search c)])

(define-judgment-form
  core-lang
  #:contract (wf-frontier/core? cfg c)
  #:mode (wf-frontier/core? I I)
  [(wf-summary-frontier/core? cfg c summary_1)
   ----------------------- "frontier-wf/core via summary"
   (wf-frontier/core? cfg c)])

(define-judgment-form
  core-lang
  #:contract (wf-cfg/core? cfg)
  #:mode (wf-cfg/core? I)
  [(wf-summary-cfg/core? cfg summary_1)
   ----------------------- "cfg-wf/core via summary"
   (wf-cfg/core? cfg)])
