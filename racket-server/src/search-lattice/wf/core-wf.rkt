#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "./kernel.rkt")

(provide (all-from-out "./kernel.rkt")
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
  #:contract (wf-goal/core? g (x_1 ...) c)
  #:mode (wf-goal/core? I I I)
  [------------------ "trivial success wf/core"
   (wf-goal/core? (succeed tag) (x_1 ...) c)]
  [------------------ "trivial fail wf/core"
   (wf-goal/core? (fail tag) (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/core? g (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/core"
   (wf-goal/core? (∃ (x_1 ...) g tag) (x_2 ...) c)]
  [(wf-goal/core? g_1 (x_1 ...) c)
   (wf-goal/core? g_2 (x_1 ...) c)
   ---------- "conj-wf/core"
   (wf-goal/core? (g_1 ∧ g_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "==-wf/core"
   (wf-goal/core? (t_1 =? t_2 tag) (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "=/=-wf/core"
   (wf-goal/core? (t_1 != t_2 tag) (x_1 ...) c)])

(define-judgment-form
  core-lang
  #:contract (wf-answer/core? search c)
  #:mode (wf-answer/core? I I)
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/core"
   (wf-answer/core? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-answer/core? search_tail c_2)
   ------------------- "answer-wrapper tree-freshened wf/core"
   (wf-answer/core? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  core-lang
  #:contract (wf-resolved/core? search c)
  #:mode (wf-resolved/core? I I)
  [------------------- "empty frontier residual is wf/core"
   (wf-resolved/core? (empty-tree) c)]
  [(wf-answer/core? search_i c)
   ------------------- "answer is resolved wf/core"
   (wf-resolved/core? search_i c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/core? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/core"
   (wf-resolved/core? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  core-lang
  #:contract (wf-work/core? runnable-search c)
  #:mode (wf-work/core? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/core? runnable-search_tail c_2)
   ------------------- "work tree-freshened scope wf/core"
   (wf-work/core? (FreshenedTree c_1 runnable-search_tail tag_1) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/core? g () c_i)
   ------------------- "goal/state frontier wf/core"
   (wf-work/core? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-search/core? search_i c_i)
   (wf-goal/core? g () c_i)
   ------------------- "conj frontier wf/core"
   (wf-work/core? (search_i × g c_i) c)])

(define-judgment-form
  core-lang
  #:contract (wf-search/core? search c)
  #:mode (wf-search/core? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/core? search_tail c_2)
   ------------------- "search tree-freshened scope wf/core"
   (wf-search/core? (FreshenedTree c_1 search_tail tag_1) c)]
  [(wf-resolved/core? search_i c)
   ------------------- "resolved search wf/core"
   (wf-search/core? search_i c)]
  [(wf-work/core? runnable-search_i c)
   ------------------- "work search wf/core"
   (wf-search/core? runnable-search_i c)])

(define-judgment-form
  core-lang
  #:contract (wf-frontier/core? cfg c)
  #:mode (wf-frontier/core? I I)
  [(wf-search/core? search_i c)
   ------------------- "search frontier wf/core"
   (wf-frontier/core? search_i c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/core? cfg_tail c_2)
   ------------------- "cfg shell-freshened scope wf/core"
   (wf-frontier/core? (FreshenedShell c_1 cfg_tail tag_1) c)])

(define-judgment-form
  core-lang
  #:contract (wf-cfg/core? cfg)
  #:mode (wf-cfg/core? I)
  [(wf-frontier/core? cfg ())
   ----------------------- "cfg-wf/core"
   (wf-cfg/core? cfg)])
