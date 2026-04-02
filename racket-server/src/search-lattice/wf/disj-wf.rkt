#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./core-wf.rkt"
                    [wf-goal/core? wf-goal/core/base])
         "./core-wf.rkt")

(provide wf-goal/disj?
         wf-work/disj?
         wf-resolved/disj?
         wf-search/disj?
         wf-promoted/disj?
         wf-frontier/disj?
         wf-cfg/disj?)

(check-redundancy #t)

(define-extended-judgment-form
  disj-lang
  wf-goal/core/base
  #:contract (wf-goal/disj? g (x_1 ...) c)
  #:mode (wf-goal/disj? I I I)
  [(wf-goal/disj? g_1 (x_1 ...) c)
   (wf-goal/disj? g_2 (x_1 ...) c)
   ------------------- "disj-wf/disj"
   (wf-goal/disj? (g_1 ∨ g_2 tag) (x_1 ...) c)])

(define-judgment-form
  disj-lang
  #:contract (wf-resolved/disj? search c)
  #:mode (wf-resolved/disj? I I)
  [------------------- "empty frontier residual is wf/disj"
   (wf-resolved/disj? (empty-tree) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/disj"
   (wf-resolved/disj? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/disj? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/disj"
   (wf-resolved/disj? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  disj-lang
  #:contract (wf-work/disj? runnable-search c)
  #:mode (wf-work/disj? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/disj? runnable-search_tail c_2)
   ------------------- "work tree-freshened scope wf/disj"
   (wf-work/disj? (FreshenedTree c_1 runnable-search_tail tag_1) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/disj? g () c_i)
   ------------------- "goal/state wf/disj"
   (wf-work/disj? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-search/disj? search_i c_i)
   (wf-goal/disj? g () c_i)
   ------------------- "conj wf/disj"
   (wf-work/disj? (search_i × g c_i) c)]
  [(wf-search/disj? search_1 c)
   (wf-search/disj? search_2 c)
   ------------------- "branch work wf/disj"
   (wf-work/disj? (search_1 <-+ search_2) c)])

(define-judgment-form
  disj-lang
  #:contract (wf-search/disj? search c)
  #:mode (wf-search/disj? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/disj? search_tail c_2)
   ------------------- "search tree-freshened scope wf/disj"
   (wf-search/disj? (FreshenedTree c_1 search_tail tag_1) c)]
  [(wf-resolved/disj? search_i c)
   ------------------- "resolved search wf/disj"
   (wf-search/disj? search_i c)]
  [(wf-work/disj? runnable-search_i c)
   ------------------- "work search wf/disj"
   (wf-search/disj? runnable-search_i c)])

(define-judgment-form
  disj-lang
  #:contract (wf-promoted/disj? promoted c)
  #:mode (wf-promoted/disj? I I)
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw promoted/state wf/disj"
   (wf-promoted/disj? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-promoted/disj? promoted_tail c_2)
   ------------------- "promoted shell-freshened scope wf/disj"
   (wf-promoted/disj? (FreshenedShell c_1 promoted_tail tag_1) c)])

(define-judgment-form
  disj-lang
  #:contract (wf-frontier/disj? cfg c)
  #:mode (wf-frontier/disj? I I)
  [(wf-search/disj? search_i c)
   ------------------- "search frontier wf/disj"
   (wf-frontier/disj? search_i c)]
  [(wf-promoted/disj? promoted_i c)
   (wf-frontier/disj? cfg_tail c)
   ------------------- "promoted stream node wf/disj"
   (wf-frontier/disj? (promoted_i + cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/disj? cfg_tail c_2)
   ------------------- "cfg shell-freshened scope wf/disj"
   (wf-frontier/disj? (FreshenedShell c_1 cfg_tail tag_1) c)])

(define-judgment-form
  disj-lang
  #:contract (wf-cfg/disj? cfg)
  #:mode (wf-cfg/disj? I)
  [(wf-frontier/disj? cfg ())
   ----------------------- "cfg-wf/disj"
   (wf-cfg/disj? cfg)])
