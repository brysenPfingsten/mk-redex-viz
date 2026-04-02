#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./disj-wf.rkt"
                    [wf-goal/disj? wf-goal/disj/base]
                    [wf-promoted/disj? wf-promoted/disj/base])
         "./core-wf.rkt")

(provide wf-goal/search-base?
         wf-work/search-base?
         wf-resolved/search-base?
         wf-search/search-base?
         wf-promoted/search-base?
         wf-frontier/search-base?
         wf-cfg/search-base?)

(check-redundancy #t)

(define-extended-judgment-form
  search-base-lang
  wf-goal/disj/base
  #:contract (wf-goal/search-base? g (x_1 ...) c)
  #:mode (wf-goal/search-base? I I I)
  [(wf-goal/search-base? g (x_1 ...) c)
   ------------------- "delay-goal-wf/search-base"
   (wf-goal/search-base? (suspend g tag) (x_1 ...) c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-resolved/search-base? search c)
  #:mode (wf-resolved/search-base? I I)
  [------------------- "empty frontier residual is wf/search-base"
   (wf-resolved/search-base? (empty-tree) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/search-base"
   (wf-resolved/search-base? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/search-base? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/search-base"
   (wf-resolved/search-base? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-work/search-base? runnable-search c)
  #:mode (wf-work/search-base? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/search-base? runnable-search_tail c_2)
   ------------------- "work tree-freshened scope wf/search-base"
   (wf-work/search-base? (FreshenedTree c_1 runnable-search_tail tag_1) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/search-base? g () c_i)
   ------------------- "goal/state wf/search-base"
   (wf-work/search-base? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-search/search-base? search_i c_i)
   (wf-goal/search-base? g () c_i)
   ------------------- "conj wf/search-base"
   (wf-work/search-base? (search_i × g c_i) c)]
  [(wf-search/search-base? search_1 c)
   (wf-search/search-base? search_2 c)
   ------------------- "disj wf/search-base"
   (wf-work/search-base? (search_1 <-+ search_2) c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-search/search-base? search c)
  #:mode (wf-search/search-base? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/search-base? search_tail c_2)
   ------------------- "search tree-freshened scope wf/search-base"
   (wf-search/search-base? (FreshenedTree c_1 search_tail tag_1) c)]
  [(wf-resolved/search-base? search_i c)
   ------------------- "resolved search wf/search-base"
   (wf-search/search-base? search_i c)]
  [(wf-work/search-base? runnable-search_i c)
   ------------------- "work search wf/search-base"
   (wf-search/search-base? runnable-search_i c)]
  [(wf-work/search-base? runnable-search_i c)
   ------------------- "delay search wf/search-base"
   (wf-search/search-base? (delay runnable-search_i) c)])

(define-extended-judgment-form
  search-base-lang
  wf-promoted/disj/base
  #:contract (wf-promoted/search-base? promoted c)
  #:mode (wf-promoted/search-base? I I))

(define-judgment-form
  search-base-lang
  #:contract (wf-frontier/search-base? cfg c)
  #:mode (wf-frontier/search-base? I I)
  [(wf-search/search-base? search_i c)
   ------------------- "search frontier wf/search-base"
   (wf-frontier/search-base? search_i c)]
  [(wf-search/search-base? search_i c)
   ------------------- "bounced search frontier wf/search-base"
   (wf-frontier/search-base? (Bounced search_i) c)]
  [(wf-promoted/search-base? promoted_i c)
   (wf-frontier/search-base? cfg_tail c)
   ------------------- "promoted stream node wf/search-base"
   (wf-frontier/search-base? (promoted_i + cfg_tail) c)]
  [(wf-frontier/search-base? cfg_tail c)
   ------------------- "bounced frontier wf/search-base"
   (wf-frontier/search-base? (Bounced cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/search-base? cfg_tail c_2)
   ------------------- "cfg shell-freshened scope wf/search-base"
   (wf-frontier/search-base? (FreshenedShell c_1 cfg_tail tag_1) c)])

(define-judgment-form
  search-base-lang
  #:contract (wf-cfg/search-base? cfg)
  #:mode (wf-cfg/search-base? I)
  [(wf-frontier/search-base? cfg ())
   ----------------------- "cfg-wf/search-base"
   (wf-cfg/search-base? cfg)])
