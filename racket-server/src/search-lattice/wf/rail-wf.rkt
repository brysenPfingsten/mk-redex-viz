#lang racket

(require redex/reduction-semantics
         "../languages/rail-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./search-base-wf.rkt"
                    [wf-goal/search-base? wf-goal/search-base/base]
                    [wf-promoted/search-base? wf-promoted/search-base/base])
         "./core-wf.rkt")

(provide wf-goal/rail?
         wf-work/rail?
         wf-resolved/rail?
         wf-search/rail?
         wf-promoted/rail?
         wf-frontier/rail?
         wf-cfg/rail?)

(check-redundancy #t)

(define-extended-judgment-form
  rail-lang
  wf-goal/search-base/base
  #:contract (wf-goal/rail? g (x_1 ...) c)
  #:mode (wf-goal/rail? I I I))

(define-judgment-form
  rail-lang
  #:contract (wf-resolved/rail? search c)
  #:mode (wf-resolved/rail? I I)
  [------------------- "empty frontier residual is wf/rail"
   (wf-resolved/rail? (empty-tree) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/rail"
   (wf-resolved/rail? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/rail? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/rail"
   (wf-resolved/rail? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  rail-lang
  #:contract (wf-work/rail? search c)
  #:mode (wf-work/rail? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/rail? search_tail c_2)
   ------------------- "work tree-freshened scope wf/rail"
   (wf-work/rail? (FreshenedTree c_1 search_tail tag_1) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/rail? g () c_i)
   ------------------- "goal/state wf/rail"
   (wf-work/rail? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-search/rail? search_i c_i)
   (wf-goal/rail? g () c_i)
   ------------------- "conj wf/rail"
   (wf-work/rail? (search_i × g c_i) c)]
  [(wf-search/rail? search_1 c)
   (wf-search/rail? search_2 c)
   ------------------- "left disj wf/rail"
   (wf-work/rail? (search_1 <-+ search_2) c)]
  [(wf-search/rail? search_1 c)
   (wf-search/rail? search_2 c)
   ------------------- "right disj wf/rail"
   (wf-work/rail? (search_1 +-> search_2) c)])

(define-judgment-form
  rail-lang
  #:contract (wf-search/rail? search c)
  #:mode (wf-search/rail? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/rail? search_tail c_2)
   ------------------- "search tree-freshened scope wf/rail"
   (wf-search/rail? (FreshenedTree c_1 search_tail tag_1) c)]
  [(wf-resolved/rail? search_i c)
   ------------------- "resolved search wf/rail"
   (wf-search/rail? search_i c)]
  [(wf-work/rail? search_i c)
   ------------------- "work search wf/rail"
   (wf-search/rail? search_i c)]
  [(wf-work/rail? search_i c)
   ------------------- "delay search wf/rail"
   (wf-search/rail? (delay search_i) c)])

(define-extended-judgment-form
  rail-lang
  wf-promoted/search-base/base
  #:contract (wf-promoted/rail? promoted c)
  #:mode (wf-promoted/rail? I I))

(define-judgment-form
  rail-lang
  #:contract (wf-frontier/rail? cfg c)
  #:mode (wf-frontier/rail? I I)
  [(wf-search/rail? search_i c)
   ------------------- "search frontier wf/rail"
   (wf-frontier/rail? search_i c)]
  [(wf-search/rail? search_i c)
   ------------------- "bounced search frontier wf/rail"
   (wf-frontier/rail? (Bounced search_i) c)]
  [(wf-promoted/rail? promoted_i c)
   (wf-frontier/rail? cfg_tail c)
   ------------------- "promoted stream node wf/rail"
   (wf-frontier/rail? (promoted_i + cfg_tail) c)]
  [(wf-frontier/rail? cfg_tail c)
   ------------------- "bounced frontier wf/rail"
   (wf-frontier/rail? (Bounced cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/rail? cfg_tail c_2)
   ------------------- "cfg shell-freshened scope wf/rail"
   (wf-frontier/rail? (FreshenedShell c_1 cfg_tail tag_1) c)])

(define-judgment-form
  rail-lang
  #:contract (wf-cfg/rail? cfg)
  #:mode (wf-cfg/rail? I)
  [(wf-frontier/rail? cfg ())
   ----------------------- "cfg-wf/rail"
   (wf-cfg/rail? cfg)])
