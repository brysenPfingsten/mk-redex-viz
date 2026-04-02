#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         (only-in "../languages/core-lang.rkt" c-append)
         (rename-in "./core-wf.rkt"
                    [wf-goal/core? wf-goal/core/base])
         "./core-wf.rkt")

(provide wf-goal/delay?
         wf-work/delay?
         wf-resolved/delay?
         wf-search/delay?
         wf-frontier/delay?
         wf-cfg/delay?)

(check-redundancy #t)

(define-extended-judgment-form
  delay-lang
  wf-goal/core/base
  #:contract (wf-goal/delay? g (x_1 ...) c)
  #:mode (wf-goal/delay? I I I)
  [(wf-goal/delay? g (x_1 ...) c)
   ------------------- "delay-goal-wf/delay"
   (wf-goal/delay? (suspend g tag) (x_1 ...) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-resolved/delay? search c)
  #:mode (wf-resolved/delay? I I)
  [------------------- "empty frontier residual is wf/delay"
   (wf-resolved/delay? (empty-tree) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "raw answer/state wf/delay"
   (wf-resolved/delay? (⊤ (state sub dis c_i trail tag)) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-resolved/delay? search_tail c_2)
   ------------------- "resolved tree-freshened scope wf/delay"
   (wf-resolved/delay? (FreshenedTree c_1 search_tail tag_1) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-work/delay? runnable-search c)
  #:mode (wf-work/delay? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-work/delay? runnable-search_tail c_2)
   ------------------- "work tree-freshened scope wf/delay"
   (wf-work/delay? (FreshenedTree c_1 runnable-search_tail tag_1) c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/delay? g () c_i)
   ------------------- "goal/state wf/delay"
   (wf-work/delay? (g (state sub dis c_i trail tag)) c)]
  [(lvars-same-members? c c_i)
   (wf-search/delay? search_i c_i)
   (wf-goal/delay? g () c_i)
   ------------------- "conj wf/delay"
   (wf-work/delay? (search_i × g c_i) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-search/delay? search c)
  #:mode (wf-search/delay? I I)
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-search/delay? search_tail c_2)
   ------------------- "search tree-freshened scope wf/delay"
   (wf-search/delay? (FreshenedTree c_1 search_tail tag_1) c)]
  [(wf-resolved/delay? search_i c)
   ------------------- "resolved search wf/delay"
   (wf-search/delay? search_i c)]
  [(wf-work/delay? runnable-search_i c)
   ------------------- "work search wf/delay"
   (wf-search/delay? runnable-search_i c)]
  [(wf-work/delay? runnable-search_i c)
   ------------------- "delay search wf/delay"
   (wf-search/delay? (delay runnable-search_i) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-frontier/delay? cfg c)
  #:mode (wf-frontier/delay? I I)
  [(wf-search/delay? search_i c)
   ------------------- "search frontier wf/delay"
   (wf-frontier/delay? search_i c)]
  [(wf-search/delay? search_i c)
   ------------------- "bounced search frontier wf/delay"
   (wf-frontier/delay? (Bounced search_i) c)]
  [(wf-frontier/delay? cfg_tail c)
   ------------------- "bounced frontier wf/delay"
   (wf-frontier/delay? (Bounced cfg_tail) c)]
  [(lvars-fresh-extension? c_1 c)
   (where c_2 (c-append c_1 c))
   (wf-frontier/delay? cfg_tail c_2)
   ------------------- "cfg shell-freshened scope wf/delay"
   (wf-frontier/delay? (FreshenedShell c_1 cfg_tail tag_1) c)])

(define-judgment-form
  delay-lang
  #:contract (wf-cfg/delay? cfg)
  #:mode (wf-cfg/delay? I)
  [(wf-frontier/delay? cfg ())
   ----------------------- "cfg-wf/delay"
   (wf-cfg/delay? cfg)])
