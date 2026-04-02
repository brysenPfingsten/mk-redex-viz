#lang racket

(require racket/match
         redex/reduction-semantics
         "./canonical-lang.rkt"
         "./canonical-core-wf.rkt")

(provide wf-goal/canonical?
         wf-work/canonical?
         wf-rel-env/canonical?
         wf-config/canonical?
         config-in-target-domain?
         wf-config/target?)

(check-redundancy #t)

(define (relcall-arity-ok?/host name env args)
  (for/or ([entry (in-list env)])
    (match entry
      [`(,entry-name ,d ,_)
       (and (equal? entry-name name)
            (= (length d) (length args)))]
      [_ #f])))

(define-metafunction canonical-lang
  relcall-arity-ok? : r Gamma (t ...) -> boolean
  [(relcall-arity-ok? r Gamma (t ...))
   ,(relcall-arity-ok?/host (term r)
                            (term Gamma)
                            (term (t ...)))])

(define-judgment-form
  canonical-lang
  #:contract (wf-goal/canonical? g Gamma (x_1 ...) c)
  #:mode (wf-goal/canonical? I I I I)
  [------------------ "trivial success wf/canonical"
   (wf-goal/canonical? (succeed tag) Gamma (x_1 ...) c)]
  [------------------ "trivial fail wf/canonical"
   (wf-goal/canonical? (fail tag) Gamma (x_1 ...) c)]
  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/canonical? g Gamma (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/canonical"
   (wf-goal/canonical? (∃ (x_1 ...) g tag) Gamma (x_2 ...) c)]
  [(wf-goal/canonical? g_1 Gamma (x_1 ...) c)
   (wf-goal/canonical? g_2 Gamma (x_1 ...) c)
   ------------------- "conj-wf/canonical"
   (wf-goal/canonical? (g_1 ∧ g_2 tag) Gamma (x_1 ...) c)]
  [(wf-goal/canonical? g_1 Gamma (x_1 ...) c)
   (wf-goal/canonical? g_2 Gamma (x_1 ...) c)
   ------------------- "disj-wf/canonical"
   (wf-goal/canonical? (g_1 ∨ g_2 tag) Gamma (x_1 ...) c)]
  [(wf-goal/canonical? g Gamma (x_1 ...) c)
   ------------------- "suspend-wf/canonical"
   (wf-goal/canonical? (suspend g tag) Gamma (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/canonical"
   (wf-goal/canonical? (t_1 =? t_2 tag) Gamma (x_1 ...) c)]
  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/canonical"
   (wf-goal/canonical? (t_1 != t_2 tag) Gamma (x_1 ...) c)]
  [(wf-term? t_1 (x_2 ...) c) ...
   (where #t (relcall-arity-ok? r Gamma (t_1 ...)))
   ------------------- "relcall-wf/canonical"
   (wf-goal/canonical? (r t_1 ... tag)
                       Gamma
                       (x_2 ...)
                       c)])

(define-judgment-form
  canonical-lang
  #:contract (wf-work/canonical? w Gamma c)
  #:mode (wf-work/canonical? I I I)
  [------------------- "empty tree is wf/canonical"
   (wf-work/canonical? (empty-tree) Gamma c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   ------------------- "single answer/state wf/canonical"
   (wf-work/canonical? (⊤ (state sub dis c_i trail tag)) Gamma c)]
  [(wf-state/at-scope? (state sub dis c_i trail tag) c)
   (wf-goal/canonical? g Gamma () c_i)
   ------------------- "goal/state wf/canonical"
   (wf-work/canonical? (g (state sub dis c_i trail tag)) Gamma c)]
  [(lvars-same-members? c c_i)
   (wf-work/canonical? w Gamma c_i)
   (wf-goal/canonical? g Gamma () c_i)
   ------------------- "conj wf/canonical"
   (wf-work/canonical? (w × g c_i) Gamma c)]
  [(wf-work/canonical? w Gamma c)
   ------------------- "delay wf/canonical"
   (wf-work/canonical? (delay w) Gamma c)]
  [(wf-work/canonical? w_1 Gamma c)
   (wf-work/canonical? w_2 Gamma c)
   ------------------- "branch wf/canonical"
   (wf-work/canonical? (w_1 <-+ w_2) Gamma c)])

(define-judgment-form
  canonical-lang
  #:contract (wf-rel-env/canonical? Gamma)
  #:mode (wf-rel-env/canonical? I)
  [(wf-goal/canonical? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/canonical"
   (wf-rel-env/canonical? ((r d g) ...))])

(define-judgment-form
  canonical-lang
  #:contract (wf-config/canonical? config)
  #:mode (wf-config/canonical? I)
  [(wf-rel-env/canonical? ((r d g) ...))
   (wf-work/canonical? w ((r d g) ...) ())
   ----------------------- "program-wf/canonical"
   (wf-config/canonical? (((r d g) ...) w))])

(define (config-in-target-domain? target-id canonical-config)
  (match target-id
    ["canonical/config"
     (redex-match? canonical-lang config canonical-config)]
    [_
     (error 'config-in-target-domain?
            "unsupported canonical target ~e"
            target-id)]))

(define (wf-config/target? target-id canonical-config)
  (match target-id
    ["canonical/config"
     (and (redex-match? canonical-lang config canonical-config)
          (judgment-holds (wf-config/canonical? ,canonical-config)))]
    [_
     (error 'wf-config/target?
            "unsupported canonical target ~e"
            target-id)]))
