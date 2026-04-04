#lang racket

(require redex/reduction-semantics
         "./core-lang.rkt")

(provide disj-lang)

(check-redundancy #t)

;; Neutral disjunction syntax with no hoist policy baked into contexts.
(define-extended-language disj-lang core-lang
  [answers answer
            (ScopedTree c answers tag)]
  [settled (in-hole FreshCtx (⊤ σ))
           (in-hole FreshCtx (empty-tree))]
  [g ....
     (g ∨ g tag)]
  [cfg ....
       (answers + cfg)]
  ;; First committed shell context on the disjunction branch.
  ;; The `+` spine marks commitment; answers payloads themselves remain
  ;; tree-freshened, not shell-freshened.
  ;; First divergent layer: L2/disj.
  ;; Allowed extension direction: add shell constructors only.
  [ShellCtx ::= hole
              (ScopedShell c ShellCtx tag)
              (answers + ShellCtx)]
  ;; Shared left-branch zipper for neutral L2. Both early and late reuse it.
  [BranchCtx ::= hole
               (ScopedTree c BranchCtx tag)
               (BranchCtx <-+ search)]
  ;; Shared late-strength path helper. The early/late difference now lives in
  ;; the reducers: early restricts itself to immediate hoist at the exposed
  ;; boundary, while late uses the larger helper for descent-first behavior.
  [LateCtx ::= BranchCtx
             (ScopedTree c (LateCtx × g c) tag)
             (LateCtx × g c)]
  [runnable-root ....
                 (search <-+ search)])
