#lang racket

(require redex/reduction-semantics
         "./core-lang.rkt")

(provide disj-lang)

(check-redundancy #t)

;; Neutral disjunction syntax with no hoist policy baked into contexts.
(define-extended-language disj-lang core-lang
  [promoted cell
            (FreshenedTree c promoted tag)]
  [g ....
     (g ∨ g tag)]
  [cfg ....
       (promoted + cfg)]
  ;; First committed shell context on the disjunction branch.
  ;; The `+` spine marks commitment; promoted payloads themselves remain
  ;; tree-freshened, not shell-freshened.
  ;; First divergent layer: L2/disj.
  ;; Allowed extension direction: add shell constructors only.
  [QShell ::= hole
              (FreshenedShell c QShell tag)
              (promoted + QShell)]
  ;; Shared left-branch zipper for neutral L2. Both seq and fused reuse it.
  [KBranch ::= hole
               (FreshenedTree c KBranch tag)
               (KBranch <-+ search)]
  ;; Shared late-strength path helper. The seq/fused difference now lives in
  ;; the reducers: seq restricts itself to immediate hoist at the exposed
  ;; boundary, while fused uses the larger helper for descent-first behavior.
  [KLate ::= KBranch
             (KLate × g c)]
  [runnable-root ....
                 (search <-+ search)])
