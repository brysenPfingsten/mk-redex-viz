#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "../languages/disj-fused-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/core-common.rkt"
         "./private/step-utils.rkt")

(provide disj-fused-red
         step-once)

(check-redundancy #t)

(define core-redex/disj (extend-core-redex disj-fused-lang))
(define-search-frontier/two-stage/no-collector
  core-frontier/disj
  core-redex/disj
  disj-fused-lang
  K
  KCorePath)

(define disj-extra
  (reduction-relation
   disj-fused-lang
   #:domain f
   [--> (in-hole KScopePath (in-hole K ((g_1 ∨ g_2 tag) σ)))
        (in-hole KScopePath (in-hole K ((g_1 σ) <-+ (g_2 σ))))
        "disj-fused/goal-to-tree"]
   [--> (in-hole KScopePath (in-hole K (((⊤ σ_new) <-+ f_rest) × g c)))
        (in-hole KScopePath (in-hole K ((g σ_new) <-+ (f_rest × g c))))
        "disj-fused/continue-left-answer"]
   [--> (in-hole KScopePath (in-hole K (((empty-tree) <-+ f_rest) × g c)))
        (in-hole KScopePath (in-hole K (f_rest × g c)))
        "disj-fused/continue-left-fail"]))

(define disj-frontier-extra
  (reduction-relation
   disj-fused-lang
   #:domain f
   [--> ((Freshened c_1 tag_1 (head_1 + f_left)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_left) <-+ f_right))
        "disj-fused/preserve-scoped-left-prefix"]
   [--> ((Freshened c_1 tag_1 (head_1 <-+ f_mid)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_mid) <-+ f_right))
        "disj-fused/bubble-scoped-left-branch"]
   [--> ((head_1 + f_left) <-+ f_right)
        (head_1 + (f_left <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "disj-fused/preserve-left-prefix"]
   [--> ((head_1 <-+ f_mid) <-+ f_right)
        (head_1 <-+ (f_mid <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "disj-fused/bubble-left-observable"]
   [--> (head_1 <-+ f_right)
        (head_1 + f_right)
        (side-condition (not (empty-freshened-head? (term head_1))))
        "disj-fused/promote-left-observable"]
   [--> (((empty-tree) <-+ f_mid) <-+ f_right)
        ((empty-tree) <-+ (f_mid <-+ f_right))
        "disj-fused/bubble-left-fail"]
   [--> ((empty-tree) <-+ f_right)
        f_right
        "disj-fused/skip-left-fail"]))

(define disj-frontier
  (context-closure disj-frontier-extra disj-fused-lang Q))

(define disj-local
  (context-closure disj-extra disj-fused-lang Q))

(define disj-fused-red
  (union-reduction-relations
   core-frontier/disj
   disj-local
   disj-frontier
   (make-core-collector disj-fused-lang)))

(define (step-once prog)
  (step-once/deterministic disj-fused-red prog))
