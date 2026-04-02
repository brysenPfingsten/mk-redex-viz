#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "../languages/disj-seq-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/core-common.rkt"
         "./private/step-utils.rkt")

(provide disj-seq-red
         step-once)

(check-redundancy #t)

(define core-redex/disj (extend-core-redex disj-seq-lang))
(define-search-frontier/two-stage/no-collector
  core-frontier/disj
  core-redex/disj
  disj-seq-lang
  K
  KCorePath)

(define disj-extra
  (reduction-relation
   disj-seq-lang
   #:domain f
   [--> (in-hole KScopePath (in-hole K ((g_1 ∨ g_2 tag) σ)))
        (in-hole KScopePath (in-hole K ((g_1 σ) <-+ (g_2 σ))))
        "disj-seq/goal-to-tree"]
   [--> (in-hole KScopePath (in-hole K ((f_1 <-+ f_2) × g c)))
        (in-hole KScopePath (in-hole K ((f_1 × g c) <-+ (f_2 × g c))))
        "disj-seq/distribute-over-conj"]))

(define disj-frontier-extra
  (reduction-relation
   disj-seq-lang
   #:domain f
   [--> ((Freshened c_1 tag_1 (head_1 + f_left)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_left) <-+ f_right))
        "disj-seq/preserve-scoped-left-prefix"]
   [--> ((Freshened c_1 tag_1 (head_1 <-+ f_mid)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_mid) <-+ f_right))
        "disj-seq/bubble-scoped-left-branch"]
   [--> ((head_1 + f_left) <-+ f_right)
        (head_1 + (f_left <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "disj-seq/preserve-left-prefix"]
   [--> ((head_1 <-+ f_mid) <-+ f_right)
        (head_1 <-+ (f_mid <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "disj-seq/bubble-left-observable"]
   [--> (head_1 <-+ f_right)
        (head_1 + f_right)
        (side-condition (not (empty-freshened-head? (term head_1))))
        "disj-seq/promote-left-observable"]
   [--> (((empty-tree) <-+ f_mid) <-+ f_right)
        ((empty-tree) <-+ (f_mid <-+ f_right))
        "disj-seq/bubble-left-fail"]
   [--> ((empty-tree) <-+ f_right)
        f_right
        "disj-seq/skip-left-fail"]))

(define disj-frontier
  (context-closure disj-frontier-extra disj-seq-lang Q))

(define disj-local
  (context-closure disj-extra disj-seq-lang Q))

(define disj-seq-red
  (union-reduction-relations
   core-frontier/disj
   disj-local
   disj-frontier
   (make-core-collector disj-seq-lang)))

(define (step-once prog)
  (step-once/deterministic disj-seq-red prog))
