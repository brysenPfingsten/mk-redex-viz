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
(define core-collector/disj (make-core-collector disj-seq-lang))
(define-search-cfg/two-stage core-cfg/disj core-redex/disj disj-seq-lang K KDisj)

(define disj-extra
  (reduction-relation
   disj-seq-lang
   #:domain cfg
   [--> ((in-hole KDisj (in-hole K ((g_1 ∨ g_2 tag) σ))) as_1)
        ((in-hole KDisj (in-hole K ((g_1 σ) <-+ (g_2 σ)))) as_1)
        "disj-seq/goal-to-tree"]
   [--> ((in-hole KDisj (in-hole K ((s_1 <-+ s_2) × g c))) as_1)
        ((in-hole KDisj (in-hole K ((s_1 × g c) <-+ (s_2 × g c)))) as_1)
        "disj-seq/distribute-over-conj"]
   [--> ((in-hole KDisj (((⊤ σ_new) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole KDisj ((⊤ σ_new) <-+ (s_mid <-+ s_right))) as_1)
        "disj-seq/bubble-left-answer"]
   [--> (((⊤ σ_new) <-+ s_right) as_1)
        (s_right ,(append-answer-host (term as_1) (term σ_new)))
        "disj-seq/promote-left-answer"]
   [--> ((in-hole KDisj (((empty-tree) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole KDisj ((empty-tree) <-+ (s_mid <-+ s_right))) as_1)
        "disj-seq/bubble-left-fail"]
   [--> (((empty-tree) <-+ s_right) as_1)
        (s_right as_1)
        "disj-seq/skip-left-fail"]))

(define disj-seq-red
  (union-reduction-relations
   disj-extra
   core-cfg/disj
   core-collector/disj))

(define (step-once prog)
  (step-once/deterministic disj-seq-red prog))
