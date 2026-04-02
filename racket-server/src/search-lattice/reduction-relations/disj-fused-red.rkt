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
(define core-collector/disj (make-core-collector disj-fused-lang))
(define-search-cfg/one-stage core-cfg/disj core-redex/disj disj-fused-lang K)

(define disj-extra
  (reduction-relation
   disj-fused-lang
   #:domain cfg
   [--> ((in-hole K ((g_1 ∨ g_2 tag) σ)) as_1)
        ((in-hole K ((g_1 σ) <-+ (g_2 σ))) as_1)
        "disj-fused/goal-to-tree"]
   [--> ((in-hole K (((⊤ σ_new) <-+ s_rest) × g c)) as_1)
        ((in-hole K ((g σ_new) <-+ (s_rest × g c))) as_1)
        "disj-fused/continue-left-answer"]
   [--> ((in-hole K (((empty-tree) <-+ s_rest) × g c)) as_1)
        ((in-hole K (s_rest × g c)) as_1)
        "disj-fused/continue-left-fail"]
   [--> ((in-hole K (((⊤ σ_new) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole K ((⊤ σ_new) <-+ (s_mid <-+ s_right))) as_1)
        "disj-fused/bubble-left-answer"]
   [--> (((⊤ σ_new) <-+ s_right) as_1)
        (s_right ,(append-answer-host (term as_1) (term σ_new)))
        "disj-fused/promote-left-answer"]
   [--> ((in-hole K (((empty-tree) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole K ((empty-tree) <-+ (s_mid <-+ s_right))) as_1)
        "disj-fused/bubble-left-fail"]
   [--> (((empty-tree) <-+ s_right) as_1)
        (s_right as_1)
        "disj-fused/skip-left-fail"]))

(define disj-fused-red
  (union-reduction-relations
   disj-extra
   core-cfg/disj
   core-collector/disj))

(define (step-once prog)
  (step-once/deterministic disj-fused-red prog))
