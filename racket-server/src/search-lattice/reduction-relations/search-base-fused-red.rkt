#lang racket

(require redex/reduction-semantics
         "../languages/search-base-fused-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/core-common.rkt"
         "./private/step-utils.rkt")

(provide search-base-fused-red
         step-once)

(check-redundancy #t)

(define core-redex/search-base-fused (extend-core-redex search-base-fused-lang))
(define core-collector/search-base-fused (make-core-collector search-base-fused-lang))
(define-search-cfg/one-stage
  core-cfg/search-base-fused
  core-redex/search-base-fused
  search-base-fused-lang
  K)

(define delay-extra
  (reduction-relation
   search-base-fused-lang
   #:domain cfg
   [--> ((in-hole K ((suspend g tag) σ)) as_1)
        ((in-hole K (delay (g σ))) as_1)
        "delay/suspend-goal"]
   [--> ((delay s_1) as_1)
        (s_1 as_1)
        "delay/invoke-delay"]
   [--> ((in-hole K ((delay s_1) × g c)) as_1)
        ((in-hole K (delay (s_1 × g c))) as_1)
        "delay/delay-through-conj"]))

(define search-extra
  (reduction-relation
   search-base-fused-lang
   #:domain cfg
   [--> ((in-hole K ((g_1 ∨ g_2 tag) σ)) as_1)
        ((in-hole K ((g_1 σ) <-+ (g_2 σ))) as_1)
        "search-base-fused/goal-to-tree"]
   [--> ((in-hole K (((⊤ σ_new) <-+ s_rest) × g c)) as_1)
        ((in-hole K ((g σ_new) <-+ (s_rest × g c))) as_1)
        "search-base-fused/continue-left-answer"]
   [--> ((in-hole K (((empty-tree) <-+ s_rest) × g c)) as_1)
        ((in-hole K (s_rest × g c)) as_1)
        "search-base-fused/continue-left-fail"]
   [--> ((in-hole K (((⊤ σ_new) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole K ((⊤ σ_new) <-+ (s_mid <-+ s_right))) as_1)
        "search-base-fused/bubble-left-answer"]
   [--> (((⊤ σ_new) <-+ s_right) as_1)
        (s_right ,(append-answer-host (term as_1) (term σ_new)))
        "search-base-fused/promote-left-answer"]
   [--> ((in-hole K (((empty-tree) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole K ((empty-tree) <-+ (s_mid <-+ s_right))) as_1)
        "search-base-fused/bubble-left-fail"]
   [--> (((empty-tree) <-+ s_right) as_1)
        (s_right as_1)
        "search-base-fused/skip-left-fail"]))

(define search-base-fused-red
  (union-reduction-relations
   core-cfg/search-base-fused
   core-collector/search-base-fused
   delay-extra
   search-extra))

(define (step-once prog)
  (step-once/deterministic search-base-fused-red prog))
