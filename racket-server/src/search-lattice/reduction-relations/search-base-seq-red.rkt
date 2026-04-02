#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-lang.rkt"
         "./private/common.rkt"
         "./private/context-pipeline.rkt"
         "./private/core-common.rkt"
         "./private/step-utils.rkt")

(provide search-base-seq-red
         step-once)

(check-redundancy #t)

(define core-redex/search-base-seq (extend-core-redex search-base-seq-lang))
(define core-collector/search-base-seq (make-core-collector search-base-seq-lang))
(define-search-cfg/two-stage
  core-cfg/search-base-seq
  core-redex/search-base-seq
  search-base-seq-lang
  K
  KDisj)

(define delay-extra
  (reduction-relation
   search-base-seq-lang
   #:domain cfg
   [--> ((in-hole KDisj (in-hole K ((suspend g tag) σ))) as_1)
        ((in-hole KDisj (in-hole K (delay (g σ)))) as_1)
        "delay/suspend-goal"]
   [--> ((delay s_1) as_1)
        (s_1 as_1)
        "delay/invoke-delay"]
   [--> ((in-hole KDisj (in-hole K ((delay s_1) × g c))) as_1)
        ((in-hole KDisj (in-hole K (delay (s_1 × g c)))) as_1)
        "delay/delay-through-conj"]))

(define search-extra
  (reduction-relation
   search-base-seq-lang
   #:domain cfg
   [--> ((in-hole KDisj (in-hole K ((g_1 ∨ g_2 tag) σ))) as_1)
        ((in-hole KDisj (in-hole K ((g_1 σ) <-+ (g_2 σ)))) as_1)
        "search-base-seq/goal-to-tree"]
   [--> ((in-hole KDisj (in-hole K ((s_1 <-+ s_2) × g c))) as_1)
        ((in-hole KDisj (in-hole K ((s_1 × g c) <-+ (s_2 × g c)))) as_1)
        "search-base-seq/distribute-over-conj"]
   [--> ((in-hole KDisj (((⊤ σ_new) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole KDisj ((⊤ σ_new) <-+ (s_mid <-+ s_right))) as_1)
        "search-base-seq/bubble-left-answer"]
   [--> (((⊤ σ_new) <-+ s_right) as_1)
        (s_right ,(append-answer-host (term as_1) (term σ_new)))
        "search-base-seq/promote-left-answer"]
   [--> ((in-hole KDisj (((empty-tree) <-+ s_mid) <-+ s_right)) as_1)
        ((in-hole KDisj ((empty-tree) <-+ (s_mid <-+ s_right))) as_1)
        "search-base-seq/bubble-left-fail"]
   [--> (((empty-tree) <-+ s_right) as_1)
        (s_right as_1)
        "search-base-seq/skip-left-fail"]))

(define search-base-seq-red
  (union-reduction-relations
   core-cfg/search-base-seq
   core-collector/search-base-seq
   delay-extra
   search-extra))

(define (step-once prog)
  (step-once/deterministic search-base-seq-red prog))
