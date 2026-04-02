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
(define-search-frontier/two-stage/no-collector
  core-frontier/search-base-fused
  core-redex/search-base-fused
  search-base-fused-lang
  K
  KCorePath)

(define delay-local
  (reduction-relation
   search-base-fused-lang
   #:domain f
   [--> (in-hole KScopePath (in-hole K ((suspend g tag) σ)))
        (in-hole KScopePath (in-hole K (delay (g σ))))
        "delay/suspend-goal"]
   [--> (in-hole KScopePath (in-hole K ((delay f_1) × g c)))
        (in-hole KScopePath (in-hole K (delay (f_1 × g c))))
        "delay/delay-through-conj"]))

(define delay-frontier-extra
  (reduction-relation
   search-base-fused-lang
   #:domain cfg
   [--> (in-hole Q (delay f_1))
        (in-hole Q (Bounced + f_1))
        "delay/invoke-delay"]))

(define search-extra
  (reduction-relation
   search-base-fused-lang
   #:domain f
   [--> (in-hole KScopePath (in-hole K ((g_1 ∨ g_2 tag) σ)))
        (in-hole KScopePath (in-hole K ((g_1 σ) <-+ (g_2 σ))))
        "search-base-fused/goal-to-tree"]
   [--> (in-hole KScopePath (in-hole K (((⊤ σ_new) + f_rest) × g c)))
        (in-hole KScopePath (in-hole K ((g σ_new) <-+ (f_rest × g c))))
        "search-base-fused/continue-left-prefix-answer"]
   [--> (in-hole KScopePath (in-hole K (((⊤ σ_new) <-+ f_rest) × g c)))
        (in-hole KScopePath (in-hole K ((g σ_new) <-+ (f_rest × g c))))
        "search-base-fused/continue-left-answer"]
   [--> (in-hole KScopePath (in-hole K (((empty-tree) <-+ f_rest) × g c)))
        (in-hole KScopePath (in-hole K (f_rest × g c)))
        "search-base-fused/continue-left-fail"]))

(define search-frontier-extra
  (reduction-relation
   search-base-fused-lang
   #:domain f
   [--> ((Freshened c_1 tag_1 (head_1 + f_left)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_left) <-+ f_right))
        "search-base-fused/preserve-scoped-left-prefix"]
   [--> ((Freshened c_1 tag_1 (head_1 <-+ f_mid)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_mid) <-+ f_right))
        "search-base-fused/bubble-scoped-left-branch"]
   [--> ((head_1 + f_left) <-+ f_right)
        (head_1 + (f_left <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "search-base-fused/preserve-left-prefix"]
   [--> ((head_1 <-+ f_mid) <-+ f_right)
        (head_1 <-+ (f_mid <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "search-base-fused/bubble-left-observable"]
   [--> (head_1 <-+ f_right)
        (head_1 + f_right)
        (side-condition (not (empty-freshened-head? (term head_1))))
        "search-base-fused/promote-left-observable"]
   [--> (((empty-tree) <-+ f_mid) <-+ f_right)
        ((empty-tree) <-+ (f_mid <-+ f_right))
        "search-base-fused/bubble-left-fail"]
   [--> ((empty-tree) <-+ f_right)
        f_right
        "search-base-fused/skip-left-fail"]))

(define delay-extra
  (context-closure delay-local search-base-fused-lang Q))

(define search-frontier
  (context-closure search-frontier-extra search-base-fused-lang Q))

(define search-local
  (context-closure search-extra search-base-fused-lang Q))

(define search-base-fused-red
  (union-reduction-relations
   delay-frontier-extra
   core-frontier/search-base-fused
   search-local
   search-frontier
   delay-extra
   (make-core-collector search-base-fused-lang)))

(define (step-once prog)
  (step-once/deterministic search-base-fused-red prog))
