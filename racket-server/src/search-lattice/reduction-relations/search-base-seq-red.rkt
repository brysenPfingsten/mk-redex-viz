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
(define-search-frontier/two-stage/no-collector
  core-frontier/search-base-seq
  core-redex/search-base-seq
  search-base-seq-lang
  K
  KCorePath)

(define delay-local
  (reduction-relation
   search-base-seq-lang
   #:domain f
   [--> (in-hole KScopePath (in-hole K ((suspend g tag) σ)))
        (in-hole KScopePath (in-hole K (delay (g σ))))
        "delay/suspend-goal"]
   [--> (in-hole KScopePath (in-hole K ((delay f_1) × g c)))
        (in-hole KScopePath (in-hole K (delay (f_1 × g c))))
        "delay/delay-through-conj"]))

(define delay-frontier-extra
  (reduction-relation
   search-base-seq-lang
   #:domain cfg
   [--> (in-hole Q (delay f_1))
        (in-hole Q (Bounced + f_1))
        "delay/invoke-delay"]))

(define search-extra
  (reduction-relation
   search-base-seq-lang
   #:domain f
   [--> (in-hole KScopePath (in-hole K ((g_1 ∨ g_2 tag) σ)))
        (in-hole KScopePath (in-hole K ((g_1 σ) <-+ (g_2 σ))))
        "search-base-seq/goal-to-tree"]
   [--> (in-hole KScopePath (in-hole K (((⊤ σ_new) + f_rest) × g c)))
        (in-hole KScopePath (in-hole K ((g σ_new) <-+ (f_rest × g c))))
        "search-base-seq/continue-left-prefix-answer"]
   [--> (in-hole KScopePath (in-hole K ((f_1 <-+ f_2) × g c)))
        (in-hole KScopePath (in-hole K ((f_1 × g c) <-+ (f_2 × g c))))
        "search-base-seq/distribute-over-conj"]))

(define search-frontier-extra
  (reduction-relation
   search-base-seq-lang
   #:domain f
   [--> ((Freshened c_1 tag_1 (head_1 + f_left)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_left) <-+ f_right))
        "search-base-seq/preserve-scoped-left-prefix"]
   [--> ((Freshened c_1 tag_1 (head_1 <-+ f_mid)) <-+ f_right)
        ((Freshened c_1 tag_1 head_1)
         + ((Freshened c_1 tag_1 f_mid) <-+ f_right))
        "search-base-seq/bubble-scoped-left-branch"]
   [--> ((head_1 + f_left) <-+ f_right)
        (head_1 + (f_left <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "search-base-seq/preserve-left-prefix"]
   [--> ((head_1 <-+ f_mid) <-+ f_right)
        (head_1 <-+ (f_mid <-+ f_right))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "search-base-seq/bubble-left-observable"]
   [--> (head_1 <-+ f_right)
        (head_1 + f_right)
        (side-condition (not (empty-freshened-head? (term head_1))))
        "search-base-seq/promote-left-observable"]
   [--> (((empty-tree) <-+ f_mid) <-+ f_right)
        ((empty-tree) <-+ (f_mid <-+ f_right))
        "search-base-seq/bubble-left-fail"]
   [--> ((empty-tree) <-+ f_right)
        f_right
        "search-base-seq/skip-left-fail"]))

(define delay-extra
  (context-closure delay-local search-base-seq-lang Q))

(define search-frontier
  (context-closure search-frontier-extra search-base-seq-lang Q))

(define search-local
  (context-closure search-extra search-base-seq-lang Q))

(define search-base-seq-red
  (union-reduction-relations
   delay-frontier-extra
   core-frontier/search-base-seq
   search-local
   search-frontier
   delay-extra
   (make-core-collector search-base-seq-lang)))

(define (step-once prog)
  (step-once/deterministic search-base-seq-red prog))
