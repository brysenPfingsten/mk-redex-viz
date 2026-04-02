#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "../languages/rail-seq-lang.rkt"
         "./private/common.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide rail-seq-red
         step-once)

(check-redundancy #t)

(define rail-seq-red
  (extend-reduction-relation
   (extend-reduction-relation search-base-seq-red rail-seq-lang)
   rail-seq-lang
   [--> ((in-hole KDisj ((delay s_1) <-+ s_2)) as_1)
        ((in-hole KDisj (delay (s_1 +-> s_2))) as_1)
        "rail-seq/enter-right"]
   [--> ((in-hole KDisj (s_2 +-> (delay s_1))) as_1)
        ((in-hole KDisj (delay (s_2 <-+ s_1))) as_1)
        "rail-seq/return-left"]
   [--> ((in-hole KDisj (in-hole K (s_left +-> ((⊤ σ_new) <-+ s_right)))) as_1)
        ((in-hole KDisj (in-hole K (s_left +-> s_right)))
         ,(append-answer-host (term as_1) (term σ_new)))
        "rail-seq/promote-right-left-answer"]
   [--> ((in-hole KDisj (in-hole K (s_left +-> ((empty-tree) <-+ s_right)))) as_1)
        ((in-hole KDisj (in-hole K (s_left +-> s_right))) as_1)
        "rail-seq/skip-right-left-fail"]
   [--> ((in-hole KDisj (in-hole K (s_left +-> (⊤ σ_new)))) as_1)
        ((in-hole KDisj (in-hole K s_left))
         ,(append-answer-host (term as_1) (term σ_new)))
        "rail-seq/promote-right-answer"]
   [--> ((in-hole KDisj (in-hole K (s_left +-> (empty-tree)))) as_1)
        ((in-hole KDisj (in-hole K s_left)) as_1)
        "rail-seq/skip-right-fail"]))

(define (step-once prog)
  (step-once/deterministic rail-seq-red prog))
