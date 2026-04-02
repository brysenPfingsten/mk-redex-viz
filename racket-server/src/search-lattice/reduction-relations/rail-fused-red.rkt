#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         "../languages/rail-fused-lang.rkt"
         "./private/common.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide rail-fused-red
         step-once)

(check-redundancy #t)

(define rail-fused-red
  (extend-reduction-relation
   (extend-reduction-relation search-base-fused-red rail-fused-lang)
   rail-fused-lang
   [--> ((in-hole K ((delay s_1) <-+ s_2)) as_1)
        ((in-hole K (delay (s_1 +-> s_2))) as_1)
        "rail-fused/enter-right"]
   [--> ((in-hole K (s_2 +-> (delay s_1))) as_1)
        ((in-hole K (delay (s_2 <-+ s_1))) as_1)
        "rail-fused/return-left"]
   [--> ((in-hole K (s_left +-> ((⊤ σ_new) <-+ s_right))) as_1)
        ((in-hole K (s_left +-> s_right))
         ,(append-answer-host (term as_1) (term σ_new)))
        "rail-fused/promote-right-left-answer"]
   [--> ((in-hole K (s_left +-> ((empty-tree) <-+ s_right))) as_1)
        ((in-hole K (s_left +-> s_right)) as_1)
        "rail-fused/skip-right-left-fail"]
   [--> ((in-hole K (s_left +-> (⊤ σ_new))) as_1)
        ((in-hole K s_left)
         ,(append-answer-host (term as_1) (term σ_new)))
        "rail-fused/promote-right-answer"]
   [--> ((in-hole K (s_left +-> (empty-tree))) as_1)
        ((in-hole K s_left) as_1)
        "rail-fused/skip-right-fail"]))

(define (step-once prog)
  (step-once/deterministic rail-fused-red prog))
