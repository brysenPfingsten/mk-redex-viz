#lang racket

(require redex/reduction-semantics
         "../languages/rail-fused-calls-lang.rkt"
         "./private/common.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-calls-red.rkt")

(provide rail-fused-calls-red
         step-once)

(check-redundancy #t)

(define rail-fused-calls-red
  (extend-reduction-relation
   (extend-reduction-relation search-base-fused-calls-red rail-fused-calls-lang)
   rail-fused-calls-lang
   [--> (Γ ((in-hole K ((delay s_1) <-+ s_2)) as_1))
        (Γ ((in-hole K (delay (s_1 +-> s_2))) as_1))
        "rail-fused-calls/enter-right"]
   [--> (Γ ((in-hole K (s_2 +-> (delay s_1))) as_1))
        (Γ ((in-hole K (delay (s_2 <-+ s_1))) as_1))
        "rail-fused-calls/return-left"]
   [--> (Γ ((in-hole K (s_left +-> ((⊤ σ_new) <-+ s_right))) as_1))
        (Γ ((in-hole K (s_left +-> s_right))
            ,(append-answer-host (term as_1) (term σ_new))))
        "rail-fused-calls/promote-right-left-answer"]
   [--> (Γ ((in-hole K (s_left +-> ((empty-tree) <-+ s_right))) as_1))
        (Γ ((in-hole K (s_left +-> s_right)) as_1))
        "rail-fused-calls/skip-right-left-fail"]
   [--> (Γ ((in-hole K (s_left +-> (⊤ σ_new))) as_1))
        (Γ ((in-hole K s_left)
            ,(append-answer-host (term as_1) (term σ_new))))
        "rail-fused-calls/promote-right-answer"]
   [--> (Γ ((in-hole K (s_left +-> (empty-tree))) as_1))
        (Γ ((in-hole K s_left) as_1))
        "rail-fused-calls/skip-right-fail"]))

(define (step-once prog)
  (step-once/deterministic rail-fused-calls-red prog))
