#lang racket

(require redex/reduction-semantics
         "../languages/rail-seq-calls-lang.rkt"
         "./private/common.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-calls-red.rkt")

(provide rail-seq-calls-red
         step-once)

(check-redundancy #t)

(define rail-seq-calls-red
  (extend-reduction-relation
   (extend-reduction-relation search-base-seq-calls-red rail-seq-calls-lang)
   rail-seq-calls-lang
   [--> (Γ ((in-hole KDisj ((delay s_1) <-+ s_2)) as_1))
        (Γ ((in-hole KDisj (delay (s_1 +-> s_2))) as_1))
        "rail-seq-calls/enter-right"]
   [--> (Γ ((in-hole KDisj (s_2 +-> (delay s_1))) as_1))
        (Γ ((in-hole KDisj (delay (s_2 <-+ s_1))) as_1))
        "rail-seq-calls/return-left"]
   [--> (Γ ((in-hole KDisj (in-hole K (s_left +-> ((⊤ σ_new) <-+ s_right)))) as_1))
        (Γ ((in-hole KDisj (in-hole K (s_left +-> s_right)))
            ,(append-answer-host (term as_1) (term σ_new))))
        "rail-seq-calls/promote-right-left-answer"]
   [--> (Γ ((in-hole KDisj (in-hole K (s_left +-> ((empty-tree) <-+ s_right)))) as_1))
        (Γ ((in-hole KDisj (in-hole K (s_left +-> s_right))) as_1))
        "rail-seq-calls/skip-right-left-fail"]
   [--> (Γ ((in-hole KDisj (in-hole K (s_left +-> (⊤ σ_new)))) as_1))
        (Γ ((in-hole KDisj (in-hole K s_left))
            ,(append-answer-host (term as_1) (term σ_new))))
        "rail-seq-calls/promote-right-answer"]
   [--> (Γ ((in-hole KDisj (in-hole K (s_left +-> (empty-tree)))) as_1))
        (Γ ((in-hole KDisj (in-hole K s_left)) as_1))
        "rail-seq-calls/skip-right-fail"]))

(define (step-once prog)
  (step-once/deterministic rail-seq-calls-red prog))
