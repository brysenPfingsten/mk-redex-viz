#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-calls-red.rkt")

(provide rail-fused-calls-local/base
         rail-fused-calls-frontier/base
         rail-fused-calls-red
         step-once)

(check-redundancy #t)

(define lifted-search-base-fused-calls-red
  (extend-reduction-relation
   search-base-fused-calls-red
   rail-calls-lang))

(define rail-fused-calls-local/base
  (reduction-relation
   rail-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KLate ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole QShell
                      (in-hole KLate
                               (delay ((in-hole QFresh runnable-search_1)
                                       +->
                                       search_2)))))
        "enter-right"]
   [--> (Γ (in-hole QShell (in-hole KLate (search_2 +-> (in-hole QFresh (delay runnable-search_1))))))
        (Γ (in-hole QShell
                      (in-hole KLate
                               (delay (search_2
                                       <-+
                                       (in-hole QFresh runnable-search_1))))))
        "return-left"]))

(define rail-fused-calls-frontier/base
  (reduction-relation
   rail-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KLate (search_left +-> ((promoted_i <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole QShell (promoted_i + (in-hole KLate (search_left +-> (search_mid <-+ search_right))))))
        "bubble-right-left-answer"]
   [--> (Γ (in-hole QShell (in-hole KLate (search_left +-> (promoted_i <-+ search_right)))))
        (Γ (in-hole QShell (promoted_i + (in-hole KLate (search_left +-> search_right)))))
        "promote-right-left-answer"]
   [--> (Γ (in-hole QShell (in-hole KLate (search_left +-> (((in-hole QFresh (empty-tree)) <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole QShell (in-hole KLate (search_left +-> (search_mid <-+ search_right)))))
        "bubble-right-left-fail"]
   [--> (Γ (in-hole QShell (in-hole KLate (search_left +-> ((in-hole QFresh (empty-tree)) <-+ search_right)))))
        (Γ (in-hole QShell (in-hole KLate (search_left +-> search_right))))
        "skip-right-left-fail"]
   [--> (Γ (in-hole QShell (in-hole KLate (search_left +-> promoted_i))))
        (Γ (in-hole QShell (promoted_i + (in-hole KLate search_left))))
        "promote-right-answer"]
   [--> (Γ (in-hole QShell (in-hole KLate (search_left +-> (in-hole QFresh (empty-tree))))))
        (Γ (in-hole QShell (in-hole KLate search_left)))
        "skip-right-fail"]))

(define rail-fused-calls-red
  (union-reduction-relations
   lifted-search-base-fused-calls-red
   rail-fused-calls-local/base
   rail-fused-calls-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-fused-calls-red prog))
