#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-calls-red.rkt")

(provide rail-seq-calls-local/base
         rail-seq-calls-frontier/base
         rail-seq-calls-red
         step-once)

(check-redundancy #t)

(define lifted-search-base-seq-calls-red
  (extend-reduction-relation search-base-seq-calls-red rail-calls-lang))

(define rail-seq-calls-local/base
  (reduction-relation
   rail-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KTail ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole QShell
                      (in-hole KTail
                               (delay ((in-hole QFresh runnable-search_1)
                                       +->
                                       search_2)))))
        "enter-right"]
   [--> (Γ (in-hole QShell (in-hole KTail (search_2 +-> (in-hole QFresh (delay runnable-search_1))))))
        (Γ (in-hole QShell
                      (in-hole KTail
                               (delay (search_2
                                       <-+
                                       (in-hole QFresh runnable-search_1))))))
        "return-left"]))

(define rail-seq-calls-frontier/base
  (reduction-relation
   rail-calls-lang
   #:domain config
   [--> (Γ (in-hole QShell (in-hole KTail (search_left +-> ((promoted_i <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole QShell (promoted_i + (in-hole KTail (search_left +-> (search_mid <-+ search_right))))))
        "bubble-right-left-answer"]
   [--> (Γ (in-hole QShell (in-hole KTail (search_left +-> (promoted_i <-+ search_right)))))
        (Γ (in-hole QShell (promoted_i + (in-hole KTail (search_left +-> search_right)))))
        "promote-right-left-answer"]
   [--> (Γ (in-hole QShell (in-hole KTail (search_left +-> (((in-hole QFresh (empty-tree)) <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole QShell (in-hole KTail (search_left +-> (search_mid <-+ search_right)))))
        "bubble-right-left-fail"]
   [--> (Γ (in-hole QShell (in-hole KTail (search_left +-> ((in-hole QFresh (empty-tree)) <-+ search_right)))))
        (Γ (in-hole QShell (in-hole KTail (search_left +-> search_right))))
        "skip-right-left-fail"]
   [--> (Γ (in-hole QShell (in-hole KTail (search_left +-> promoted_i))))
        (Γ (in-hole QShell (promoted_i + (in-hole KTail search_left))))
        "promote-right-answer"]
   [--> (Γ (in-hole QShell (in-hole KTail (search_left +-> (in-hole QFresh (empty-tree))))))
        (Γ (in-hole QShell (in-hole KTail search_left)))
        "skip-right-fail"]))

(define rail-seq-calls-red
  (union-reduction-relations
   lifted-search-base-seq-calls-red
   rail-seq-calls-local/base
   rail-seq-calls-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-seq-calls-red prog))
