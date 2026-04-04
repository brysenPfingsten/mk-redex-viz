#lang racket

(require redex/reduction-semantics
         "../languages/rail-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide rail-fused-red
         step-once)

(check-redundancy #t)

(define lifted-search-base-fused-red
  (extend-reduction-relation
   search-base-fused-red
   rail-lang))

(define rail-fused-frontier/base
  (reduction-relation
   rail-lang
   #:domain cfg
   [--> (in-hole QShell (in-hole KLate (search_left +-> ((promoted_i <-+ search_mid) <-+ search_right))))
        (in-hole QShell (promoted_i + (in-hole KLate (search_left +-> (search_mid <-+ search_right)))))
        "rail-fused/bubble-right-left-answer"]
   [--> (in-hole QShell (in-hole KLate (search_left +-> (promoted_i <-+ search_right))))
        (in-hole QShell (promoted_i + (in-hole KLate (search_left +-> search_right))))
        "rail-fused/promote-right-left-answer"]
   [--> (in-hole QShell (in-hole KLate (search_left +-> (((in-hole QFresh (empty-tree)) <-+ search_mid) <-+ search_right))))
        (in-hole QShell (in-hole KLate (search_left +-> (search_mid <-+ search_right))))
        "rail-fused/bubble-right-left-fail"]
   [--> (in-hole QShell (in-hole KLate (search_left +-> ((in-hole QFresh (empty-tree)) <-+ search_right))))
        (in-hole QShell (in-hole KLate (search_left +-> search_right)))
        "rail-fused/skip-right-left-fail"]
   [--> (in-hole QShell (in-hole KLate (search_left +-> promoted_i)))
        (in-hole QShell (promoted_i + (in-hole KLate search_left)))
        "rail-fused/promote-right-observable"]
   [--> (in-hole QShell (in-hole KLate (search_left +-> (in-hole QFresh (empty-tree)))))
        (in-hole QShell (in-hole KLate search_left))
        "rail-fused/skip-right-fail"]))

(define rail-fused-local/under-QShell
  (let ([rail-fused-local/base
         (reduction-relation
          rail-lang
          #:domain cfg
          [--> (in-hole KLate ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))
               (in-hole KLate
                        (delay ((in-hole QFresh runnable-search_1) +-> search_2)))
               "rail-fused/enter-right"]
          [--> (in-hole KLate (search_2 +-> (in-hole QFresh (delay runnable-search_1))))
               (in-hole KLate
                        (delay (search_2 <-+ (in-hole QFresh runnable-search_1))))
               "rail-fused/return-left"])])
    (context-closure rail-fused-local/base rail-lang QShell)))

(define rail-fused-red
  (union-reduction-relations
   lifted-search-base-fused-red
   rail-fused-local/under-QShell
   rail-fused-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-fused-red prog))
