#lang racket

(require redex/reduction-semantics
         "../languages/rail-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide rail-seq-red
         step-once)

(check-redundancy #t)

(define lifted-search-base-seq-red
  (extend-reduction-relation
   search-base-seq-red
   rail-lang))

(define rail-seq-frontier/base
  (reduction-relation
   rail-lang
   #:domain cfg
   [--> (in-hole QShell (in-hole KTail (search_left +-> ((promoted_i <-+ search_mid) <-+ search_right))))
        (in-hole QShell (promoted_i + (in-hole KTail (search_left +-> (search_mid <-+ search_right)))))
        "rail-seq/bubble-right-left-answer"]
   [--> (in-hole QShell (in-hole KTail (search_left +-> (promoted_i <-+ search_right))))
        (in-hole QShell (promoted_i + (in-hole KTail (search_left +-> search_right))))
        "rail-seq/promote-right-left-answer"]
   [--> (in-hole QShell (in-hole KTail (search_left +-> (((in-hole QFresh (empty-tree)) <-+ search_mid) <-+ search_right))))
        (in-hole QShell (in-hole KTail (search_left +-> (search_mid <-+ search_right))))
        "rail-seq/bubble-right-left-fail"]
   [--> (in-hole QShell (in-hole KTail (search_left +-> ((in-hole QFresh (empty-tree)) <-+ search_right))))
        (in-hole QShell (in-hole KTail (search_left +-> search_right)))
        "rail-seq/skip-right-left-fail"]
   [--> (in-hole QShell (in-hole KTail (search_left +-> promoted_i)))
        (in-hole QShell (promoted_i + (in-hole KTail search_left)))
        "rail-seq/promote-right-observable"]
   [--> (in-hole QShell (in-hole KTail (search_left +-> (in-hole QFresh (empty-tree)))))
        (in-hole QShell (in-hole KTail search_left))
        "rail-seq/skip-right-fail"]))

(define rail-seq-local/under-QShell
  (let ([rail-seq-local/base
         (reduction-relation
          rail-lang
          #:domain cfg
          [--> (in-hole KTail ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))
               (in-hole KTail
                        (delay ((in-hole QFresh runnable-search_1) +-> search_2)))
               "rail-seq/enter-right"]
          [--> (in-hole KTail (search_2 +-> (in-hole QFresh (delay runnable-search_1))))
               (in-hole KTail
                        (delay (search_2 <-+ (in-hole QFresh runnable-search_1))))
               "rail-seq/return-left"])])
    (context-closure rail-seq-local/base rail-lang QShell)))

(define rail-seq-red
  (union-reduction-relations
   lifted-search-base-seq-red
   rail-seq-local/under-QShell
   rail-seq-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-seq-red prog))
