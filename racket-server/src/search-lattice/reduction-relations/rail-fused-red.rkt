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

(define lifted-search-base-fused-red
  (extend-reduction-relation
   search-base-fused-red
   rail-fused-lang))

(define rail-extra
  (reduction-relation
   rail-fused-lang
   #:domain f
   [--> (in-hole KScopePath (in-hole K ((delay f_1) <-+ f_2)))
        (in-hole KScopePath (in-hole K (delay (f_1 +-> f_2))))
        "rail-fused/enter-right"]
   [--> (in-hole KScopePath (in-hole K (f_2 +-> (delay f_1))))
        (in-hole KScopePath (in-hole K (delay (f_2 <-+ f_1))))
        "rail-fused/return-left"]))

(define rail-frontier-extra
  (reduction-relation
   rail-fused-lang
   #:domain cfg
   [--> (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (Freshened c_1 tag_1 (head_1 + f_right))))))
        (in-hole Q
                 (in-hole KScopePath
                          (in-hole K
                                   ((Freshened c_1 tag_1 head_1)
                                    + (f_left +-> (Freshened c_1 tag_1 f_right))))))
        "rail-fused/preserve-scoped-right-prefix"]
   [--> (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (Freshened c_1 tag_1 (head_1 <-+ f_right))))))
        (in-hole Q
                 (in-hole KScopePath
                          (in-hole K
                                   ((Freshened c_1 tag_1 head_1)
                                    + (f_left +-> (Freshened c_1 tag_1 f_right))))))
        "rail-fused/bubble-scoped-right-branch"]
   [--> (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (head_1 <-+ f_right)))))
        (in-hole Q (in-hole KScopePath (in-hole K (head_1 + (f_left +-> f_right)))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-fused/promote-right-left-head"]
   [--> (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> ((empty-tree) <-+ f_right)))))
        (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> f_right))))
        "rail-fused/skip-right-left-fail"]
   [--> (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (head_1 + f_right)))))
        (in-hole Q (in-hole KScopePath (in-hole K (head_1 + (f_left +-> f_right)))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-fused/preserve-right-prefix"]
   [--> (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> head_1))))
        (in-hole Q (in-hole KScopePath (in-hole K (head_1 + f_left))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-fused/promote-right-observable"]
   [--> (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (empty-tree)))))
        (in-hole Q (in-hole KScopePath (in-hole K f_left)))
        "rail-fused/skip-right-fail"]))

(define rail-local
  (context-closure rail-extra rail-fused-lang Q))

(define rail-fused-red
  (union-reduction-relations
   lifted-search-base-fused-red
   rail-local
   rail-frontier-extra
   ))

(define (step-once prog)
  (step-once/deterministic rail-fused-red prog))
