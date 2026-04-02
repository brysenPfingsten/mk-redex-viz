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
   search-base-fused-calls-red
   rail-fused-calls-lang
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K ((delay f_1) <-+ f_2)))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (delay (f_1 +-> f_2))))))
        "rail-fused-calls/enter-right"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_2 +-> (delay f_1))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (delay (f_2 <-+ f_1))))))
        "rail-fused-calls/return-left"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (head_1 <-+ f_right))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (head_1 + (f_left +-> f_right))))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-fused-calls/promote-right-left-head"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> ((empty-tree) <-+ f_right))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> f_right)))))
        "rail-fused-calls/skip-right-left-fail"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (Freshened c_1 tag_1 (head_1 + f_right)))))))
        (Γ
         (in-hole Q
                  (in-hole KScopePath
                           (in-hole K
                                    ((Freshened c_1 tag_1 head_1)
                                     + (f_left +-> (Freshened c_1 tag_1 f_right)))))))
        "rail-fused-calls/preserve-scoped-right-prefix"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (Freshened c_1 tag_1 (head_1 <-+ f_right)))))))
        (Γ
         (in-hole Q
                  (in-hole KScopePath
                           (in-hole K
                                    ((Freshened c_1 tag_1 head_1)
                                     + (f_left +-> (Freshened c_1 tag_1 f_right)))))))
        "rail-fused-calls/bubble-scoped-right-branch"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (head_1 + f_right))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (head_1 + (f_left +-> f_right))))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-fused-calls/preserve-right-prefix"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> head_1)))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (head_1 + f_left)))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-fused-calls/promote-right-observable"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (empty-tree))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K f_left))))
        "rail-fused-calls/skip-right-fail"]))

(define (step-once prog)
  (step-once/deterministic rail-fused-calls-red prog))
