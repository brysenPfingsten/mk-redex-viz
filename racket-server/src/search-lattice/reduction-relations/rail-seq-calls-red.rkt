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
   search-base-seq-calls-red
   rail-seq-calls-lang
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K ((delay f_1) <-+ f_2)))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (delay (f_1 +-> f_2))))))
        "rail-seq-calls/enter-right"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_2 +-> (delay f_1))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (delay (f_2 <-+ f_1))))))
        "rail-seq-calls/return-left"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (head_1 <-+ f_right))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (head_1 + (f_left +-> f_right))))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-seq-calls/promote-right-left-head"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> ((empty-tree) <-+ f_right))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> f_right)))))
        "rail-seq-calls/skip-right-left-fail"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (Freshened c_1 tag_1 (head_1 + f_right)))))))
        (Γ
         (in-hole Q
                  (in-hole KScopePath
                           (in-hole K
                                    ((Freshened c_1 tag_1 head_1)
                                     + (f_left +-> (Freshened c_1 tag_1 f_right)))))))
        "rail-seq-calls/preserve-scoped-right-prefix"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (Freshened c_1 tag_1 (head_1 <-+ f_right)))))))
        (Γ
         (in-hole Q
                  (in-hole KScopePath
                           (in-hole K
                                    ((Freshened c_1 tag_1 head_1)
                                     + (f_left +-> (Freshened c_1 tag_1 f_right)))))))
        "rail-seq-calls/bubble-scoped-right-branch"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (head_1 + f_right))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (head_1 + (f_left +-> f_right))))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-seq-calls/preserve-right-prefix"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> head_1)))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K (head_1 + f_left)))))
        (side-condition (not (empty-freshened-head? (term head_1))))
        "rail-seq-calls/promote-right-observable"]
   [--> (Γ (in-hole Q (in-hole KScopePath (in-hole K (f_left +-> (empty-tree))))))
        (Γ (in-hole Q (in-hole KScopePath (in-hole K f_left))))
        "rail-seq-calls/skip-right-fail"]))

(define (step-once prog)
  (step-once/deterministic rail-seq-calls-red prog))
