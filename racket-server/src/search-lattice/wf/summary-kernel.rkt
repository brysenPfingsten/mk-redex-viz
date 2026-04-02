#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt")

(provide summary-zero
         summary+
         summary-add-answer
         summary-add-bounced
         summary-add-tree
         summary-add-shell
         summary-answer-count/host
         summary-bounced-count/host
         summary-freshened-tree-count/host
         summary-freshened-shell-count/host
         summary-freshened-count/host)

(define-metafunction core-lang
  summary-zero : -> summary
  [(summary-zero) (wf-summary 0 0 0 0)])

(define-metafunction core-lang
  summary+ : summary summary -> summary
  [(summary+ (wf-summary number_1 number_2 number_3 number_4)
             (wf-summary number_5 number_6 number_7 number_8))
   (wf-summary ,(+ (term number_1) (term number_5))
               ,(+ (term number_2) (term number_6))
               ,(+ (term number_3) (term number_7))
               ,(+ (term number_4) (term number_8)))])

(define-metafunction core-lang
  summary-add-answer : summary -> summary
  [(summary-add-answer (wf-summary number_1 number_2 number_3 number_4))
   (wf-summary ,(add1 (term number_1))
               number_2
               number_3
               number_4)])

(define-metafunction core-lang
  summary-add-bounced : summary -> summary
  [(summary-add-bounced (wf-summary number_1 number_2 number_3 number_4))
   (wf-summary number_1
               ,(add1 (term number_2))
               number_3
               number_4)])

(define-metafunction core-lang
  summary-add-tree : summary -> summary
  [(summary-add-tree (wf-summary number_1 number_2 number_3 number_4))
   (wf-summary number_1
               number_2
               ,(add1 (term number_3))
               number_4)])

(define-metafunction core-lang
  summary-add-shell : summary -> summary
  [(summary-add-shell (wf-summary number_1 number_2 number_3 number_4))
   (wf-summary number_1
               number_2
               number_3
               ,(add1 (term number_4)))])

(define (summary-answer-count/host summary)
  (match summary
    [`(wf-summary ,answers ,_ ,_ ,_) answers]
    [_ #f]))

(define (summary-bounced-count/host summary)
  (match summary
    [`(wf-summary ,_ ,bounced ,_ ,_) bounced]
    [_ #f]))

(define (summary-freshened-tree-count/host summary)
  (match summary
    [`(wf-summary ,_ ,_ ,freshened-tree ,_) freshened-tree]
    [_ #f]))

(define (summary-freshened-shell-count/host summary)
  (match summary
    [`(wf-summary ,_ ,_ ,_ ,freshened-shell) freshened-shell]
    [_ #f]))

(define (summary-freshened-count/host summary)
  (match summary
    [`(wf-summary ,_ ,_ ,freshened-tree ,freshened-shell)
     (+ freshened-tree freshened-shell)]
    [_ #f]))
