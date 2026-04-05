#lang racket

(require redex/reduction-semantics
         "../languages/rail-lang.rkt"
         "./private/step-utils.rkt"
         "./search-early-red.rkt")

(provide under-ShellCtx
         rail-early-red
         step-once)

(check-redundancy #t)

(define lifted-search-early-red
  (extend-reduction-relation search-early-red rail-lang))

(define local/under-BranchCtx
  (let ([local/base
         (reduction-relation
          rail-lang
          #:domain cfg
          [--> ((in-hole FreshCtx+ (delay runnable-search_1)) <-+ search_2)
               (delay ((in-hole FreshCtx+ runnable-search_1) +-> search_2))
               "enter-right-through-scoped-delay"]
          [--> ((delay runnable-search_1) <-+ search_2)
               (delay (runnable-search_1 +-> search_2))
               "enter-right-at-branch"]
          [--> (search_2 +-> (in-hole FreshCtx+ (delay runnable-search_1)))
               (delay (search_2 <-+ (in-hole FreshCtx+ runnable-search_1)))
               "return-left-through-scoped-delay"]
          [--> (search_2 +-> (delay runnable-search_1))
               (delay (search_2 <-+ runnable-search_1))
               "return-left-at-branch"])])
    (context-closure local/base rail-lang BranchCtx)))

(define under-ShellCtx
  (let ([frontier/local-base
         (reduction-relation
          rail-lang
          #:domain cfg
          [--> (in-hole BranchCtx (search_left +-> ((answers_i <-+ search_mid) <-+ search_right)))
               (answers_i + (in-hole BranchCtx (search_left +-> (search_mid <-+ search_right))))
               "bubble-right-left-answer"]
          [--> (in-hole BranchCtx (search_left +-> (answers_i <-+ search_right)))
               (answers_i + (in-hole BranchCtx (search_left +-> search_right)))
               "promote-right-left-answer"]
          [--> (in-hole BranchCtx (search_left +-> (((in-hole FreshCtx (empty-tree)) <-+ search_mid) <-+ search_right)))
               (in-hole BranchCtx (search_left +-> (search_mid <-+ search_right)))
               "bubble-right-left-fail"]
          [--> (in-hole BranchCtx (search_left +-> ((in-hole FreshCtx (empty-tree)) <-+ search_right)))
               (in-hole BranchCtx (search_left +-> search_right))
               "skip-right-left-fail"]
          [--> (in-hole BranchCtx (search_left +-> answers_i))
               (answers_i + (in-hole BranchCtx search_left))
               "promote-right-answer"]
          [--> (in-hole BranchCtx (search_left +-> (in-hole FreshCtx (empty-tree))))
               (in-hole BranchCtx search_left)
               "skip-right-fail"])])
    (context-closure
     (union-reduction-relations frontier/local-base local/under-BranchCtx)
     rail-lang
     ShellCtx)))

(define rail-early-red
  (union-reduction-relations lifted-search-early-red under-ShellCtx))

(define (step-once prog)
  (step-once/deterministic rail-early-red prog))
