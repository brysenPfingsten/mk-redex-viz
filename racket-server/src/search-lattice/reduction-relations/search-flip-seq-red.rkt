#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-flip-early-extra
         search-flip-early-red
         step-once)

(check-redundancy #t)

(define search-flip-early-extra
  (let ([search-flip-early-extra/base
         (reduction-relation
          search-lang
          #:domain cfg
          [--> (in-hole BranchCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))
               (in-hole BranchCtx
                        (delay (search_2 <-+ (in-hole FreshCtx runnable-search_1))))
               "delay-swap-left"])])
    (context-closure search-flip-early-extra/base search-lang ShellCtx)))

(define search-flip-early-red
  (union-reduction-relations
   search-early-red
   search-flip-early-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-early-red prog))
