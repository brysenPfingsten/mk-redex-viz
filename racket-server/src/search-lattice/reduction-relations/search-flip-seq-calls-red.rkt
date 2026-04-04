#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-seq-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-flip-early-relcall-extra
         search-flip-early-relcall-red
         step-once)

(check-redundancy #t)

(define search-flip-early-relcall-extra
  (reduction-relation
   search-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole BranchCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole ShellCtx
                      (in-hole BranchCtx
                               (delay (search_2
                                       <-+
                                       (in-hole FreshCtx runnable-search_1))))))
        "delay-swap-left"]))

(define search-flip-early-relcall-red
  (union-reduction-relations
   search-early-relcall-red
   search-flip-early-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-early-relcall-red prog))
