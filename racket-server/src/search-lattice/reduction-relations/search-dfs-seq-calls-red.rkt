#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-seq-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-early-relcall-extra
         search-dfs-early-relcall-red
         step-once)

(check-redundancy #t)

(define search-dfs-early-relcall-extra
  (reduction-relation
   search-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole BranchCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole ShellCtx
                      (in-hole BranchCtx
                               (delay ((in-hole FreshCtx runnable-search_1)
                                       <-+
                                       search_2)))))
        "delay-through-left"]))

(define search-dfs-early-relcall-red
  (union-reduction-relations
   search-early-relcall-red
   search-dfs-early-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-early-relcall-red prog))
