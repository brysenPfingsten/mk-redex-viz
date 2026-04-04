#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-fused-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-dfs-late-relcall-extra
         search-dfs-late-relcall-red
         step-once)

(check-redundancy #t)

(define search-dfs-late-relcall-extra
  (reduction-relation
   search-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole ShellCtx
                      (in-hole LateCtx
                               (delay ((in-hole FreshCtx runnable-search_1)
                                       <-+
                                       search_2)))))
        "delay-through-left"]))

(define search-dfs-late-relcall-red
  (union-reduction-relations
   search-late-relcall-red
   search-dfs-late-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-dfs-late-relcall-red prog))
