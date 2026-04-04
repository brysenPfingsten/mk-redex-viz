#lang racket

(require redex/reduction-semantics
         "../languages/search-base-calls-lang.rkt"
         "./search-base-fused-calls-red.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         )

(provide search-flip-late-relcall-extra
         search-flip-late-relcall-red
         step-once)

(check-redundancy #t)

(define search-flip-late-relcall-extra
  (reduction-relation
   search-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole ShellCtx
                      (in-hole LateCtx
                               (delay (search_2
                                       <-+
                                       (in-hole FreshCtx runnable-search_1))))))
        "delay-swap-left"]))

(define search-flip-late-relcall-red
  (union-reduction-relations
   search-late-relcall-red
   search-flip-late-relcall-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-late-relcall-red prog))
