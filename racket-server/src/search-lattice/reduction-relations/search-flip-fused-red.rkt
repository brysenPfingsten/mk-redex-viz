#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide search-flip-late-extra
         search-flip-late-red
         step-once)

(check-redundancy #t)

(define search-flip-late-extra
  (let ([search-flip-late-extra/base
         (reduction-relation
          search-lang
          #:domain cfg
          [--> (in-hole LateCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))
               (in-hole LateCtx
                        (delay (search_2 <-+ (in-hole FreshCtx runnable-search_1))))
               "delay-swap-left"])])
    (context-closure search-flip-late-extra/base search-lang ShellCtx)))

(define search-flip-late-red
  (union-reduction-relations
   search-late-red
   search-flip-late-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-late-red prog))
