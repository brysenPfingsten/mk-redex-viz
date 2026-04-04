#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-flip-seq-red
         step-once)

(check-redundancy #t)

(define search-flip-seq-extra
  (let ([search-flip-seq-extra/base
         (reduction-relation
          search-base-lang
          #:domain cfg
          [--> (in-hole KBranch ((in-hole QFresh (delay runnable-search_1)) <-+ search_2))
               (in-hole KBranch
                        (delay (search_2 <-+ (in-hole QFresh runnable-search_1))))
               "search-flip-seq/delay-swap-left"])])
    (context-closure search-flip-seq-extra/base search-base-lang QShell)))

(define search-flip-seq-red
  (union-reduction-relations
   search-base-seq-red
   search-flip-seq-extra))

(define (step-once prog)
  (step-once/deterministic search-flip-seq-red prog))
