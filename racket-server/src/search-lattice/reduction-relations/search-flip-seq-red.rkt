#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-flip-seq-red
         step-once)

(check-redundancy #t)

(define search-flip-seq-red
  (extend-reduction-relation
   search-base-seq-red
   search-base-seq-lang
   [--> (in-hole Q (in-hole KScopePath ((delay f_1) <-+ f_2)))
        (in-hole Q (in-hole KScopePath (delay (f_2 <-+ f_1))))
        "search-flip-seq/delay-swap-left"]))

(define (step-once prog)
  (step-once/deterministic search-flip-seq-red prog))
