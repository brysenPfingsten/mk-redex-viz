#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide search-dfs-seq-red
         step-once)

(check-redundancy #t)

(define search-dfs-seq-red
  (extend-reduction-relation
   search-base-seq-red
   search-base-seq-lang
   [--> (in-hole Q (in-hole KScopePath ((delay f_1) <-+ f_2)))
        (in-hole Q (in-hole KScopePath (delay (f_1 <-+ f_2))))
        "search-dfs-seq/delay-through-left"]))

(define (step-once prog)
  (step-once/deterministic search-dfs-seq-red prog))
