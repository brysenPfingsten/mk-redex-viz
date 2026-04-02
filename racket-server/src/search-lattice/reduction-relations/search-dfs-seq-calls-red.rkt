#lang racket

(require redex/reduction-semantics
         "../languages/search-base-seq-calls-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-calls-red.rkt")

(provide search-dfs-seq-calls-red
         step-once)

(check-redundancy #t)

(define search-dfs-seq-calls-red
  (extend-reduction-relation
   search-base-seq-calls-red
   search-base-seq-calls-lang
   [--> (Γ (in-hole Q (in-hole KScopePath ((delay f_1) <-+ f_2))))
        (Γ (in-hole Q (in-hole KScopePath (delay (f_1 <-+ f_2)))))
        "search-dfs-seq-calls/delay-through-left"]))

(define (step-once prog)
  (step-once/deterministic search-dfs-seq-calls-red prog))
