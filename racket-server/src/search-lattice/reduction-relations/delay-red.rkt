#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         (only-in "../languages/core-lang.rkt" fresh-tree-prefix->shell-prefix)
         (prefix-in core: "./core-red.rkt")
         "./private/step-utils.rkt")

(provide local/delta
         frontier/delta
         delay-red
         step-once)

(check-redundancy #t)

(define local/delta
  (reduction-relation
   delay-lang
   #:domain search
   [--> ((suspend g tag) σ)
        (delay (g σ))
        "suspend-goal"]
   [--> ((in-hole FreshCtx (delay runnable-search_1)) × g c)
        (delay ((in-hole FreshCtx runnable-search_1) × g c))
        "delay-through-conj"]))

(define local/base
  ;; L1 keeps the same two-stage shape as L0: a LocalCtx seam, then the
  ;; final ShellCtx closure when the runnable machine is assembled.
  (context-closure
   (union-reduction-relations
    (extend-reduction-relation core:local/base delay-lang)
    local/delta)
   delay-lang
   LocalCtx))

(define local/under-ShellCtx
  (context-closure
   local/base
   delay-lang
   ShellCtx))

(define frontier/delta
  (reduction-relation
   delay-lang
   #:domain cfg
   [--> (in-hole ShellCtx (in-hole FreshCtx (delay runnable-search_i)))
        (in-hole ShellCtx
                 (fresh-tree-prefix->shell-prefix
                  (in-hole FreshCtx (Deferred runnable-search_i))))
        "invoke-delay"]))

(define shell/base
  (union-reduction-relations
   (extend-reduction-relation core:shell/base delay-lang)
   frontier/delta))

(define delay-red
  (union-reduction-relations
   local/under-ShellCtx
   shell/base))

(define (step-once prog)
  (step-once/deterministic delay-red prog))
