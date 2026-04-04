#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         (only-in "../languages/core-lang.rkt"
                  fresh-tree-prefix->shell-prefix)
         (only-in "./core-red.rkt"
                  core-local/base
                  core-shell/base)
         "./private/step-utils.rkt")

(provide delay-local/delta
         delay-frontier/delta
         delay-red
         step-once)

(check-redundancy #t)

(define delay-local/delta
  (reduction-relation
   delay-lang
   #:domain search
   [--> ((suspend g tag) σ)
        (delay (g σ))
        "suspend-goal"]
   [--> ((in-hole FreshCtx (delay runnable-search_1)) × g c)
        (delay ((in-hole FreshCtx runnable-search_1) × g c))
        "delay-through-conj"]))

(define delay-frontier/delta
  (reduction-relation
   delay-lang
   #:domain cfg
   [--> (in-hole ShellCtx (in-hole FreshCtx (delay runnable-search_i)))
        (in-hole ShellCtx
                 (fresh-tree-prefix->shell-prefix
                   (in-hole FreshCtx (Deferred runnable-search_i))))
        "invoke-delay"]))

(define delay-local/base
  (union-reduction-relations
   (extend-reduction-relation core-local/base delay-lang)
   delay-local/delta))

;; L1 mirrors L0: an augmented local base, then the usual LocalCtx/ShellCtx closure.
(define delay-local
  (context-closure
   (context-closure delay-local/base delay-lang LocalCtx)
   delay-lang
   ShellCtx))

(define delay-shell/base
  (union-reduction-relations
   (extend-reduction-relation core-shell/base delay-lang)
   delay-frontier/delta))

(define delay-red
  (union-reduction-relations
   delay-local
   delay-shell/base))

(define (step-once prog)
  (step-once/deterministic delay-red prog))
