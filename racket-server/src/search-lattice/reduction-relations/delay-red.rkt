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
        "delay/suspend-goal"]
   [--> ((in-hole QFresh (delay runnable-search_1)) × g c)
        (delay ((in-hole QFresh runnable-search_1) × g c))
        "delay/delay-through-conj"]))

(define delay-frontier/delta
  (reduction-relation
   delay-lang
   #:domain cfg
   [--> (in-hole QShell (in-hole QFresh (delay runnable-search_i)))
        (in-hole QShell
                 (fresh-tree-prefix->shell-prefix
                   (in-hole QFresh (Bounced runnable-search_i))))
        "delay/invoke-delay"]))

(define delay-local/base
  (union-reduction-relations
   (extend-reduction-relation core-local/base delay-lang)
   delay-local/delta))

;; L1 mirrors L0: an augmented local base, then the usual KLocal/QShell closure.
(define delay-local
  (context-closure
   (context-closure delay-local/base delay-lang KLocal)
   delay-lang
   QShell))

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
