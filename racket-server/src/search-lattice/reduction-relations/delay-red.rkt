#lang racket

(require redex/reduction-semantics
         "../languages/delay-lang.rkt"
         (only-in "../languages/core-lang.rkt"
                  fresh-tree-prefix->shell-prefix)
         (only-in "./core-red.rkt"
                  extend-core-local-redex
                  extend-core-shell-redex)
         "./private/step-utils.rkt")

(provide delay-local/base
         delay-local/under-QShell
         delay-frontier/base
         delay-red
         step-once)

(check-redundancy #t)

(define core-local/delay/base
  (extend-core-local-redex delay-lang))

(define core-local/delay
  (context-closure core-local/delay/base delay-lang KLocal))

(define core-shell/delay/base
  (extend-core-shell-redex delay-lang))

;; Delay lifts core local work under the first committed shell: QShell ∘ KLocal.
(define core-red/delay
  (union-reduction-relations
   (context-closure core-local/delay delay-lang QShell)
   core-shell/delay/base))

(define delay-local/base
  (reduction-relation
   delay-lang
   #:domain cfg
   [--> (in-hole KLocal ((suspend g tag) σ))
        (in-hole KLocal (delay (g σ)))
        "delay/suspend-goal"]
   [--> (in-hole KLocal ((in-hole QFresh (delay runnable-search_1)) × g c))
        (in-hole KLocal
                 (delay ((in-hole QFresh runnable-search_1) × g c)))
        "delay/delay-through-conj"]))

(define delay-frontier/base
  (reduction-relation
   delay-lang
   #:domain cfg
   [--> (in-hole QShell (in-hole QFresh (delay runnable-search_i)))
        (in-hole QShell
                 (fresh-tree-prefix->shell-prefix
                  (in-hole QFresh (Bounced runnable-search_i))))
        "delay/invoke-delay"]))

(define delay-local/under-QShell
  (context-closure delay-local/base delay-lang QShell))

(define delay-red
  (union-reduction-relations
   core-red/delay
   delay-local/under-QShell
   delay-frontier/base))

(define (step-once prog)
  (step-once/deterministic delay-red prog))
