#lang racket

(require redex/reduction-semantics
         "../canonical-adapter.rkt"
         "../languages/canonical-lang.rkt"
         "./calls-wf.rkt"
         "./canonical-core-wf.rkt"
         "./core-wf.rkt"
         "./delay-wf.rkt"
         "./disj-wf.rkt"
         "./rail-calls-wf.rkt"
         "./rail-wf.rkt"
         "./search-base-calls-wf.rkt"
         "./search-base-wf.rkt")

(provide (all-from-out "./canonical-core-wf.rkt")
         (all-from-out "./core-wf.rkt")
         (all-from-out "./delay-wf.rkt")
         (all-from-out "./disj-wf.rkt")
         (all-from-out "./search-base-wf.rkt")
         (all-from-out "./rail-wf.rkt")
         (all-from-out "./calls-wf.rkt")
         (all-from-out "./search-base-calls-wf.rkt")
         (all-from-out "./rail-calls-wf.rkt")
         config-in-target-domain?
         wf-config/target?)

(define (config-in-target-domain? target-id cfg)
  (case (string->symbol target-id)
    [(canonical/config) (redex-match? canonical-lang config cfg)]
    [else #f]))

(define (wf-config/target? target-id cfg)
  (case (string->symbol target-id)
    [(canonical/config)
     (and (redex-match? canonical-lang config cfg)
          (judgment-holds
           (wf-config/search-base-calls?
            ,(canonical-flat->calls-config cfg))))]
    [else #f]))
