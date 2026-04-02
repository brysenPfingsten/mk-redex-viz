#lang racket

(require redex/reduction-semantics
         "./l0.rkt"
         "./l1.rkt"
         "./l2.rkt"
         "./l3.rkt"
         "./l4.rkt"
         "../languages/all.rkt")

(provide (all-from-out "./l0.rkt")
         wf-config/L1?
         wf-config/L2?
         wf-config/L3?
         wf-config/L4?
         config-in-target-domain?
         wf-config/target?)

(define (config-in-target-domain? target-id cfg)
  (case (string->symbol target-id)
    [(L0/config) (redex-match? L0 config cfg)]
    [(L1/config) (redex-match? L1 config cfg)]
    [(L2/config) (redex-match? L2 config cfg)]
    [(L3/config) (redex-match? L3 config cfg)]
    [(L4/config) (redex-match? L4 config cfg)]
    [else #f]))

(define (wf-config/target? target-id cfg)
  (case (string->symbol target-id)
    [(L0/config) (and (redex-match? L0 config cfg)
                      (judgment-holds (wf-config? ,cfg)))]
    [(L1/config) (and (redex-match? L1 config cfg)
                      (judgment-holds (wf-config/L1? ,cfg)))]
    [(L2/config) (and (redex-match? L2 config cfg)
                      (judgment-holds (wf-config/L2? ,cfg)))]
    [(L3/config) (and (redex-match? L3 config cfg)
                      (judgment-holds (wf-config/L3? ,cfg)))]
    [(L4/config) (and (redex-match? L4 config cfg)
                      (judgment-holds (wf-config/L4? ,cfg)))]
    [else #f]))
