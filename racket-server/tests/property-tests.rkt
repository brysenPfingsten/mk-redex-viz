#lang racket

(require rackunit
         rackunit/text-ui
         "./property-core.rkt")

;; Legacy entrypoint kept for compatibility.
;; New property suite lives in property-core.rkt.
(define-test-suite PROPERTY-TESTS-LEGACY
  PROPERTY-CORE)

(module+ test
  (displayln "[property-tests] Delegating to PROPERTY-CORE (legacy wrapper).")
  (run-tests PROPERTY-TESTS-LEGACY))
