#lang racket

(require rackunit
         rackunit/text-ui
         "./determinism-overlap-tests.rkt"
         "./property-core.rkt"
         "./search-lattice-tests.rkt"
         "./stabilization-gates-tests.rkt")

(define-test-suite HEADLESS
  DETERMINISM-OVERLAP
  PROPERTY-CORE
  SEARCH-LATTICE
  STABILIZATION-GATES)

(module+ test
  (run-tests HEADLESS))
