#lang racket

(require rackunit
         rackunit/text-ui
         "./determinism-overlap-tests.rkt"
         "./property-core.rkt"
         "./property-non-core.rkt"
         "./search-lattice-tests.rkt"
         "./stabilization-gates-tests.rkt")

(define-test-suite HEADLESS
  DETERMINISM-OVERLAP
  PROPERTY-CORE
  PROPERTY-NON-CORE
  SEARCH-LATTICE
  STABILIZATION-GATES)

(module+ test
  (run-tests HEADLESS))
