#lang racket

(require rackunit
         rackunit/text-ui
         "./confidence-gates-tests.rkt"
         "./determinism-overlap-tests.rkt"
         "./example-compat-tests.rkt"
         "./helpers-tests.rkt"
         "./model-example-matrix-tests.rkt"
         "./property-core.rkt"
         "./search-lattice-tests.rkt"
         "./search-runtime-tests.rkt")

(define-test-suite HEADLESS
  HELPERS-TESTS
  PROPERTY-CORE
  SEARCH-RUNTIME
  SEARCH-LATTICE
  EXAMPLE-COMPAT
  DETERMINISM-OVERLAP
  CONFIDENCE-GATES
  MODEL-EXAMPLE-MATRIX)

(module+ test
  (run-tests HEADLESS))
