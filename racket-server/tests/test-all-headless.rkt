#lang racket

(require rackunit
         rackunit/text-ui
         "./helpers-tests.rkt"
         "./capability-analysis-tests.rkt"
         "./property-core.rkt"
         "./example-compat-tests.rkt"
         "./determinism-overlap-tests.rkt"
         "./confidence-gates-tests.rkt"
         "./model-example-matrix-tests.rkt"
         "./variant-module-tests.rkt"
         "./property-variants.rkt"
         "./property-variants-random.rkt")

(define-test-suite HEADLESS
  HELPERS-TESTS
  CAPABILITY-ANALYSIS
  PROPERTY-CORE
  EXAMPLE-COMPAT
  DETERMINISM-OVERLAP
  CONFIDENCE-GATES
  MODEL-EXAMPLE-MATRIX
  VARIANT-MODULES
  PROPERTY-VARIANTS
  PROPERTY-VARIANTS-RANDOM)

(module+ test
  (run-tests HEADLESS))
