#lang racket

(require rackunit
         rackunit/text-ui
         "./helpers-tests.rkt"
         "./property-core.rkt"
         "./example-compat-tests.rkt"
         "./variant-module-tests.rkt"
         "./property-variants.rkt"
         "./property-variants-random.rkt")

(define-test-suite HEADLESS
  HELPERS-TESTS
  PROPERTY-CORE
  EXAMPLE-COMPAT
  VARIANT-MODULES
  PROPERTY-VARIANTS
  PROPERTY-VARIANTS-RANDOM)

(module+ test
  (run-tests HEADLESS))
