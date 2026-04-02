#lang racket

(require rackunit
         rackunit/text-ui
         "./helpers-tests.rkt"
         "./property-core.rkt"
         "./variant-module-tests.rkt"
         "./property-variants.rkt"
         "./property-variants-random.rkt")

(define-test-suite HEADLESS
  HELPERS-TESTS
  PROPERTY-CORE
  VARIANT-MODULES
  PROPERTY-VARIANTS
  PROPERTY-VARIANTS-RANDOM)

(module+ test
  (run-tests HEADLESS))
