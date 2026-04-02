#lang racket

(require rackunit
         rackunit/text-ui
         "./variant-module-tests.rkt"
         "./property-variants.rkt"
         "./property-variants-random.rkt")

(provide LEGACY)

(define-test-suite LEGACY
  VARIANT-MODULES
  PROPERTY-VARIANTS
  PROPERTY-VARIANTS-RANDOM)

(module+ test
  (run-tests LEGACY))
