#lang racket

(require rackunit
         rackunit/text-ui
         "../archive/legacy-ladder/tests/test-all-legacy.rkt")

(module+ test
  (run-tests LEGACY))
