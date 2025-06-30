#lang racket
(require rackunit rackunit/gui)
(require "./test-app.rkt"
         "./test-reification.rkt"
         "./test-syntax-checking.rkt"
         "./test-zipper.rkt"
         "./test-metafunctions.rkt")

(define-test-suite ALL
  APP
  REIFICATION
  SYNTAX-CHECKER
  ZIPPER
  METAFUNCTIONS)

(test/gui ALL)
