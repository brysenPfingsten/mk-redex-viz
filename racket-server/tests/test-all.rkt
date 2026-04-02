#lang racket
(require rackunit rackunit/gui)
(require "./test-app.rkt"
         "./test-reification.rkt"
         "./test-syntax-checking.rkt"
         "./test-zipper.rkt"
         "./test-metafunctions.rkt"
         "./test-transpiler.rkt"
         "./test-disequality.rkt"
         "./test-reduction-relations.rkt"
         )

(define-test-suite ALL
  APP
  REIFICATION
  SYNTAX-CHECKER
  ZIPPER
  METAFUNCTIONS
  TRANSPILER
  DISEQUALITY
  RED)

(test/gui ALL)
