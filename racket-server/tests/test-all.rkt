#lang racket
(require rackunit rackunit/gui)
(require "./test-app.rkt"
         "./test-syntax-checking.rkt"
         "./test-zipper.rkt"
         "./test-transpiler.rkt")

(define-test-suite ALL
  APP
  SYNTAX-CHECKER
  ZIPPER
  TRANSPILER)

(test/gui ALL)
