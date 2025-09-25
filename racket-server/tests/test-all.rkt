#lang racket
(require rackunit rackunit/gui)
(require #;"./test-app.rkt"
         "./test-reification.rkt"
         "./test-syntax-checking.rkt"
         "./test-zipper.rkt"
         "./test-metafunctions.rkt"
         "./test-transpiler.rkt")

(define-test-suite ALL
;;  APP
  REIFICATION
  SYNTAX-CHECKER
  ZIPPER
  METAFUNCTIONS
  TRANSPILER)

(test/gui ALL)
