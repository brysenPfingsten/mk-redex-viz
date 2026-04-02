#lang racket

(provide dedupe-tagged-successors)

;; Remove duplicate tagged successors while preserving first-seen order.
(define (dedupe-tagged-successors succ*)
  (define seen (make-hash))
  (for/list ([succ (in-list succ*)]
             #:unless (hash-ref seen succ #f))
    (hash-set! seen succ #t)
    succ))
