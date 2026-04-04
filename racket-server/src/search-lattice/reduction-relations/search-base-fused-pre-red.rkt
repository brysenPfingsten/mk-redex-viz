#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-pre-red.rkt")

(provide search-base-fused-pre-red)

(check-redundancy #t)

(define search-base-fused-shared-local/under-QShell
  (context-closure
   (context-closure search-base-local/base search-base-lang KLate)
   search-base-lang
   QShell))

(define search-base-fused-pre-red
  (union-reduction-relations
   search-base-fused-shared-local/under-QShell
   search-base-shell/base))
