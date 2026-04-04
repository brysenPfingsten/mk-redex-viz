#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-pre-red.rkt")

(provide search-base-seq-pre-red)

(check-redundancy #t)

(define search-base-seq-shared-local/under-QShell
  (context-closure
   (context-closure search-base-local/base search-base-lang KBranch)
   search-base-lang
   QShell))

(define search-base-seq-pre-red
  (union-reduction-relations
   search-base-seq-shared-local/under-QShell
   search-base-shell/base))
