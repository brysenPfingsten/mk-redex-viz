#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-pre-red.rkt")

(provide search-late-pre-red)

(check-redundancy #t)

(define search-late-shared-local/under-ShellCtx
  (context-closure
   (context-closure search-local/base search-lang LateCtx)
   search-lang
   ShellCtx))

(define search-late-pre-red
  (union-reduction-relations
   search-late-shared-local/under-ShellCtx
   search-shell/base))
