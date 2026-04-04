#lang racket

(require redex/reduction-semantics
         "../languages/search-base-lang.rkt"
         "./search-base-pre-red.rkt")

(provide search-early-pre-red)

(check-redundancy #t)

(define search-early-shared-local/under-ShellCtx
  (context-closure
   (context-closure search-local/base search-lang BranchCtx)
   search-lang
   ShellCtx))

(define search-early-pre-red
  (union-reduction-relations
   search-early-shared-local/under-ShellCtx
   search-shell/base))
