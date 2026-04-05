#lang racket

(require redex/reduction-semantics
         "../languages/search-lang.rkt"
         (prefix-in search: "./search-pre-red.rkt"))

(provide search-early-pre-red)

(check-redundancy #t)

(define shared-local/under-ShellCtx
  (context-closure
   (context-closure search:local/base search-lang BranchCtx)
   search-lang
   ShellCtx))

(define search-early-pre-red
  (union-reduction-relations shared-local/under-ShellCtx search:shell/base))
