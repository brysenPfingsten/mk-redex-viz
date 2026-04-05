#lang racket

(require redex/reduction-semantics
         "../languages/search-lang.rkt"
         (prefix-in search: "./search-pre-red.rkt"))

(provide search-late-pre-red)

(check-redundancy #t)

(define shared-local/under-ShellCtx
  (context-closure
   (context-closure search:local/base search-lang LateCtx)
   search-lang
   ShellCtx))

(define search-late-pre-red
  (union-reduction-relations shared-local/under-ShellCtx search:shell/base))
