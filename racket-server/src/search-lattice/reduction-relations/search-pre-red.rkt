#lang racket

(require redex/reduction-semantics
         (prefix-in delay: "./delay-red.rkt")
         (prefix-in disj: "./disj-base-red.rkt"))

(provide local/base
         shell/base)

(check-redundancy #t)

(require "../languages/search-lang.rkt")

;; search now joins only its immediate predecessors. disj carries the shared
;; assembled core seam here because Redex unions reject duplicate inherited
;; rule names when both parents contribute the same core rules.
(define local/base
  (union-reduction-relations
   (extend-reduction-relation disj:local/base search-lang)
   (context-closure
    (extend-reduction-relation delay:local/delta search-lang)
    search-lang
    LocalCtx)))

(define shell/base
  (union-reduction-relations
   (extend-reduction-relation disj:shell/base search-lang)
   (extend-reduction-relation delay:frontier/delta search-lang)))
