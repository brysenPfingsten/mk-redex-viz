#lang racket

(require redex/reduction-semantics
         (only-in "./delay-red.rkt"
                  delay-local/delta
                  delay-frontier/delta)
         (only-in "./disj-base-red.rkt"
                  disj-local/base
                  disj-shell/base))

(provide search-local/base
         search-shell/base)

(check-redundancy #t)

(require "../languages/search-base-lang.rkt")

;; search now joins only its immediate predecessors. disj carries the shared
;; assembled core seam here because Redex unions reject duplicate inherited
;; rule names when both parents contribute the same core rules.
(define search-local/base
  (union-reduction-relations
   (extend-reduction-relation disj-local/base search-lang)
   (context-closure
    (extend-reduction-relation delay-local/delta search-lang)
    search-lang
    LocalCtx)))

(define search-shell/base
  (union-reduction-relations
   (extend-reduction-relation disj-shell/base search-lang)
   (extend-reduction-relation delay-frontier/delta search-lang)))
