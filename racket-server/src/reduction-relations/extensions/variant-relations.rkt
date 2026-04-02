#lang racket

(require redex/reduction-semantics
         "../step-utils.rkt"
         (rename-in "./rcall-eager.rkt" [Rcall-eager Rl1-call-eager])
         (rename-in "./rcall-lazy.rkt" [Rcall-lazy Rl1-call-lazy])
         (rename-in "./rdisj-left.rkt" [Rdisj-left Rl2-disj-left])
         (rename-in "./rbase-e.rkt" [Rbase-e Rl3-pre-eager])
         (rename-in "./rbase-l.rkt" [Rbase-l Rl3-pre-lazy])
         (rename-in "./rflip-e.rkt" [Rflip-e Rl3-flip-eager])
         (rename-in "./rflip-l.rkt" [Rflip-l Rl3-flip-lazy])
         (rename-in "./rrail-e.rkt" [Rrail-e Rl4-rail-eager])
         (rename-in "./rrail-l.rkt" [Rrail-l Rl4-rail-lazy])
         "./rdfs-common.rkt")

;; Relation names follow the language/relation lattice:
;; - Rl1-call-{eager,lazy}
;; - Rl2-disj-left
;; - Rl3-pre-{eager,lazy}
;; - Rl3-dfs-{eager,lazy}
;; - Rl3-flip-{eager,lazy}
;; - Rl4-rail-{eager,lazy}

(define (step-once/by rel prog)
  (dedupe-tagged-successors
   (apply-reduction-relation/tag-with-names rel (term ,prog))))

(define Rl3-dfs-eager
  (extend-with-dfs-rules Rl3-pre-eager))

(define Rl3-dfs-lazy
  (extend-with-dfs-rules Rl3-pre-lazy))

(provide
 ;; Canonical relation exports
 Rl1-call-eager
 Rl1-call-lazy
 Rl2-disj-left
 Rl3-pre-eager
 Rl3-pre-lazy
 Rl3-dfs-eager
 Rl3-dfs-lazy
 Rl3-flip-eager
 Rl3-flip-lazy
 Rl4-rail-eager
 Rl4-rail-lazy
 ;; Canonical generic step function
 step-once/by)
