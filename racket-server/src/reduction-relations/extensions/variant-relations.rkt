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
 ;; Canonical step wrappers
 step-once/Rl1-call-eager
 step-once/Rl1-call-lazy
 step-once/Rl2-disj-left
 step-once/Rl3-pre-eager
 step-once/Rl3-pre-lazy
 step-once/Rl3-dfs-eager
 step-once/Rl3-dfs-lazy
 step-once/Rl3-flip-eager
 step-once/Rl3-flip-lazy
 step-once/Rl4-rail-eager
 step-once/Rl4-rail-lazy)

;; Canonical step wrappers.
(define (step-once/Rl1-call-eager prog)
  (step-once/by Rl1-call-eager prog))

(define (step-once/Rl1-call-lazy prog)
  (step-once/by Rl1-call-lazy prog))

(define (step-once/Rl2-disj-left prog)
  (step-once/by Rl2-disj-left prog))

(define (step-once/Rl3-pre-eager prog)
  (step-once/by Rl3-pre-eager prog))

(define (step-once/Rl3-pre-lazy prog)
  (step-once/by Rl3-pre-lazy prog))

(define (step-once/Rl3-dfs-eager prog)
  (step-once/by Rl3-dfs-eager prog))

(define (step-once/Rl3-dfs-lazy prog)
  (step-once/by Rl3-dfs-lazy prog))

(define (step-once/Rl3-flip-eager prog)
  (step-once/by Rl3-flip-eager prog))

(define (step-once/Rl3-flip-lazy prog)
  (step-once/by Rl3-flip-lazy prog))

(define (step-once/Rl4-rail-eager prog)
  (step-once/by Rl4-rail-eager prog))

(define (step-once/Rl4-rail-lazy prog)
  (step-once/by Rl4-rail-lazy prog))
