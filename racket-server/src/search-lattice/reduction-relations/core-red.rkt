#lang racket

(require redex/reduction-semantics
         "../languages/core-lang.rkt"
         (only-in "./private/common.rkt"
                  subst-goal-host)
         "./private/step-utils.rkt")

(provide core-local/base
         core-shell/base
         core-red
         step-once)

(check-redundancy #t)

(define core-local/base
  (reduction-relation
   core-lang
   #:domain search
   [--> ((g_1 ∧ g_2 tag) (state sub dis c trail tag_1))
        ((g_1 (state sub dis c trail tag_1)) × g_2 c)
        "conj-distribute-state"]
   [--> ((succeed tag) σ)
        (⊤ σ)
        "succeed"]
   [--> ((fail tag) σ)
        (empty-tree)
        "fail"]
   [--> ((in-hole QFresh (⊤ σ)) × g c_2)
        (in-hole QFresh (g σ))
        "conj-bring-scoped-success"]
   [--> ((in-hole QFresh (empty-tree)) × g c_2)
        (in-hole QFresh (empty-tree))
        "conj-preserve-scoped-fail"]
   [--> ((∃ d g tag) (state sub dis c trail tag_1))
        (FreshenedTree (u_1 ...) (g_new (state sub dis (u_1 ... ,@(term c)) trail tag_1)) tag)
        (where ((x_bound u_1) ...) (fresh-substitution c d))
        (where g_new ,(subst-goal-host (term g) (term ((x_bound u_1) ...))))
        "fresh-substitute"]
   [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
        (⊤ (state sub_1 dis c ((t_3 =? t_4 tag_1) ... (t_1 =? t_2 tag)) tag_2))
        (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
        (where #f (invalid? sub_1 dis))
        "unify-success"]
   [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
        (empty-tree)
        (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
        (where #t (invalid? sub_1 dis))
        "unify-violates-disequality"]
   [--> ((t_1 =? t_2 tag) (state sub dis c trail tag_2))
        (empty-tree)
        (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
        "unify-fail"]
   [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
        (⊤ (state sub dis_1 c trail tag_2))
        (where dis_1 ((t_1 t_2) ,@(term dis)))
        (where #f (invalid? sub dis_1))
        "disequality-success"]
   [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
        (empty-tree)
        (where dis_1 ((t_1 t_2) ,@(term dis)))
        (where #t (invalid? sub dis_1))
        "disequality-fail"]))

(define core-shell/base
  (reduction-relation
   core-lang
   #:domain cfg
   [--> (in-hole QShell (in-hole QFresh+ (⊤ σ)))
        (in-hole QShell
                 (fresh-tree-prefix->shell-prefix
                  (in-hole QFresh+ (⊤ σ))))
        "finish-answer"]
   [--> (in-hole QShell (in-hole QFresh+ (empty-tree)))
        (in-hole QShell
                 (fresh-tree-prefix->shell-prefix
                  (in-hole QFresh+ (empty-tree))))
        "finish-fail"]))

(define core-local
  (context-closure
   (context-closure core-local/base core-lang KLocal)
   core-lang
   QShell))

;; Core splits unfinished tree work from the one final lift into the shell.
(define core-red
  (union-reduction-relations core-local core-shell/base))

(define (step-once prog)
  (step-once/deterministic core-red prog))
