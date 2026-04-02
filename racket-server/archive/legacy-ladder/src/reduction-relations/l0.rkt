#lang racket
(require redex/reduction-semantics
         "../languages/l0.rkt"
         "../wf/l0.rkt"
         "./private/step-utils.rkt")

(check-redundancy #t)

(provide Rl0-core
         step-once
         -->*e)

(module+ examples)

(module+ test
  (require (submod ".." examples)
           rackunit))

;; Term -> [Listof [List String Term]]
(define (step-once prog)
  (step-once/deterministic Rl0-core prog))

(define -->e
  (reduction-relation
    L0

    [--> ((g_1 ∧ g_2 tag) (state sub dis c trail tag_1))
         ((g_1 (state sub dis c trail tag_1)) × g_2 c)
         "l0/conj-distribute-state"]

    [--> ((succeed tag) σ)
         (⊤ σ)
         "l0/succeed"]

    [--> ((fail tag) σ)
         (empty-tree)
         "l0/fail"]

    [--> ((⊤ σ) × g c)
         (g σ)
         "l0/conj-bring-success"]

    [--> ((empty-tree) × g c)
         (empty-tree)
         "l0/conj-prune-fail"]

    [--> ((∃ d g tag) (state sub dis c trail tag_1))
         ((subst-goal g ((x_1 u_1) ...))
          (state sub dis (u_1 ... ,@(term c)) trail tag_1))
         (where ((x_1 u_1) ...)
                (fresh-substitution c d))
         "l0/fresh-substitute"]

    [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
         (⊤ (state sub_1 dis c ((t_3 =? t_4 tag_1) ... (t_1 =? t_2 tag)) tag_2))
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         (where #f (invalid? sub_1 dis))
         "l0/unify-success"]

    [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
         (empty-tree)
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         (where #t (invalid? sub_1 dis))
         "l0/unify-violates-disequality"]

    [--> ((t_1 =? t_2 tag) (state sub dis c trail tag_2))
         (empty-tree)
         (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
         "l0/unify-fail"]

    [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
         (⊤ (state sub dis_1 c trail tag_2))
         (where dis_1 ((t_1 t_2) ,@(term dis)))
         (where #f (invalid? sub dis_1))
         "l0/disequality-success"]

    [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
         (empty-tree)
         (where dis_1 ((t_1 t_2) ,@(term dis)))
         (where #t (invalid? sub dis_1))
         "l0/disequality-fail"]

    ))

(define -->*e (context-closure -->e L0 Kconj))

(define -->collect
  (reduction-relation
   L0
   #:domain config
   [--> (Γ (⊤ σ_new) as_old)
        (Γ (empty-tree) (append-answer as_old σ_new))
        "l0/collect-single-answer"]))

(define l0-cfg/work (context-closure -->*e L0 (Γ hole as)))

(define Rl0-core
  (union-reduction-relations
   l0-cfg/work
   -->collect))

(module+ examples
  (provide trivial-conjunction-tree)

  (define trivial-conjunction-tree
    (term (((succeed (label "fish")) ∧ (succeed (label "dog")) (label "horse")) (state () () () () (label "cat")))))
)

(module+ test
  (require (submod ".." examples))
  (check-true (redex-match? L0 σ (term (state () () () () (label "cat")))))
  (check-true (redex-match? L0 g (term ((succeed (label "fish")) ∧ (succeed (label "dog")) (label "horse")))))
  (check-true (redex-match? L0 s (term (((succeed (label "fish")) ∧ (succeed (label "dog")) (label "horse")) (state () () () () (label "cat"))))))

  (check-equal?
   (apply-reduction-relation -->*e trivial-conjunction-tree)
   '((((succeed (label "fish")) (state () () () () (label "cat")))
      ×
      (succeed (label "dog"))
      ())))

  (define (-->*e-closed? st)
    (let ([st* (apply-reduction-relation -->*e st)])
      (andmap (lambda (st^) (redex-match? L0 s st^)) st*)))

  (check-reduction-relation -->*e -->*e-closed?)

  (define (final-config? cfg)
    (redex-match? L0 end-config cfg))

  (define (wf-config-term? cfg)
    (judgment-holds (wf-config? ,cfg)))

  (define (unique-decomposition? cfg)
    (let ([next* (apply-reduction-relation Rl0-core cfg)])
      (cond
        [(final-config? cfg) (null? next*)] ; finals must be stuck
        [else (null? (cdr next*))])))        ; non-finals must have exactly one step

  (redex-check L0
               config
               (implies (wf-config-term? (term config))
                        (unique-decomposition? (term config)))
    #:attempts 10000)

  (define matches (redex-match L0 s trivial-conjunction-tree))
  (check-true (null? (cdr matches)))
  (define m (first matches))
  (check-true (match? m))

  (define binds (match-bindings m)) ; list of bind structs
  (check-equal? (map bind-name binds) '(s))
  (check-equal? (map bind-exp binds)
                (list '(((succeed (label "fish")) ∧ (succeed (label "dog")) (label "horse"))
                        (state () () () () (label "cat")))))

  (check-true (judgment-holds (wf-tree? ,trivial-conjunction-tree () ())))
  (check-true
   (judgment-holds
    (wf-tree?
     ,trivial-conjunction-tree
     ((r:foo (x:1 x:2 x:3) (succeed (label "ok"))))
     ())))

  (define (progress? cfg)
    (or (final-config? cfg)
        (not (null? (apply-reduction-relation Rl0-core cfg)))))

  (redex-check L0
               config
               (implies (wf-config-term? (term config))
                        (progress? (term config)))
    #:attempts 10000)

  (define (wf-preserved? cfg)
    (for/and ([cfg^ (in-list (apply-reduction-relation Rl0-core cfg))])
      (judgment-holds (wf-config? ,cfg^))))

  (redex-check L0
               config
               (implies (wf-config-term? (term config))
                        (wf-preserved? (term config)))
    #:attempts 10000)

  )
