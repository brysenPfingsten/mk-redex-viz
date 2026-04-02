#lang racket
(require redex/reduction-semantics
         "../core-definitions.rkt"
         "../wf-core.rkt"
         "./step-utils.rkt")

(check-redundancy #t)

(provide -->cfg step-once -->*e)

(module+ examples)

(module+ test
  (require (submod ".." examples)
           rackunit))

;; Term -> [Listof [List String Term]]
(define (step-once prog)
  (dedupe-tagged-successors
   (apply-reduction-relation/tag-with-names -->cfg (term ,prog))))

(define -->e
  (reduction-relation
    Core

    [--> ((g_1 ∧ g_2 tag) (state sub dis c trail tag_1))
         ((g_1 (state sub dis c trail tag_1)) × g_2 c)
         "Distribute State Over Conjunction"]

    [--> ((succeed tag) σ)
         (⊤ σ)
         "(succeed) succeeds"]

    [--> ((fail tag) σ)
         (empty-tree)
         "(fail) fails"]

    [--> ((⊤ σ) × g c)
         (g σ)
         "Bring Success State To Second Conjunct"]

    [--> ((empty-tree) × g c)
         (empty-tree)
         "Prune Failed Conjuncts"]

    [--> ((∃ d g tag) (state sub dis c trail tag_1))
         ((subst-goal g ((x_1 u_1) ...))
          (state sub dis (u_1 ... ,@(term c)) trail tag_1))
         (where ((x_1 u_1) ...)
                (fresh-substitution c d))
         "Substitute Fresh Variables"]

    [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
         (⊤ (state sub_1 dis c ((t_3 =? t_4 tag_1) ... (t_1 =? t_2 tag)) tag_2))
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         (where #f (invalid? sub_1 dis))
         "Unification Succeeds"]

    [--> ((t_1 =? t_2 tag) (state sub dis c ((t_3 =? t_4 tag_1) ...) tag_2))
         (empty-tree)
         (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
         (where #t (invalid? sub_1 dis))
         "Unification Violates Disequality"]

    [--> ((t_1 =? t_2 tag) (state sub dis c trail tag_2))
         (empty-tree)
         (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
          "Unification Fails"]

    [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
         (⊤ (state sub dis_1 c trail tag_2))
         (where dis_1 ((t_1 t_2) ,@(term dis)))
         (where #f (invalid? sub dis_1))
         "Disequality Succeeds"]

    [--> ((t_1 != t_2 tag) (state sub dis c trail tag_2))
         (empty-tree)
         (where dis_1 ((t_1 t_2) ,@(term dis)))
         (where #t (invalid? sub dis_1))
         "Disequality Fails"]

    ))

(define -->*e (context-closure -->e Core Es))

(define -->collect
  (reduction-relation
   Core
   #:domain config
   [--> (Γ (⊤ σ_new) as_old)
        (Γ (empty-tree) (append-answer as_old σ_new))
        "Collect Single Answer"]))

(define -->cfg/work (context-closure -->*e Core (Γ hole as)))

(define -->cfg
  (union-reduction-relations
   -->cfg/work
   -->collect))

(module+ examples
  (provide trivial-conjunction-tree)

  (define trivial-conjunction-tree
    (term (((succeed (label "fish")) ∧ (succeed (label "dog")) (label "horse")) (state () () () () (label "cat")))))
)

(module+ test
  (require (submod ".." examples))
  (check-true (redex-match? Core σ (term (state () () () () (label "cat")))))
  (check-true (redex-match? Core g (term ((succeed (label "fish")) ∧ (succeed (label "dog")) (label "horse")))))
  (check-true (redex-match? Core s (term (((succeed (label "fish")) ∧ (succeed (label "dog")) (label "horse")) (state () () () () (label "cat"))))))

  (check-equal?
   (apply-reduction-relation -->*e trivial-conjunction-tree)
   '((((succeed (label "fish")) (state () () () () (label "cat")))
      ×
      (succeed (label "dog"))
      ())))

  (define (-->*e-closed? st)
    (let ([st* (apply-reduction-relation -->*e st)])
      (andmap (lambda (st^) (redex-match? Core s st^)) st*)))

  (check-reduction-relation -->*e -->*e-closed?)

  (define (final-config? cfg)
    (redex-match? Core end-config cfg))

  (define (wf-config-term? cfg)
    (judgment-holds (wf-config? ,cfg)))

  (define (unique-decomposition? cfg)
    (let ([next* (apply-reduction-relation -->cfg cfg)])
      (cond
        [(final-config? cfg) (null? next*)] ; finals must be stuck
        [else (null? (cdr next*))])))        ; non-finals must have exactly one step

  (redex-check Core
               config
               (implies (wf-config-term? (term config))
                        (unique-decomposition? (term config)))
    #:attempts 10000)

  (define matches (redex-match Core s trivial-conjunction-tree))
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
        (not (null? (apply-reduction-relation -->cfg cfg)))))

  (redex-check Core
               config
               (implies (wf-config-term? (term config))
                        (progress? (term config)))
    #:attempts 10000)

  (define (wf-preserved? cfg)
    (for/and ([cfg^ (in-list (apply-reduction-relation -->cfg cfg))])
      (judgment-holds (wf-config? ,cfg^))))

  (redex-check Core
               config
               (implies (wf-config-term? (term config))
                        (wf-preserved? (term config)))
    #:attempts 10000)

  )
