#lang racket
(require redex
		 redex/reduction-semantics
		 rackunit
		 redex/pict)
(check-redundancy #t)

(provide red step-once red-tree)
(require "../definitions.rkt" "../judgment-forms.rkt")

;; Term -> [Listof [List String Term]]
(define (step-once prog)
  (apply-reduction-relation/tag-with-names red (term ,prog)))

(define red
  (reduction-relation L
                      #:domain (side-condition (name prog p) (judgment-holds (closed-program? prog)))

                      [--> ((in-hole Ex (proceed ((r_1 t ... o) σ)))
                            ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...))
                           ((in-hole Ex ((substitute g_1 (x_1 t) ...) σ))
                            ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...))
                           "Substitute Relation Body And Proceed"]

					  [--> ((in-hole Ev (delay s)) ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...))
						   ((in-hole Ev s) ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...))
						   "Invoke Delay"]

					  [--> ((in-hole Ev ((⊤ σ) <-+ s)) Γ)
						   ((in-hole Ev ((⊤ σ) + s)) Γ)
						   "Promote Left Answer"]

					  [--> ((in-hole Ev (s +-> (⊤ σ))) Γ)
						   ((in-hole Ev ((⊤ σ) + s)) Γ)
						   "Promote Right Answer"]

             [--> (e_1 Γ)
                           (e_2 Γ)
						   (side-condition (not (null? (apply-reduction-relation red-tree (term e_1)))))
                           (where e_2 ,(car (apply-reduction-relation red-tree (term e_1))))
							(computed-name (caar (apply-reduction-relation/tag-with-names red-tree (term e_1))))]))

(define red-tree
  (reduction-relation L
                      #:domain e
                      [==> ((g_1 ∨ g_2 _) (state sub c trail o))
                           ((g_1 (state sub c trail o)) <-+ (g_2 (state sub c trail ,(symbol->string (gensym)))))
                           "Distribute State Over Disjunction"]

                      [==> ((g_1 ∧ g_2 _) σ)
                           ((g_1 σ) × g_2)
                           "Distribute State Over Conjunction"]

                      [==> (((⊤ σ) <-+ s) × g)
                           (((⊤ σ) × g) <-+ (s × g))
                           "Distribute Left Disjunction Answer Over Conjunction"]

                      [==> ((s +-> (⊤ σ)) × g)
                           ((s × g) +-> ((⊤ σ) × g))
                           "Distribute Right Disjunction Answer Over Conjunction"]

                      [==> (s_2 +-> ((⊤ σ) <-+ s))
                           ((⊤ σ) <-+ (s_2 +-> s))
                           "Reassociate Right-Left Disjunction"]

                      [==> (s_2 +-> (s +-> (⊤ σ)))
                           ((s_2 +-> s) +-> (⊤ σ))
                           "Reassociate Right-Right Disjunction"]

                      [==> (((⊤ σ) <-+ s) <-+ s_2)
                           ((⊤ σ) <-+ (s <-+ s_2))
                           "Reassociate Left-Left Disjunction"]

                      [==> ((s +-> (⊤ σ)) <-+ s_2)
                           ((s <-+ s_2) +-> (⊤ σ))
                           "Reassociate Left-Right Disjunction"]

                      [==> ((⊤ σ) × g)
                           (g σ)
                           "Bring Success State To Second Conjunct"]

                      [==> (() × g)
                           ()
                           "Prune Failed Conjuncts"]

                      [==> (() <-+ s)
                           s
                           "Prune Left Disjunction Failure"]

                      [==> (s +-> ())
                           s
                           "Prune Right Disjunction Failure"]

                      [==> ((∃ (x ...) g _) (state sub c trail o))
                           ((substitute g ,@(term (fresh-sub c x ...))) 
                                        (state sub (bump c (x ...)) trail o))
                           "Substitute Fresh Variables"]

                      [==> ((r_1 t ... o) σ)
                           (delay (proceed ((r_1 t ... o) σ)))
                           "Relation Call And Add Delay"]

                      [==> ((t_1 =? t_2 o) (state sub c ((t_3 =? t_4 o_1) ...) o_2))
                           (⊤ (state sub_1 c ((t_3 =? t_4 o_1) ... (t_1 =? t_2 o)) o_2))
                           (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "Unification Succeeds"]

                      [==> ((t_1 =? t_2 o) (state sub _ _ _))
                           ()
                           (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "Unification Fails"]

                      [==> ((delay s) × g)
                           (delay (s × g))
                           "Propagate Delay Through Conjunction"]

                      [==> ((delay s_1) <-+ s_2)
                           (delay (s_1 +-> s_2))
                           "Propagate Delay Through Left Disjunction And Flip"]

                      [==> (s_2 +-> (delay s_1))
                           (delay (s_2 <-+ s_1))
                           "Propagate Delay Through Right Disjunction And Flip"]

                      with [(--> (in-hole Ex a) (in-hole Ex b))
                            (==> a b)]
					  ))
