#lang racket
(require redex
         redex/reduction-semantics
         redex/pict)
(check-redundancy #t)

(provide no-rr step-once)
(require "../definitions.rkt" "../judgment-forms.rkt")

;; Term -> [Listof [List String Term]]
(define (step-once prog)
  (apply-reduction-relation/tag-with-names no-rr (term ,prog)))

(define no-rr
  (reduction-relation L 
                      #:domain (side-condition (name prog p) (judgment-holds (closed-program? prog)))

                      [==> ((g_1 ∨ g_2 _) (state sub c trail o))
                           ((g_1 (state sub c trail o)) <-+ (g_2 (state sub c trail ,(symbol->string (gensym)))))
                           "Distribute State Over Disjunction"]

                      [==> ((g_1 ∧ g_2 _) σ)
                           ((g_1 σ) × g_2)
                           "Distribute State Over Conjunction"]

                      [==> (((⊤ σ) <-+ s) × g)
                           (((⊤ σ) × g) <-+ (s × g))
                           "Distribute Disjunction Answer Over Conjunction"]

                      [==> (((⊤ σ) <-+ s) <-+ s_2)
                           ((⊤ σ) <-+ (s <-+ s_2))
                           "Reassociate Disjunction"]

                      [==> ((⊤ σ) × g)
                           (g σ)
                           "Bring Success State To Second Conjunct"]

                      [==> (() × g)
                           ()
                           "Prune Failed Conjuncts"]

                      [==> (() <-+ s)
                           s
                           "Prune Disjunction Failure"]
                      
                      [==> ((∃ (x ...) g _) (state sub c trail o))
                           ((substitute g ,@(term (fresh-sub c x ...))) 
                                        (state sub ,(+ (length (term (fresh-sub c x ...))) (term c)) trail o))
                           "Substitute Fresh Variables"]

                      [==> ((r_1 t ... o) σ)
                           (delay (proceed ((r_1 t ... o) σ)))
                           "Relation Call And Add Delay"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                 (in-hole Ev (in-hole Es (proceed ((r_1 t ... o) σ)))))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                 (in-hole Ev (in-hole Es ((substitute g_1 (x_1 t) ...) σ))))
                           "Substitute Relation Body And Proceed"]

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
                           (delay (s_2 <-+ s_1))
                           "Propagate Delay Through Disjunction And Flip"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (delay s)))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev s))
                           "Invoke Delay"]

                      [--> (prog Γ (in-hole Ev ((⊤ σ) <-+ s)))
                           (prog Γ (in-hole Ev ((⊤ σ) + s)))
                           "Promote Answer"]

                      with
                      [(--> (in-hole Ex a) (in-hole Ex b))
                            (==> a b)]
                      ))
