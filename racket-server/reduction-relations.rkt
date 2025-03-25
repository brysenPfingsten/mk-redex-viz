#lang racket
(require redex
         redex/reduction-semantics
         rackunit
         redex-etc
         redex/pict)
(check-redundancy #t)


(provide red)
(require "definitions.rkt" "judgment-forms.rkt")

(define red
  (reduction-relation L
                      #:domain p #;(side-condition p (where (judgment-holds (closed-program? p))))

                      [--> (in-hole Ex ((g_1 ∨ g_2 _) σ))
                           (in-hole Ex ((g_1 σ) <-+ (g_2 σ)))
                           "distribute subst in disj"]

                      [--> (in-hole Ex ((g_1 ∧ g_2 _) σ))
                           (in-hole Ex ((g_1 σ) × g_2))
                           "distribute subst over conj"]

                      [--> (in-hole Ex (((⊤ σ_1) <-+ s) × g))
                           (in-hole Ex (((⊤ σ_1) × g) <-+ (s × g)))
                           "distribute left disj ans over conj"]

                      [--> (in-hole Ex ((s +-> (⊤ σ)) × g))
                           (in-hole Ex ((s × g) +-> ((⊤ σ) × g)))
                           "distribute right disj ans over conj"]

                      [--> (in-hole Ex (s_2 +-> ((⊤ σ) <-+ s)))
                           (in-hole Ex ((⊤ σ) <-+ (s_2 +-> s)))
                           "reassociate right left disj"]

                      [--> (in-hole Ex (s_2 +-> (s +-> (⊤ σ))))
                           (in-hole Ex ((s_2 +-> s) +-> (⊤ σ)))
                           "reassociate right right disj"]

                      [--> (in-hole Ex (((⊤ σ) <-+ s) <-+ s_2))
                           (in-hole Ex ((⊤ σ) <-+ (s <-+ s_2)))
                           "reassociate left left disj"]

                      [--> (in-hole Ex ((s +-> (⊤ σ)) <-+ s_2))
                           (in-hole Ex ((s <-+ s_2) +-> (⊤ σ)))
                           "reassociate left right disj"]

                      [--> (in-hole Ex ((⊤ σ) × g))
                           (in-hole Ex (g σ))
                           "bring subst to 2nd conjunct"]

                      [--> (in-hole Ex (() × g))
                           (in-hole Ex ())
                           "prune failure conjuncts"]

                      [--> (in-hole Ex (() <-+ s))
                           (in-hole Ex s)
                           "prune left failure disjuncts"]
                      
                      [--> (in-hole Ex (s +-> ()))
                           (in-hole Ex s)
                           "prune right failure disjuncts"]
                      
                      [--> (in-hole Ex ((∃ (x ...) g _) (state sub c trail)))
                           (in-hole Ex ((substitute-env g (fresh-sub c x ...)) (state sub ,(+ (length (term (fresh-sub c x ...))) (term c)) trail)))
                           "fresh-n subst"]

                      [--> (in-hole Ex ((r_1 t ... o) σ))
                           (in-hole Ex (delay (unfold-me ((r_1 t ... o) σ))))
                           "relcall and add delay"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es (unfold-me ((r_1 t ... o) σ)))))
						   (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es ((substitute* g_1 (x_1 t) ...) σ))))
                           "substitute in for relation body and proceed through it"]

                      [--> (in-hole Ex ((t_1 =? t_2 o) (state sub c ((t_3 =? t_4 o_1) ...))))
                           (in-hole Ex (⊤ (state (unify (walk t_1 sub) (walk t_2 sub) sub) c ((t_3 =? t_4 o_1) ... (t_1 =? t_2 o)))))
                           (where ((natural t) ...) (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "unify succeed"]

                      [--> (in-hole Ex ((t_1 =? t_2 o) (state sub c trail)))
                           (in-hole Ex ())
                           (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "unify fails"]

                      [--> (in-hole Ex ((delay s) × g))
                           (in-hole Ex (delay (s × g)))
                           "propagate delay through conj"]

                      [--> (in-hole Ex ((delay s_1) <-+ s_2))
                           (in-hole Ex (delay (s_1 +-> s_2)))
                           "propagate left delay through disj, and flip"]
                      
                      [--> (in-hole Ex (s_2 +-> (delay s_1)))
                           (in-hole Ex (delay (s_2 <-+ s_1)))
                           "propagate right delay through disj, and flip"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (delay s)))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev s))
                           "invoke delay"]

                      [--> (prog Γ (in-hole Ev ((⊤ σ) <-+ s)))
                           (prog Γ (in-hole Ev ((⊤ σ) + s)))
                           "promote left answer"]

                      [--> (prog Γ (in-hole Ev (s +-> (⊤ σ))))
                           (prog Γ (in-hole Ev ((⊤ σ) + s)))
                           "promote right answer"]
                      ))
