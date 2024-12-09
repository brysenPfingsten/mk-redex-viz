#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)
(require redex-etc)

(provide red)
(require "definitions.rkt")

(define red
  (reduction-relation L
                      #:domain p

                      [--> (in-hole Ex ((g_1 ∨ g_2) σ))
                           (in-hole Ex ((g_1 σ) <-+ (g_2 σ)))
                           "distribute subst in disj"]

                      [--> (in-hole Ex ((g_1 ∧ g_2) σ))
                           (in-hole Ex ((g_1 σ) × g_2))
                           "distribute subst over conj"]

                      [--> (in-hole Ex (((⊤ σ_1) <-+ s) × g))
                           (in-hole Ex (((⊤ σ_1) × g) <-+ (s × g)))
                           "distribute left disj ans over conj"]

                      [--> (in-hole Ex ((s +-> (⊤ σ_1)) × g))
                           (in-hole Ex ((s × g) +-> ((⊤ σ_1) × g)))
                           "distribute right disj ans over conj"]

                      [--> (in-hole Ex (((⊤ σ) <-+ s) <-+ s_2))
                           (in-hole Ex ((⊤ σ) <-+ (s <-+ s_2)))
                           "reassociate disj1"]

                      [--> (in-hole Ex (s_2 +-> ((⊤ σ) <-+ s)))
                           (in-hole Ex ((⊤ σ) <-+ (s_2 +-> s)))
                           "reassociate disj2"]

                      [--> (in-hole Ex (s_2 +-> (s +-> (⊤ σ))))
                           (in-hole Ex ((s_2 +-> s) +-> (⊤ σ)))
                           "reassociate disj3"]
                      
                      [--> (in-hole Ex ((s +-> (⊤ σ)) <-+ s_2))
                           (in-hole Ex ((s <-+ s_2) +-> (⊤ σ)))
                           "reassociate disj4"]

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
                      
                      [--> (in-hole Ex ((∃ (x ...) g) (state sub c)))
                           (in-hole Ex ((substitute-env g (fresh-sub c x ...)) (state sub ,(+ (length (term (fresh-sub c x ...))) (term c)))))
                           "fresh-n subst"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ..._1) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es ((r_1 t ..._1) σ))))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es (delay ((substitute* g_1 (x_1 t) ...) σ)))))
                           "relcall and add delay"]

                      [--> (in-hole Ex ((t_1 =? t_2) (state sub c)))
                           (in-hole Ex (⊤ (state (unify (walk t_1 sub) (walk t_2 sub) sub) c)))
                           (where ((natural t) ...) (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "unify succeed"]

                      [--> (in-hole Ex (⊥ σ))
                           (in-hole Ex ())
                           "fail fails"]

                      [--> (in-hole Ex ((t_1 =? t_2) (state sub c)))
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

                      [--> (in-hole EΓ (in-hole Ev (delay s)))
                           (in-hole EΓ (in-hole Ev (done-delay s)))
                           "invoke delay"]

                      [--> (in-hole EΓ (in-hole Ev ((⊤ σ) <-+ s)))
                           (in-hole EΓ (in-hole Ev ((⊤ σ) + s)))
                           "promote left answer"]

                      [--> (in-hole EΓ (in-hole Ev (s +-> (⊤ σ))))
                           (in-hole EΓ (in-hole Ev ((⊤ σ) + s)))
                           "promote right answer"]
))