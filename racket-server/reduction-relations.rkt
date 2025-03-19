#lang racket
(require redex)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)
(require redex-etc)

(provide red)
(require "definitions.rkt" "judgment-forms.rkt")

(define red
  (reduction-relation L
                      #:domain p #;(side-condition p (judgment-holds (closed-program? (term p))))

                      [--> (in-hole Ex ((g_1 ∨ g_2) σ))
                           (in-hole Ex ((g_1 σ) <-+ (g_2 σ)))
                           "distribute subst in disj"]

                      [--> (in-hole Ex ((g_1 ∧ g_2) σ))
                           (in-hole Ex ((g_1 σ) × g_2))
                           "distribute subst over conj"]

                      [--> (in-hole Ex (((⊤ σ_1) + s) × g))
                           (in-hole Ex (((⊤ σ_1) × g) <-+ (s × g)))
                           "distribute disj ans over conj"]

                      [--> (in-hole Ex (s_2 +-> ((⊤ σ) + s)))
                           (in-hole Ex ((⊤ σ) + (s_2 +-> s)))
                           "reassociate right disj"]

                      [--> (in-hole Ex (((⊤ σ) + s) <-+ s_2))
                           (in-hole Ex ((⊤ σ) + (s <-+ s_2)))
                           "reassociate left disj"]


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

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es ((r_1 t ...) σ))))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es (delay ((r_1 t ...) σ)))))
                           "relcall and add delay"]
                      #|
                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es ((r_1 t ..._1) σ))))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es (delay ((substitute* g_1 (x_1 t) ...) σ)))))
                           "relcall and add delay"]
                      |#

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

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es (delay ((r_1 t ..._1) σ)))))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (in-hole Es ((substitute* g_1 (x_1 t) ...) σ))))
                           "invoke delay"]

                      [--> (in-hole Ex ((⊤ σ) <-+ s))
                           (in-hole Ex ((⊤ σ) + s))
                           "promote left answer"]

                      [--> (in-hole Ex (s +-> (⊤ σ)))
                           (in-hole Ex ((⊤ σ) + s))
                           "promote right answer"]
                      ))