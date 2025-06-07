#lang racket

(require redex
         redex/reduction-semantics
         rackunit
         redex-etc
         redex/pict)
(check-redundancy #t)

(provide dmitry-and-dmitry step-once)
(require "../definitions.rkt" "../judgment-forms.rkt")

;; Term -> [Listof [List String Term]]
(define (step-once prog)
  (apply-reduction-relation/tag-with-names dmitry-and-dmitry (term ,prog)))

(define dmitry-and-dmitry
  (reduction-relation L 
                      #:domain (side-condition (name prog p) (judgment-holds (closed-program? prog)))

                      [--> (in-hole Ex ((t_1 =? t_2 o) (state sub c trail)))
                           (in-hole Ex ())
                           (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "UnifyFail"]

                      [--> (in-hole Ex ((t_1 =? t_2 o) (state sub c ((t_3 =? t_4 o_1) ...))))
                           (in-hole Ex (delay (⊤ (state (unify (walk t_1 sub) (walk t_2 sub) sub) c ((t_3 =? t_4 o_1) ... (t_1 =? t_2 o))))))
                           (where ((natural t) ...) (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "UnifySuccess"]

                      [--> (in-hole Ex ((g_1 ∨ g_2 _) σ))
                           (in-hole Ex ((g_1 σ) <-+ (g_2 σ)))
                           "Disj"]

                      [--> (in-hole Ex ((g_1 ∧ g_2 _) σ))
                           (in-hole Ex ((g_1 σ) × g_2))
                           "Conj"]

                      [--> (in-hole Ex ((∃ (x) g _) (state sub c trail)))
                           (in-hole Ex (delay ((substitute-env g (fresh-sub c x)) (state sub ,(+ 1 (term c)) trail))))
                           "Fresh"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                  (in-hole Ev (in-hole Es ((r_1 t ... o) σ))))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                  (delay (in-hole Ev (in-hole Es ((substitute* g_1 (x_1 t) ...) σ)))))
                           "Invoke"]

                      [--> (in-hole Ex (() <-+ s))
                           (in-hole Ex s)
                           "DisjStopLeft"]
                      
                      [--> (in-hole Ex (s +-> ()))
                           (in-hole Ex s)
                           "DisjStopRight"]

                      [--> (in-hole Ex ((⊤ σ) <-+ s))
                           (in-hole Ex s)
                           "DisjStopAnsLeft"]

                      [--> (prog Γ (in-hole Ex (s +-> (⊤ σ))))
                           (prog Γ (in-hole Ex s))
                           "DisjStopAnsRight"]

                      [--> (in-hole Ex (() × g))
                           (in-hole Ex ())
                           "ConjStop"]

                      [--> (in-hole Ex ((⊤ σ) × g))
                           (in-hole Ex (g σ))
                           "ConjStopAns"]
                      
                      [--> (in-hole Ex ((delay s_1) <-+ s_2)) 
                           (in-hole Ex (s_1 +-> s_2)) 
                           (where #f ,(redex-match? L (⊤ σ) (term s_1)))
                           "DisjStepLeft"]

                      [--> (in-hole Ex (s_1 +-> (delay s_2)))
                           (in-hole Ex (s_1 <-+ s_2))
                           (where #f ,(redex-match? L (⊤ σ) (term s_2)))
                           "DisjStepRight"]

                      [--> (in-hole Ex ((delay (⊤ σ)) <-+ s))
                           (in-hole Ex ((⊤ σ) +-> s))
                           "DisjStepAnsLeft"]

                      [--> (in-hole Ex (s +-> (delay (⊤ σ))))
                           (in-hole Ex (s <-+ (⊤ σ)))
                           "DisjStepAnsRight"]

                      [--> (in-hole Ex ((delay s) × g))
                           (in-hole Ex (s × g))
                           (where #f ,(redex-match? L (⊤ σ) (term s_1)))
                           "ConjStep"]

                      [--> (in-hole Ex (((⊤ σ) <-+ s) × g))
                           (in-hole Ex (((⊤ σ) × g) <-+ (s × g)))
                           "ConjStepAnsLeft"]

                      [--> (in-hole Ex ((s +-> (⊤ σ)) × g))
                           (in-hole Ex ((s × g) +-> ((⊤ σ) × g)))
                           "ConjStepAnsRight"]

                      [--> (prog Γ (in-hole Ev (delay s)))
                           (prog Γ (in-hole Ev s))
                           "TopStep"]
                      ))
