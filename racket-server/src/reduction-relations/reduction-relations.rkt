#lang racket
(require redex
         redex/reduction-semantics
         rackunit
         redex/pict)
(check-redundancy #t)

(provide red step-once)
(require "../definitions.rkt" "../judgment-forms.rkt")

;; Term -> [Listof [List String Term]]
(define (step-once prog)
  (apply-reduction-relation/tag-with-names red (term ,prog)))

(define red
  (reduction-relation L 
                      #:domain (side-condition (name prog p) (judgment-holds (closed-program? prog)))

                      [--> (in-hole Ex ((g_1 ∨ g_2 _) σ))
                           (in-hole Ex ((g_1 σ) <-+ (g_2 σ)))
                           "Distribute State Over Disjunction"]

                      [--> (in-hole Ex ((g_1 ∧ g_2 _) σ))
                           (in-hole Ex ((g_1 σ) × g_2))
                           "Distribute State Over Conjunction"]

                      [--> (in-hole Ex (((⊤ σ) <-+ s) × g))
                           (in-hole Ex (((⊤ σ) × g) <-+ (s × g)))
                           "Distribute Left Disjunction Answer Over Conjunction"]

                      [--> (in-hole Ex ((s +-> (⊤ σ)) × g))
                           (in-hole Ex ((s × g) +-> ((⊤ σ) × g)))
                           "Distribute Right Disjunction Answer Over Conjunction"]

                      [--> (in-hole Ex (s_2 +-> ((⊤ σ) <-+ s)))
                           (in-hole Ex ((⊤ σ) <-+ (s_2 +-> s)))
                           "Reassociate Right-Left Disjunction"]

                      [--> (in-hole Ex (s_2 +-> (s +-> (⊤ σ))))
                           (in-hole Ex ((s_2 +-> s) +-> (⊤ σ)))
                           "Reassociate Right-Right Disjunction"]

                      [--> (in-hole Ex (((⊤ σ) <-+ s) <-+ s_2))
                           (in-hole Ex ((⊤ σ) <-+ (s <-+ s_2)))
                           "Reassociate Left-Left Disjunction"]

                      [--> (in-hole Ex ((s +-> (⊤ σ)) <-+ s_2))
                           (in-hole Ex ((s <-+ s_2) +-> (⊤ σ)))
                           "Reassociate Left-Right Disjunction"]

                      [--> (in-hole Ex ((⊤ σ) × g))
                           (in-hole Ex (g σ))
                           "Bring Success State To Second Conjunct"]

                      [--> (in-hole Ex (() × g))
                           (in-hole Ex ())
                           "Prune Failed Conjuncts"]

                      [--> (in-hole Ex (() <-+ s))
                           (in-hole Ex s)
                           "Prune Left Disjunction Failure"]
                      
                      [--> (in-hole Ex (s +-> ()))
                           (in-hole Ex s)
                           "Prune Right Disjunction Failure"]
                      
                      [--> (in-hole Ex ((∃ (x ...) g _) (state sub c trail)))
                           (in-hole Ex ((substitute g ,@(term (fresh-sub c x ...))) 
                                        (state sub ,(+ (length (term (fresh-sub c x ...))) (term c)) trail)))
                           "Substitute Fresh Variables"]

                      [--> (in-hole Ex ((r_1 t ... o) σ))
                           (in-hole Ex (delay (proceed ((r_1 t ... o) σ))))
                           "Relation Call And Add Delay"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                 (in-hole Ev (in-hole Es (proceed ((r_1 t ... o) σ)))))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                 (in-hole Ev (in-hole Es ((substitute g_1 (x_1 t) ...) σ))))
                           "Substitute Relation Body And Proceed"]

                      [--> (in-hole Ex ((t_1 =? t_2 o) (state sub c ((t_3 =? t_4 o_1) ...))))
                           (in-hole Ex (⊤ (state (unify (walk t_1 sub) (walk t_2 sub) sub) c ((t_3 =? t_4 o_1) ... (t_1 =? t_2 o)))))
                           (where ((natural t) ...) (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "Unification Succeeds"]

                      [--> (in-hole Ex ((t_1 =? t_2 o) (state sub c trail)))
                           (in-hole Ex ())
                           (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "Unification Fails"]

                      [--> (in-hole Ex ((delay s) × g))
                           (in-hole Ex (delay (s × g)))
                           "Propagate Delay Through Conjunction"]

                      [--> (in-hole Ex ((delay s_1) <-+ s_2))
                           (in-hole Ex (delay (s_1 +-> s_2)))
                           "Propagate Delay Through Left Disjunction And Flip"]
                      
                      [--> (in-hole Ex (s_2 +-> (delay s_1)))
                           (in-hole Ex (delay (s_2 <-+ s_1)))
                           "Propagate Delay Through Right Disjunction And Flip"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev (delay s)))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) (in-hole Ev s))
                           "Invoke Delay"]

                      [--> (prog Γ (in-hole Ev ((⊤ σ) <-+ s)))
                           (prog Γ (in-hole Ev ((⊤ σ) + s)))
                           "Promote Left Answer"]

                      [--> (prog Γ (in-hole Ev (s +-> (⊤ σ))))
                           (prog Γ (in-hole Ev ((⊤ σ) + s)))
                           "Promote Right Answer"]
                      ))
