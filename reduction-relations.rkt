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

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((g_1 ∨ g_2) σ))))
                           (in-hole EΓ (in-hole Ev (in-hole Es ((g_1 σ) + (g_2 σ)))))
                           "distribute subst in disj"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((g_1 ∧ g_2) σ))))
                           (in-hole EΓ (in-hole Ev (in-hole Es ((g_1 σ) × g_2))))
                           "distribute subst over conj"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es (((⊤ σ_1) + s) × g))))
                           (in-hole EΓ (in-hole Ev (in-hole Es (((⊤ σ_1) × g) + (s × g)))))
                           "distribute disj ans over conj"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es (((⊤ σ) + s) + s_2))))
                           (in-hole EΓ (in-hole Ev (in-hole Es ((⊤ σ) + (s + s_2)))))
                           "reassociate disj"]

                      [--> (in-hole EΓ (in-hole Ev (delay s)))
                           (in-hole EΓ (in-hole Ev s))
                           "invoke delay"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((⊤ σ) × g))))
                           (in-hole EΓ (in-hole Ev (in-hole Es (g σ))))
                           "bring subst to 2nd conjunct"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es (() × g))))
                           (in-hole EΓ (in-hole Ev (in-hole Es ())))
                           "prune failure conjuncts"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es (() + s))))
                           (in-hole EΓ (in-hole Ev (in-hole Es s)))
                           "prune failure disjuncts"]
                      
                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((∃ x ... g) (state sub c)))))
                           (in-hole EΓ (in-hole Ev (in-hole Es ((substitute-env g (fresh-sub c x ...)) (state sub ,(+ (length (term (fresh-sub c x ...))) (term c)))))))
                           "fresh-n subst"]

                      [--> (prog ((r_0 x_0 ... g_0) ... (r_1 x_1 ..._1 g_1) (r_2 x_2 ... g_2) ...) (in-hole Ev (in-hole Es ((r_1 t ..._1) σ))))
                           (prog ((r_0 x_0 ... g_0) ... (r_1 x_1 ... g_1) (r_2 x_2 ... g_2) ...) (in-hole Ev (in-hole Es (delay ((substitute* g_1 (x_1 t) ...) σ)))))
                           "relcall and add delay"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((t_1 =? t_2) (state sub c)))))
                           (in-hole EΓ (in-hole Ev (in-hole Es (⊤ (state (unify (walk t_1 sub) (walk t_2 sub) sub) c)))))
                           (where ((natural t) ...) (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "unify succeed"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es (⊥ σ))))
                           (in-hole EΓ (in-hole Ev (in-hole Es ())))
                           "fail fails"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((t_1 =? t_2) (state sub c)))))
                           (in-hole EΓ (in-hole Ev (in-hole Es ())))
                           (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "unify fails"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((delay s) × g))))
                           (in-hole EΓ (in-hole Ev (in-hole Es (delay (s × g)))))
                           "propagate delay through conj"]

                      [--> (in-hole EΓ (in-hole Ev (in-hole Es ((delay s_1) + s_2))))
                           (in-hole EΓ (in-hole Ev (in-hole Es (delay (s_2 + s_1)))))
                           "propagate delay through disj, and flip"]

                      ;; I think this is right because it's the equivalent in prolog of
                      ;; a choice point with failure at the end, for no more results.
                      ;; We could prune it or leave it here, either way
                      ;; [--> (in-hole EΓ (in-hole Ev ((⊤ σ) + (⊥ #f))))
                      ;;      (in-hole EΓ (in-hole Ev (⊤ σ)))
                      ;;      "prune failure from end"]

                      #;[--> (in-hole EΓ (in-hole Ev (⊥ #f)))
                           (in-hole EΓ (in-hole Ev ()))
                           "prune bald failure"]
))