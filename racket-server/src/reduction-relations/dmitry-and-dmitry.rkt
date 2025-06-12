#lang racket

(require redex redex/gui
         redex/reduction-semantics
         rackunit
         redex/pict)
(check-redundancy #t)

(provide dmitry-and-dmitry step-once)
(require "../definitions.rkt" "../judgment-forms.rkt")

;; Term -> [Listof [List String Term]]
(define (step-once prog)
  (apply-reduction-relation/tag-with-names dmitry-and-dmitry (term ,prog)))


(define dmitry-and-dmitry
  (reduction-relation L 
                      #:domain p ; (side-condition (name prog p) (judgment-holds (closed-program? prog)))

                      [==> ((t_1 =? t_2 o) (state sub c trail))
                           (∂ () #f)
                           (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "UnifyFail"]

                      [==> ((t_1 =? t_2 o) (state sub c ((t_3 =? t_4 o_1) ...)))
                           (∂ (⊤ (state sub_1 c ((t_3 =? t_4 o_1) ... (t_1 =? t_2 o)))) sub_1)
                           (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "UnifySuccess"]

                      [==> ((g_1 ∨ g_2 _) σ)
                           (∂ ((g_1 σ) <-+ (g_2 σ)) #f)
                           "Disj"]

                      [==> ((g_1 ∧ g_2 _) σ)
                           (∂ ((g_1 σ) × g_2) #f)
                           "Conj"]

                      [==> ((∃ (x) g _) (state sub c trail))
                           (∂ ((substitute g ,@(term (fresh-sub c x))) (state sub ,(+ 1 (term c)) trail)) #f)
                           "Fresh"]

                      [--> (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                  (in-hole Es ((r_1 t ... o) σ)))
                           (prog ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...) 
                                  (∂ (in-hole Es ((substitute g_1 (x_1 t) ...) σ)) #f))
                           "Invoke"]

                      [==> ((∂ () #f) <-+ s)
                           s
                           "DisjStopLeft"]
                      
                      [==> (s +-> (∂ () #f))
                           s
                           "DisjStopRight"]

                      [==> ((∂ (⊤ σ) sub) <-+ s)
                           s
                           "DisjStopAnsLeft"]

                      [==> (s +-> (∂ (⊤ σ) sub))
                           s
                           "DisjStopAnsRight"]

                      [==> ((∂ () #f) × g)
                           (∂ () #f)
                           "ConjStop"]

                      [==> ((∂ (⊤ σ) sub) × g)
                           (g σ)
                           "ConjStopAns"]
                      
                      [==> ((∂ s_1 #f) <-+ s_2) 
                           (s_1 +-> s_2) 
                           (where #f ,(redex-match? L () (term s_1)))
                           "DisjStepLeft"]

                      [==> (s_1 +-> (∂ s_2 #f))
                           (s_1 <-+ s_2)
                           (where #f ,(redex-match? L () (term s_2)))
                           "DisjStepRight"]

                      [==> ((∂ s_1 sub) <-+ s_2)
                           (s_1 +-> s_2)
                           (where #f ,(redex-match? L (⊤ σ) (term s_1)))
                           "DisjStepAnsLeft"]

                      [==> (s_1 +-> (∂ s_2 sub))
                           (s_1 <-+ s_2)
                           (where #f ,(redex-match? L (⊤ σ) (term s_2)))
                           "DisjStepAnsRight"]

                      [==> ((∂ s #f) × g)
                           (s × g)
                           (where #f ,(redex-match? L () (term s)))
                           "ConjStep"]

                      [==> (((∂ (⊤ σ) sub) <-+ s) × g)
                           (((⊤ σ) × g) <-+ (s × g))
                           "ConjStepAnsLeft"]

                      [==> ((s +-> (∂ (⊤ σ) sub)) × g)
                           ((s × g) +-> ((⊤ σ) × g))
                           "ConjStepAnsRight"]
                      
                      [--> (in-hole EΓ (∂ s _))
                           (in-hole EΓ s)
                           "TopStep"]

                      with
                      [(--> (in-hole Ex a) (in-hole Ex b))
                            (==> a b)]
                      ))

(stepper dmitry-and-dmitry '(prog
  ((r:appendo
    (x:l x:s x:out)
    (((x:l =? empty "u2") ∧ (x:out =? x:s "u3") "c1") ∨ (∃ (x:a) (∃ (x:d) (∃ (x:res) (((x:l =? (x:a : x:d) "u9") ∧ (x:out =? (x:a : x:res) "u10") "c8") ∧ (r:appendo x:d x:s x:res "r11") "c7") "f6") "f5") "f4") "d0")))
  ((∃ (x:q) (r:appendo ((sym "dog") : ((sym "cat") : empty)) ((sym "bear") : ((sym "lion") : empty)) x:q "r13") "f12") (state () 0 ()))))
