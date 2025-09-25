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
                      #:domain (side-condition (name prog p) (judgment-holds (closed-program? prog)))

                      [--> ((in-hole Ex ((r_1 t ... o) σ))
                            ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...))
                           ((in-hole Ex (∂ ((substitute g_1 (x_1 t) ...) σ) #f))
                            ((r_0 (x_0 ...) g_0) ... (r_1 (x_1 ...) g_1) (r_2 (x_2 ...) g_2) ...))
                           "Invoke"]

                      [--> (e_1 Γ)
                           (e_2 Γ)
                        (side-condition (not (null? (apply-reduction-relation red-tree (term e_1)))))
                        (where e_2 ,(car (apply-reduction-relation red-tree (term e_1))))
                        (computed-name (caar (apply-reduction-relation/tag-with-names red-tree (term e_1))))]))

(define red-tree
  (reduction-relation L 
                      #:domain e

                      [==> ((t_1 =? t_2 o) (state sub _ _ _))
                           (∂ () #f)
                           (where #f (unify (walk t_1 sub) (walk t_2 sub) sub))
                           "UnifyFail"]

                      [==> ((t_1 =? t_2 o) (state sub c trail o_1))
                           (∂ (⊤ σ) σ)
                           (where sub_1 (unify (walk t_1 sub) (walk t_2 sub) sub))
                           (where σ (state sub_1 c (,@(term trail) (t_1 =? t_2 o)) o_1))
                           "UnifySuccess"]

                      [==> ((g_1 ∨ g_2 _) (state sub c trail o))
                           (∂ ((g_1 (state sub c trail o)) <-+ (g_2 (state sub c trail ,(symbol->string (gensym))))) #f)
                           "Disj"]

                      [==> ((g_1 ∧ g_2 _) σ)
                           (∂ ((g_1 σ) × g_2) #f)
                           "Conj"]

                      [==> ((∃ (x) g _) (state sub c trail o))
                           (∂ ((substitute g ,@(term (fresh-sub c x))) (state sub ,(+ 1 (term c)) trail o)) #f)
                           "Fresh"]


                      [==> ((∂ () #f) <-+ s)
                           (∂ s #f)
                           "DisjStopLeft"]
                      
                      [==> (s +-> (∂ () #f))
                           (∂ s #f)
                           "DisjStopRight"]

                      [==> ((∂ (⊤ σ) σ) <-+ s)
                           (∂ s σ)
                           "DisjStopAnsLeft"]

                      [==> (s +-> (∂ (⊤ σ) σ))
                           (∂ s σ)
                           "DisjStopAnsRight"]

                      [==> ((∂ () #f) × g)
                           (∂ () #f)
                           "ConjStop"]

                      [==> ((∂ (⊤ σ) σ) × g)
                           (∂ (g σ) #f)
                           "ConjStopAns"]
                      
                      [==> ((∂ s_1 #f) <-+ s_2) 
                           (∂ (s_1 +-> s_2) #f)
                           (where #f ,(redex-match? L () (term s_1)))
                           "DisjStepLeft"]

                      [==> (s_1 +-> (∂ s_2 #f))
                           (∂ (s_1 <-+ s_2) #f)
                           (where #f ,(redex-match? L () (term s_2)))
                           "DisjStepRight"]

                      [==> ((∂ s_1 σ) <-+ s_2)
                           (∂ (s_1 +-> σ) sub)
                           (where #f ,(redex-match? L (⊤ σ) (term s_1)))
                           "DisjStepAnsLeft"]

                      [==> (s_1 +-> (∂ s_2 σ))
                           (∂ (s_1 <-+ s_2) σ)
                           (where #f ,(redex-match? L (⊤ σ) (term s_2)))
                           "DisjStepAnsRight"]

                      [==> ((∂ s #f) × g)
                           (∂ (s × g) #f)
                           (where #f ,(redex-match? L () (term s)))
                           "ConjStep"]

                      [==> ((∂ s σ) × g)
                           (∂ ((g σ) <-+ (s × g)) #f)
                           (where #f ,(redex-match? L (⊤ σ) (term s)))
                           "ConjStepAns"]

                      [--> (in-hole Ev (∂ (⊤ σ) σ))
                           (in-hole Ev ((⊤ σ) + ()))
                           "TopStopAns"]

                      [--> (in-hole Ev (∂ s σ))
                           (in-hole Ev ((⊤ σ) + s))
                           (where #f ,(redex-match? L (⊤ σ) (term s)))
                           "TopStepAns"]

                      [--> (in-hole Ev (∂ s #f))
                           (in-hole Ev s)
                           "TopStep"]

                      with
                      [(--> (in-hole Ex a) (in-hole Ex b))
                            (==> a b)]
                      ))

#;(stepper dmitry-and-dmitry 
'(((∃ (x:q) (r:appendo ((sym "dog") : ((sym "cat") : empty)) ((sym "bear") : ((sym "lion") : empty)) x:q "r13") "f12") (state () 0 () "s"))
  ((r:appendo
    (x:l x:s x:out)
    (((x:l =? empty "u2") ∧ (x:out =? x:s "u3") "c1") ∨ (∃ (x:a) (∃ (x:d) (∃ (x:res) (((x:l =? (x:a : x:d) "u9") ∧ (x:out =? (x:a : x:res) "u10") "c8") ∧ (r:appendo x:d x:s x:res "r11") "c7") "f6") "f5") "f4") "d0")))
  ))

#;(stepper dmitry-and-dmitry '(
((∃ (x:a) (∃ (x:b) (r:maino x:a x:b "r9") "f8") "f7") (state () 0 () "s"))
  ((r:m (x:q) ((sym "m") =? (sym "m") "u0"))
   (r:maino
    (x:p x:q)
    (∃
     (x:z)
     ((((sym "a") =? x:q "u4") ∨ ((sym "b") =? x:q "u5") "d3")
      ∧
      ((sym "m") =? x:p "u6")
      "c2")
     "f1")))
  )
)
