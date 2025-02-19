#lang racket
(require redex)
(require (rename-in minikanren
                    (fresh fresh_)))
(require "definitions.rkt")

(provide reify to-json prog->tree)



(define-metafunction L
  term->mk : t -> any
  [(term->mk (t_1 : t_2)) ,(cons (term (term->mk t_1)) (term (term->mk t_2)))]
  [(term->mk empty) ()]
  [(term->mk c) ,(string->symbol (string-append "_" (number->string (term c))))]
  [(term->mk t) t])

(define (reify sub)
  (let* ([underscore (λ (n) (string->symbol (string-append "_" (number->string n))))]
         [freshen (λ (p) (underscore (car p)))]
         [unify (λ (p) `(== ,(if (= (car p) 0) 'q (underscore (car p)))
                            ,(term (term->mk ,(second p)))))])
    (car (eval `(run* (q) (fresh_ ,(map freshen sub) ,@(map unify sub)))))))
                           

(define (extract-name input-str)
  (define re #px"^[x,r]:([a-zA-Z]+)") ;; (x or r):letters ; Stops at the <<...>>
  (define matches (regexp-match re input-str))
  (if matches (second matches) input-str))

(define-metafunction L
  list->json : any -> any
  [(list->json (t_1 : t_2))
   ,(string-append (term (term->json t_1))
                   ", "
                   (term (list->json t_2)))]
  [(list->json t) (term->json t)]
  [(list->json (t)) (term->json t)]
  [(list->json (t_1 t_2 ...))
   ,(string-append (term (term->json t_1))
                   ", "
                   (term (list->json (t_2 ...))))])


 
(define-metafunction L
  term->json : t -> any
  [(term->json c) ,(number->string (term c))]
  [(term->json #t) "\"#t\""]
  [(term->json #f) "\"#f\""]
  [(term->json string) ,(string-append "\"" (term string) "\"")]
  [(term->json x) ,(string-append "\"" (extract-name (symbol->string (term x))) "\"")]
  [(term->json empty) "\"empty\""]
  [(term->json (t_1 : t_2))
   ,(string-append
     "["
     (term (list->json (t_1 : t_2)))
     "]")])

(define-metafunction L
  sub->json : sub -> string
  [(sub->json ()) ""]
  [(sub->json ((c t)))
   ,(string-append
    "{\"key\": " (number->string (term c))
    ", \"value\": " (term (term->json t)) "}")] 
  [(sub->json ((c t) (c_1 t_1) ...))
   ,(string-append
    "{\"key\": " (number->string (term c))
    ", \"value\": " (term (term->json t)) "}, "
    (term (sub->json (state ((c_1 t_1) ...) c_2 any))))])

(define-metafunction L
  goal->json : g -> string
  [(goal->json ⊤)
   "{\"name\": \"Succeed\"}"]

  [(goal->json (t_1 =? t_2 _))
   ,(let* ([left-json (term (term->json  t_1))]
           [right-json (term (term->json t_2))])
      (string-append
       "{\"name\": \"Unify\", "
       "\"left\": " left-json ", "
       "\"right\": " right-json "}"))]

  [(goal->json (r t ...))
   ,(let* ([rel-name (extract-name (symbol->string (term r)))]
           [args-json (term (list->json (t ...)))])
      (string-append
       "{\"name\": \"Rel-Call\", "
       "\"rel\": \"" rel-name "\", "
       "\"args\": [" args-json "]}"))]

  [(goal->json (g_1 ∨ g_2))
   ,(let* ([left-json (term (goal->json g_1))]
           [right-json (term (goal->json g_2))])
      (string-append
       "{\"name\": \"Goal-Disj\", "
       "\"children\": [" left-json ", "
                         right-json "]}"))]

  [(goal->json (g_1 ∧ g_2))
   ,(let* ([left-json (term (goal->json g_1))]
           [right-json (term (goal->json g_2))])
      (string-append
       "{\"name\": \"Goal-Conj\", "
       "\"children\": [" left-json ", "
                         right-json "]}"))]

  [(goal->json (∃ d g))
   ,(let* ([var-name  (term (list->json d))]
           [goal-json (term (goal->json g))])
      (string-append
       "{\"name\": \"Fresh\", "
       "\"vars\": [" var-name "], "
       "\"children\": [" goal-json "]}"))])

(define-metafunction L
  to-json : s -> string
  [(to-json ())
   "{\"name\": \"Empty\"}"]

  [(to-json (g (_ sub _ _)))
   ,(let* ([goal-json (term (goal->json g))]
           [sigma-json (term (sub->json sub))]
           #;[reified (reify (term sub))])
      (string-append
       (substring goal-json 0 (sub1 (string-length goal-json))) ", "
       "\"sub\": [" sigma-json "]}"
       #;"\"reified\": " #;reified #;"}"))]

  [(to-json (s_1 +-> s_2))
   ,(let* ([left-json (term (to-json s_1))]
           [right-json (term (to-json s_2))])
      (string-append
       "{\"name\": \"+->\", "
       "\"children\": [" left-json ", "
                         right-json "]}"))]

  [(to-json (s_1 <-+ s_2))
   ,(let* ([left-json (term (to-json s_1))]
           [right-json (term (to-json s_2))])
      (string-append
       "{\"name\": \"<-+\", "
       "\"children\": [" left-json ", "
                         right-json "]}"))]

  [(to-json ((⊤ (_ sub _ _)) + s))
   ,(let* ([sub-json (term (sub->json sub))]
           [rest-json (term (to-json s))])
      (string-append
       "{\"name\": \"Answer\", "
       "\"sub\": [" sub-json "], "
       "\"children\": [" rest-json "]}"))]

  [(to-json (s × g))
   ,(let* ([left-json (term (to-json s))]
           [right-json (term (goal->json g))])
      (string-append
       "{\"name\": \"Conjunction\", "
       "\"children\": [" left-json ", " right-json "]}"))]

  [(to-json (delay s))
   ,(let* ([tree-json (term (to-json s))])
      (string-append
       "{\"name\": \"Delay\", "
       "\"children\": [" tree-json "]}"))])

(define-metafunction L
  prog->tree : p -> e
  [(prog->tree (prog Γ e)) e])



