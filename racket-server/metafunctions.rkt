#lang racket
(require redex
         json)
(require (prefix-in mk: minikanren)) 

(require "definitions.rkt" "reification.rkt")

(provide to-json prog->tree)

(define-metafunction L
  prog->tree : p -> e
  [(prog->tree (prog Γ e)) e])

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
  [(term->json x) ,(string-append "{\"var\": \"" (extract-name (symbol->string (term x))) "\"}")]
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
     (term (sub->json ((c_1 t_1) ...))))])

(define-metafunction L
  crumb->json : (t =? t o) sub -> string
  [(crumb->json (t_1 =? t_2 o) sub)
   ,(let* ([left (term (term->json (walk t_1 sub)))]
           [right (term (term->json (walk t_2 sub)))]
           [id (term (term->json o))])
      (string-append
       "{\"left\": " left ", "
       "\"right\": " right ", "
       "\"id\": " id "}"))])

(define-metafunction L
  trail->json : trail sub -> string
  [(trail->json () _) ""]
  [(trail->json ((t_1 =? t_2 o)) sub) (crumb->json (t_1 =? t_2 o) sub)]
  [(trail->json ((t_1 =? t_2 o_1) (t_3 =? t_4 o_2) ...) sub)
   ,(string-append (term (crumb->json (t_1 =? t_2 o_1) sub))
                   ", "
                   (term (trail->json ((t_3 =? t_4 o_2) ...) sub)))]) 

(define-metafunction L
  goal->json : g -> string
  [(goal->json ⊤)
   "{\"name\": \"Succeed\"}"]

  [(goal->json (t_1 =? t_2 o))
   ,(let* ([left-json (term (term->json  t_1))]
           [right-json (term (term->json t_2))])
      (string-append
       "{\"name\": \"Unify\", "
       "\"id\": \"" (term o) "\", "
       "\"left\": " left-json ", "
       "\"right\": " right-json "}"))]

  [(goal->json (r t ... o))
   ,(let* ([rel-name (extract-name (symbol->string (term r)))]
           [args-json (term (list->json (t ...)))])
      (string-append
       "{\"name\": \"Rel-Call\", "
       "\"id\": \"" (term o) "\", "
       "\"rel\": \"" rel-name "\", "
       "\"args\": [" args-json "]}"))]

  [(goal->json (g_1 ∨ g_2 o))
   ,(let* ([left-json (term (goal->json g_1))]
           [right-json (term (goal->json g_2))])
      (string-append
       "{\"name\": \"Goal-Disj\", "
       "\"id\": \"" (term o) "\", "  
       "\"children\": [" left-json ", "
       right-json "]}"))]

  [(goal->json (g_1 ∧ g_2 o))
   ,(let* ([left-json (term (goal->json g_1))]
           [right-json (term (goal->json g_2))])
      (string-append
       "{\"name\": \"Goal-Conj\", "
        "\"id\": \"" (term o) "\", "
       "\"children\": [" left-json ", "
       right-json "]}"))]

  [(goal->json (∃ d g o))
   ,(let* ([var-name (term (list->json d))]
           [goal-json (term (goal->json g))])
      (string-append
       "{\"name\": \"Fresh\", "
       "\"id\": \"" (term o) "\", "
       "\"vars\": [" var-name "], "
       "\"children\": [" goal-json "]}"))])

(define-metafunction L
  to-json : s -> string
  [(to-json ())
   "{\"name\": \"Empty\"}"]

  [(to-json (g (_ sub c trail)))
   ,(let* ([goal-json (term (goal->json g))]
           [sigma-json (term (sub->json sub))]
           [trail-json (term (trail->json trail sub))]
           [reified (reify (term sub) (term c))])
      (string-append
       (substring goal-json 0 (sub1 (string-length goal-json))) ", "
       "\"sub\": [" sigma-json "], "
       "\"trail\": [" trail-json "], "
       "\"reified\": " reified "}"))]

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

  [(to-json ((⊤ (_ sub c trail)) + s))
   ,(let* ([sub-json (term (sub->json sub))]
           [rest-json (term (to-json s))]
           [trail-json (term (trail->json trail sub))]
           [reified (reify (term sub) (term c))])
      (string-append
       "{\"name\": \"Answer\", "
       "\"sub\": [" sub-json "], "
       "\"trail\": [" trail-json "], "
       "\"reified\": " reified ", "
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