#lang racket
(require redex
         json
         racket/hash
         "definitions.rkt"
         "reification.rkt")

(provide to-json prog->tree num-query-vars)

(define num-of-query-vars 'uninitialized)
(define (set-num-query-vars! n)
  (set! num-of-query-vars n))

(define (extract-name input-str)
  (define re #rx"^[x,r]:([^«]+)") ;; (x or r):letters ; Stops at the <<...>>
  (define matches (regexp-match re input-str))
  (if matches (second matches) input-str))

(define-metafunction L
  list->list : any -> any
  [(list->list empty) ()]
  [(list->list (t_1 : t_2))
   ,(cons (term (term->json t_1))
          (term (list->list t_2)))]
  [(list->list (t)) ,(cons (term (term->json t)) '())]
  [(list->list t) ,(cons (term (term->json t)) '())])

(define-metafunction L
  term->json : t -> any
  [(term->json x) ,(hasheq 'var (extract-name (symbol->string (term x))))]
  [(term->json empty) ()]
  [(term->json (sym string)) ,(hasheq 'sym (term string))]
  [(term->json (nat natural)) ,(hasheq 'num (term natural))]
  [(term->json (t_1 : t_2)) (list->list (t_1 : t_2))]
  [(term->json t) t])

(define (args->json vars)
  (map (λ (v) (term (term->json ,v))) vars))

(define (sub->json sub)
  (map (λ (p) (let ([var (car p)]
                    [val (term (term->json ,(cadr p)))])
                (hasheq 'key var
                        'value val)))
       sub))

(define (trail->json trail sub)
  (map (λ (crumb) (let ([left (term (term->json ,(first crumb)))]
                        [right (term (term->json ,(third crumb)))]
                        [id (fourth crumb)])
                    (hasheq 'left left
                            'right right
                            'id id)))
       trail))

(define-metafunction L
  goal->json : g -> any
  
  [(goal->json ⊤)
   ,(hasheq 'name "Succeed")]

  [(goal->json (t_1 =? t_2 o))
   ,(let* ([left-json (term (term->json  t_1))]
           [right-json (term (term->json t_2))])
      (hasheq 'name "Unify"
              'id (term o)
              'left left-json
              'right right-json))]

  [(goal->json (r t ... o))
   ,(let* ([rel-name (extract-name (symbol->string (term r)))]
           [args-json (args->json (term (t ...)))])
      (hasheq 'name "Rel-Call"
              'id (term o)
              'rel rel-name
              'args args-json))]

  [(goal->json (g_1 ∨ g_2 o))
   ,(let* ([left-json (term (goal->json g_1))]
           [right-json (term (goal->json g_2))])
      (hasheq 'name "Goal-Disj"
              'id (term o)
              'children (list left-json right-json)))]

  [(goal->json (g_1 ∧ g_2 o))
   ,(let* ([left-json (term (goal->json g_1))]
           [right-json (term (goal->json g_2))])
      (hasheq 'name "Goal-Conj"
              'id (term o)
              'children (list left-json right-json)))]

  [(goal->json (∃ d g o))
   ,(let* ([vars-json (args->json (term d))]
           [goal-json (term (goal->json g))])
      (hasheq 'name "Fresh"
              'id (term o)
              'vars vars-json
              'children (list goal-json)))])

(define-metafunction L
  tree->json : s natural -> any
  [(tree->json () _)
   ,(hasheq 'name "Empty")]

  [(tree->json (g (_ sub c trail o)) natural)
   ,(let* ([goal-json (term (goal->json g))]
           [sub-json (sub->json (term sub))]
           [trail-json (trail->json (term trail) (term sub))]
           [reified (reify (term sub) (add1 (term c)) (term natural))])
      (hash-union goal-json
                  (hasheq
                   'stateId (term o)
                   'sub sub-json
                   'trail trail-json
                   'reified reified)))]

  [(tree->json (∂ s maybe-state) natural)
   ,(let* ([tree-json (term (tree->json s natural))]
           [sub-json (if (term maybe-state) #t #f)])
      (hash-union tree-json
                  (hasheq
                    'partial #t
                    'hasAnswer sub-json)))]

  [(tree->json (proceed ((r t ... o) (_ sub c trail o_1))) natural)
   ,(let* ([goal-json (term (goal->json  (r t ...  o)))]
           [sub-json (sub->json (term sub))]
           [trail-json (trail->json (term trail) (term sub))]
           [reified (reify (term sub) (add1 (term c)) (term natural))])
      (hasheq 'name "Proceed"
              'id (term o)
              'stateId (term o_1)
              'goal goal-json
              'sub sub-json
              'trail trail-json
              'refied reified))]

  [(tree->json (s_1 +-> s_2) natural)
   ,(let* ([left-json (term (tree->json s_1 natural))]
           [right-json (term (tree->json s_2 natural))])
      (hasheq 'name "+->"
              'children (list left-json right-json)))]

  [(tree->json (s_1 <-+ s_2) natural)
   ,(let* ([left-json (term (tree->json s_1 natural))]
           [right-json (term (tree->json s_2 natural))])
      (hasheq 'name "<-+"
              'children (list left-json right-json)))]

  [(tree->json ((⊤ (_ sub c trail o)) + ()) natural)
   ,(let* ([sub-json (sub->json (term sub))]
           [trail-json (trail->json (term trail) (term sub))]
           [reified (reify (term sub) (term c) (term natural))])
      (hasheq 'name "Answer"
              'stateId (term o)
              'sub sub-json
              'trail trail-json
              'reified reified))]

  [(tree->json ((⊤ (_ sub c trail o)) + s) natural)
   ,(let* ([sub-json (sub->json (term sub))]
           [rest-json (term (tree->json s natural))]
           [trail-json (trail->json (term trail) (term sub))]
           [reified (reify (term sub) (term c) (term natural))])
      (hasheq 'name "Answer"
              'stateId (term o)
              'sub sub-json
              'trail trail-json
              'reified reified
              'children (list rest-json)))]

  [(tree->json (s × g) natural)
   ,(let* ([left-json (term (tree->json s natural))]
           [right-json (term (goal->json g))])
      (hasheq 'name "Conjunction"
              'children (list left-json right-json)))]

  [(tree->json (delay s) natural)
   ,(let* ([children (term (tree->json s natural))])
      (hasheq 'name "Delay"
              'children (list children)))])

(define-metafunction L
  prog->tree : p -> e
  [(prog->tree (e Γ)) e])

(define (to-json prog num-query-variables)
  (jsexpr->string (term (tree->json (prog->tree ,prog) ,num-query-variables))))

(define-metafunction L
  extract-query-vars : p -> d
  [(extract-query-vars (((∃ d _ _) _) _)) d])

(define (num-query-vars prog)
  (length (term (extract-query-vars ,prog))))
