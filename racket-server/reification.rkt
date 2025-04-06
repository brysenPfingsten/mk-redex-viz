#lang racket
(require redex)

(require "definitions.rkt")
(provide reify)

(define-metafunction L
  term->mk : t -> any
  [(term->mk (t_1 : t_2)) (append (list (term->mk t_1)) (list (term->mk t_2)))] ; ugh these terms need to be quoted b/c they will be 
  [(term->mk empty) '()]                                        ; going through eval
  [(term->mk c) ,(string->symbol (string-append "_" (number->string (term c))))]
  [(term->mk (sym string)) (string->symbol ,(term string))]
  [(term->mk (nat natural)) natural]
  [(term->mk x) (term->json x)]
  [(term->mk t) t])

(define (extract-name input-str)
  (define re #px"^[x,r]:([a-zA-Z]+)") ;; (x or r):letters ; Stops at the <<...>>
  (define matches (regexp-match re input-str))
  (if matches (second matches) input-str))

(define-metafunction L
  list->list : any -> any
  [(list->list empty) ()]
  [(list->list (t_1 : t_2))
   ,(cons (term (term->json t_1))
          (term (list->list t_2)))]
  [(list->list t) ,(cons (term (term->json t)) '())])

(define-metafunction L
  term->json : t -> any
  [(term->json x) ,(hasheq 'var (extract-name (symbol->string (term x))))]
  [(term->json empty) ()]
  [(term->json (sym string)) ,(hasheq 'sym (term string))]
  [(term->json (nat natural)) ,(hasheq 'num (term natural))]
  [(term->json (t_1 : t_2)) (list->list (t_1 : t_2))]
  [(term->json t) t])

(define (deep-symbol->string x)
  (cond
    [(symbol? x) (symbol->string x)]
    [(list? x) (map deep-symbol->string x)]
    [(pair? x) (cons (deep-symbol->string (car x))
                     (deep-symbol->string (cdr x)))]
    [else x]))
  
(define (reify sub c)
  (if (empty? sub)
      #f
      (let* ([underscore (λ (n) (string->symbol (string-append "_" (number->string n))))]
             [freshen (map (λ (i) (string->symbol (string-append "_" (number->string i)))) (range 1 c))]
             [unify (λ (p) `(== ,(if (= (car p) 0) 'q (underscore (car p))) 
                                ,(term (term->mk ,(cadr p)))))]
             [ns (make-base-namespace)] ; Create a new namespace
             [_ (parameterize ([current-namespace ns]) (eval '(require minikanren)))] 
             [result (parameterize ([current-namespace ns])  ; Run eval inside this namespace
                       (car (eval `(run* (q) (fresh ,freshen ,@(map unify sub))))))]) ;; evil eval
        (deep-symbol->string result))))