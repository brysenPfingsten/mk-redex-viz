#lang racket
(require redex)

(require "definitions.rkt")
(provide reify term->mk)

(define-metafunction L
  term->mk : t -> any
  [(term->mk (t_1 : t_2)) (cons (term->mk t_1) (term->mk t_2))] ; ugh
  [(term->mk empty) '()]
  [(term->mk c) ,(string->symbol (string-append "_" (number->string (term c))))]
  [(term->mk x) (term->json x)]
  [(term->mk t) t])

(define (extract-name input-str)
  (define re #px"^[x,r]:([a-zA-Z]+)") ;; (x or r):letters ; Stops at the <<...>>
  (define matches (regexp-match re input-str))
  (if matches (second matches) input-str))

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

(define (reify sub c)
  (display sub)
  (flush-output)
  (if (empty? sub)
      "[]"
      (let* ([underscore (λ (n) (string->symbol (string-append "_" (number->string n))))]
             [freshen (cons 'q (map (λ (i) (string->symbol (string-append "_" (number->string i)))) (range 1 c)))]
             [unify (λ (p) `(== ,(if (= (car p) 0) 'q (underscore (car p))) 
                                ,(term (term->mk ,(second p)))))]
             [ns (make-base-namespace)] ; Create a new namespace
             [_ (parameterize ([current-namespace ns]) (eval '(require minikanren)))] 
             [result (parameterize ([current-namespace ns])  ; Run eval inside this namespace
                       (car (eval `(run* (q) (fresh ,freshen ,@(map unify sub))))))]) ;; evil eval
        (display result)
        (string-append "["
        (cond
          [(cons? result) (string-append "[" (string-join (map (λ (t) (term (term->json ,t))) result) ", ") "]")]
          [(symbol? result) (string-append "\"" (symbol->string result) "\"")]
          [else (term (term->json ,result))])
        "]"))))