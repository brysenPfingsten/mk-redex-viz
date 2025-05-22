#lang racket
(require redex)
(require "definitions.rkt")
(provide reify term->mk mk->json reified?
         generate-fresh-names generate-query-vars
         make-unify-clause prepare-minikanren-namespace
         process-reify-result)

;; Purpose: Converts a redex term to a miniKanren term
(define-metafunction L
  term->mk : t -> any
  [(term->mk (t_1 : t_2)) (list->list (t_1 : t_2))]
  [(term->mk empty) '()]
  [(term->mk c) ,(string->symbol (string-append "_" (number->string (term c))))]
  [(term->mk (sym string)) (quote ,(string->symbol (term string)))]
  [(term->mk (nat natural)) natural]
  [(term->mk t) t])

;; Purpose: Converts a redex list to a racket list
(define-metafunction L
  list->list : any -> any
  [(list->list empty) '()]
  [(list->list (t_1 : t_2))
   (cons (term->mk t_1)
         (list->list t_2))]
  [(list->list t) (cons (term->mk t) '())])


;; Any -> Boolean
;; True if the given arg is of the form '_.n, False otherwise
(define (reified? r)
  (and (symbol? r)
       (let ([s (symbol->string r)])
         (and (>= (string-length s) 2)
              (char=? (string-ref s 0) #\_)
              (char=? (string-ref s 1) #\.)))))


;; Any -> Any
;; Purpose: Converts the mk terms to JSON
(define (mk->json expr)
  (match expr
    [ref #:when (reified? ref) (symbol->string ref)]
    [sym #:when (symbol? sym) (hasheq 'sym (symbol->string sym))]
    [nat #:when (natural? nat) (hasheq 'num nat)]
    [(cons t1 t2) (cons (mk->json t1) (mk->json t2))]
    [_ expr]))

;; Nat -> Symbol
;; Purpose: Prefixes the given number with an underscore and makes it a symbol.
;;          Used to transform logic vars (nats) into a form that can be used by mk.
(define (underscore-symbol n)
  (string->symbol (string-append "_" (number->string n))))


;; Nat -> (ListOf Symbol)
;; Purpose: Generates the list '(_1 _2 ... _c-1)
(define (generate-fresh-names c)
  (map underscore-symbol (range 1 c)))


;; Nat -> (ListOf Symbol)
;; Purpose: Generates a list of n random symbols
(define (generate-query-vars n)
  (for/list ([_ (in-range n)]) (gensym)))


;; (ListOf Symbol) Nat (PairOf Term Term) -> (List Symbol Term Term)
;; Purpose: Creates a quoted equation unifying the given pair
(define (make-unify-clause query-vars n pair)
  (let* ([l      (car pair)]
         [r      (cadr pair)]
         [lhs    (if (< l n)
                     (list-ref query-vars l)
                     (underscore-symbol l))]
         [rhs    (if (and (number? r) (< r n))
                     (list-ref query-vars r)
                     (term (term->mk ,r)))])
    `(== ,lhs ,rhs)))


;; -> Namespace
;; Purpose: Creates a namespace with miniKanren required
(define (prepare-minikanren-namespace)
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'minikanren))
    ns))


;; namespace (ListOf Symbol) (ListOf Symbol) (List Symbol Term Term) -> Symbol or (ListOf Term)
;; Purpose: Constructs the mk program and runs it in the given namespace
(define (run-in-namespace ns query-vars fresh-names unify-clauses)
  (car (eval `(run* ,query-vars
                    (fresh ,fresh-names
                           ,@unify-clauses))
             ns)))


;; Symbol or (ListOf Term) -> JSON
;; Purpose: post-process the raw result into JSON
(define (process-reify-result result)
  (if (cons? result)
      (map mk->json result)
      (mk->json result)))


;; Sub Nat Nat -> JSON
;; Purpose: Reifies the given sub with logic variables up to c-1 using n query variables
(define (reify sub c n)
  (if (empty? sub)
      '()
      (let* ([fresh-names   (generate-fresh-names c)]
             [query-vars    (generate-query-vars n)]
             [unify-clauses (map (λ (p) (make-unify-clause query-vars n p)) sub)]
             [ns            (prepare-minikanren-namespace)]
             [raw-result    (run-in-namespace ns
                                              query-vars
                                              fresh-names
                                              unify-clauses)])
        (process-reify-result raw-result))))