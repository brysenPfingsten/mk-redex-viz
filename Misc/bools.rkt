#lang racket
(require redex/reduction-semantics rackunit redex/gui)

(define-language bools
  (Vt ::= tru (λ (x) B))
  (V ::= fals Vt)
  (B ::= x V (or B B) (let (x B) B) (B B) (and B B) (not B))
  (E ::=  hole (let (x E) B) (or E B) (and E B) (not E) #;B)
  ; (x y ::= variable-not-otherwise-mentioned)
  [x (variable-prefix x:)]
  [y (variable-prefix x:)]
  
  #:binding-forms
  (λ (x) B #:refers-to x)
  (let (y B_1) B_2 #:refers-to y))

(alpha-equivalent? bools
          `(λ (x:a) x:a)
          `(λ (x:b) x:b))

(define ->
  (reduction-relation
   bools
   #:domain B
   #:codomain B
   (--> ((λ (x) B_1) B_2) (substitute B_1 x B_2))
   (--> (or tru B_1) tru)
   (--> (or fals B_1) B_1)
   (--> (and tru B_1) B_1)
   (--> (and fals B_1) fals)
   (--> (not fals) tru)
   (--> (not Vt_1) fals) ;; general not
   (--> (let (x B_1) B_2) (substitute B_2 x B_1))
   #;(--> (or tru B_1) B_1)))

(define ->* (compatible-closure -> bools B))
(define ->*wrong (compatible-closure -> bools V))
(define ->cbv (context-closure -> bools E))

(define-metafunction bools
  bools-eval : B -> V
  [(bools-eval B)
   ,(car (apply-reduction-relation* ->cbv (term B)))])

(define-metafunction bools
  normalize : B -> V
  [(normalize B)
   ,(car (apply-reduction-relation* ->* (term B)))])

(define-judgment-form
  bools
  #:contract (=== B (B ...))
  #:mode (=== I I)

  [(where (B B) ((normalize B_1) (normalize B_2)))
   ----------------"cc normalized"
   (=== B_1 B_2)])

(define-judgment-form
  bools
  #:contract (cbv= B B)
  #:mode (cbv= I I)

  [(where (B B) ((bools-eval B_1) (bools-eval B_2)))
   ----------------"cbv reduced"
   (cbv= B_1 B_2)])

(define-judgment-form
  bools
  #:contract (=/= B B)
  #:mode (=/= I I)

  [(where (B_!_7 B_!_7 #;B_8 #;B_8) ((bools-eval B_1) (bools-eval B_2) #;(normalize B_1) #;(normalize B_2)))
  ------------------------
  (=/= B_1 B_2)])

(define-judgment-form
  bools
  #:contract (test= B B)
  #:mode (test= I I)

  [
  ------------------------
  (test= B_1 B_2)])

(judgment-holds
 (=/= (λ (x:a) ((λ (x:b) x:b) x:a))
      (λ (x:c) x:c)))
 


;; DO NOT QUOTE JUDGMENT EXPRS
(judgment-holds
 (=== (λ (x:a) ((λ (x:b) x:b) x:a))
      ((λ (x:c) x:c))))

(judgment-holds
 (cbv= (λ (x:a) ((λ (x:b) x:b) x:a))
      (λ (x:c) x:c)))

(check-not-false (redex-match bools B (term (let (x:y tru) (or x:y x:y)))))
(check-not-false (redex-match bools V (first (apply-reduction-relation -> (term (or tru fals))))))
(check-not-false (redex-match bools B (term fals)))
; (check-false (redex-match bools B (term false)))

(default-language bools)
(test-->>∃ ->cbv ;#:equiv alpha-equivalent?
          `((λ (x:a) x:a) tru)
          `tru)
(test--> ->* #:equiv alpha-equivalent?
          `((λ (x:a) x:a) tru)
          `tru)
(test-->> ->* #:equiv alpha-equivalent?
          `(or ((λ (x:a) x:a) tru) ((λ (x:b) x:b) fals))
          `tru)
(test-->> ->* #:equiv alpha-equivalent?
          `((λ (x:a) (λ (x:b) x:b)) tru)
          `(λ (x:b) x:b))
(test-->> ->* #:equiv alpha-equivalent?
          `((λ (x:a) (λ (x:b) x:b)) tru)
          `(λ (x:c) x:c))



          