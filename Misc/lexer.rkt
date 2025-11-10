#lang racket
(require parser-tools/lex)

;-----------------Structures-------------------
(struct prog (relations query) #:transparent)
(struct fresh (vars goal) #:transparent)
(struct conde (clauses) #:transparent)
(struct disj (g1 g2) #:transparent)
(struct conj (g1 g2) #:transparent)
(struct unify (t1 t2) #:transparent)
(struct succeed () #:transparent)
(struct fail () #:transparent)
(struct relcall (name terms) #:transparent)
(struct nil () #:transparent)
(struct konst (k) #:transparent)
(struct kons (a d) #:transparent)
(struct var (v) #:transparent)
(struct relname (name) #:transparent)
(struct defrel (name lop goal) #:transparent)
(struct run (n q goal) #:transparent)
;-----------------------------------------------

(define sample-input
  "(defrel (appendo l s out)
  (conde
    [(== l '()) (== s out)]
    [(fresh (a d res)
       (== l (cons a d))
       (== out (cons a res))
       (appendo d s res))]))

(run* (q) (appendo (list 'dog 'cat) `(bear lion) q))")

(define (get-tokens a-lexer)
  (define p (open-input-string sample-input))
  (define (get-tokens-help)
    (let ([next (a-lexer p)])
      (if (equal? next eof)
          '()
          (cons next (get-tokens-help)))))
  (get-tokens-help))

(define the-lexer/primitive
  (lexer [(eof) eof]
         ["(" 'lp]
         [")" 'rp]
         ["[" 'lb]
         ["]" 'rb]
         ["'()" 'mt]
         ["==" 'unify]
         ["defrel" 'defrel]
         ["fresh" 'fresh]
         ["conde" 'conde]
         ["run" 'run-n]
         ["run*" 'run-all]
         ["'" 'sym]
         ["`" 'quasi]
         ["." 'dot]
         [(repetition 1 +inf.0 " ") lexeme]
         ["\n" 'newline]
         [(repetition 1 +inf.0 numeric) 
          (string->number lexeme)]
         [(concatenation (union alphabetic #\_) (repetition 0 +inf.0 (union alphabetic numeric #\_)))
          lexeme]
         [whitespace lexeme]))
(get-tokens the-lexer/primitive)

'(lp defrel " " lp "appendo" " " "l" " " "s" " " "out"rp"\n" " " " "lpde"\n" " " " " " " " "lblpfy " " "l" " "mtrp " "
     lpfy " " "s" " " "out"rprb"\n" " " " " " " " "lblpsh " "lp "a" " " "d" " " "res"rp"\n" " " " " " " " " " " " " " "
     lpfy " " "l" " "lp "cons" " " "a" " " "d"rprp"\n" " " " " " " " " " " " " " "lpfy " " "out" " "lp "cons" " "
     "a" " " "res"rprp"\n" " " " " " " " " " " " " " "lp "appendo" " " "d" " " "s" " " "res"rprprbrprp
     "\n""\n"lpll " "lp "q"rp " "lp "appendo" " "lp "list" " "ym "dog" " "ym "cat"rp " "silp "bear" " " "lion"rp " " "q"rp
     rp)

;; (ListOf Primitives) --> CST
;; Purpose: Parse the list of tokens into a CST
(define (tokens->CST tokens)
  (match tokens
    [n #:when (number? n) n]
    []
    [`(lp unify ,a ,b rp) (list "unify" (list (tokens->CST a)
                                              (tokens->CST b)))]))


