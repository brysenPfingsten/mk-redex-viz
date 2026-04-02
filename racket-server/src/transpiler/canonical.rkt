#lang racket

(require "./ast.rkt"
         "./profile.rkt"
         "./program.rkt")

(provide parse-prog/canonical)

(define (id->label id)
  `(label ,id))

(define (konst->canonical-term const)
  (match const
    [(struct konst (s)) #:when (symbol? s) `(sym ,(symbol->string s))]
    [(struct konst (s)) #:when (string? s) `(str ,s)]
    [(struct konst (b)) #:when (boolean? b) b]
    [(struct konst (n)) #:when (number? n) `(nat ,n)]))

(define (unwrap-symbolish v who)
  (match v
    [(? symbol? sym) sym]
    [(struct var ((? symbol? sym))) sym]
    [(struct relname ((? symbol? sym))) sym]
    [_ (error who "expected symbol-like value, got ~a" v)]))

(define (next-hidden-id counter)
  (values (string-append "y" (number->string counter)) (add1 counter)))

(define (flatten-guid-groups reversed-guid-groups [acc '()])
  (match reversed-guid-groups
    ['() acc]
    [(cons guid-group rest)
     (flatten-guid-groups rest (foldr cons acc guid-group))]))

(define (transpile-canonical/list exprs count hidden-count [acc '()] [reversed-guid-groups '()])
  (match exprs
    ['()
     (values (reverse acc)
             count
             hidden-count
             (flatten-guid-groups reversed-guid-groups))]
    [(cons expr rest)
     (define-values (t-expr count^ hidden^ expr-guids)
       (transpile-canonical expr count hidden-count))
     (transpile-canonical/list rest
                               count^
                               hidden^
                               (cons t-expr acc)
                               (cons expr-guids reversed-guid-groups))]))

(define (transpile-canonical expr count hidden-count)
  (match expr
    [(struct prog (rels q))
     (define-values (trs count1 hidden1 guids1)
       (transpile-canonical/list rels count hidden-count))
     (define-values (tq count2 hidden2 guids2)
       (transpile-canonical q count1 hidden1))
     (values `(,trs ,tq) count2 hidden2 (append guids1 guids2))]

    [(struct fresh (vars goal))
     (define-values (id count1) (next-g-id "f" count))
     (define-values (tvars count2 hidden1 guids1)
       (transpile-canonical/list vars count1 hidden-count))
     (define-values (tgoal count3 hidden2 guids2)
       (transpile-canonical goal count2 hidden1))
     (values `(∃ ,tvars ,tgoal ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(struct conde (clauses))
     (define-values (id count1) (next-g-id "d" count))
     (struct acc (expr count hidden guids))
     (define final-acc
       (foldr
        (lambda (clause accum)
          (define-values (t-clause new-count new-hidden new-guids)
            (transpile-canonical clause
                                 (acc-count accum)
                                 (acc-hidden accum)))
          (acc (if (null? (acc-expr accum))
                   t-clause
                   `(,t-clause ∨ ,(acc-expr accum) ,(id->label id)))
               new-count
               new-hidden
               (append new-guids (acc-guids accum))))
        (acc '() count1 hidden-count '())
        clauses))
     (values (acc-expr final-acc)
             (acc-count final-acc)
             (acc-hidden final-acc)
             (cons id (acc-guids final-acc)))]

    [(struct conj (g1 g2))
     (define-values (id count1) (next-g-id "c" count))
     (define-values (tg1 count2 hidden1 guids1)
       (transpile-canonical g1 count1 hidden-count))
     (define-values (tg2 count3 hidden2 guids2)
       (transpile-canonical g2 count2 hidden1))
     (values `(,tg1 ∧ ,tg2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(struct disj (g1 g2))
     (define-values (id count1) (next-g-id "d" count))
     (define-values (tg1 count2 hidden1 guids1)
       (transpile-canonical g1 count1 hidden-count))
     (define-values (tg2 count3 hidden2 guids2)
       (transpile-canonical g2 count2 hidden1))
     (values `(,tg1 ∨ ,tg2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(struct unify (t1 t2))
     (define-values (id count1) (next-g-id "u" count))
     (define-values (tt1 count2 hidden1 guids1)
       (transpile-canonical t1 count1 hidden-count))
     (define-values (tt2 count3 hidden2 guids2)
       (transpile-canonical t2 count2 hidden1))
     (values `(,tt1 =? ,tt2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(struct diseq (t1 t2))
     (define-values (id count1) (next-g-id "n" count))
     (define-values (tt1 count2 hidden1 guids1)
       (transpile-canonical t1 count1 hidden-count))
     (define-values (tt2 count3 hidden2 guids2)
       (transpile-canonical t2 count2 hidden1))
     (values `(,tt1 != ,tt2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(struct succeed ())
     (values `(succeed (label "succeed")) count hidden-count '())]

    [(struct fail ())
     (values `(fail (label "fail")) count hidden-count '())]

    [(struct delay-goal (goal))
     (define-values (id count1) (next-g-id "y" count))
     (define-values (tg count2 hidden1 guids1)
       (transpile-canonical goal count1 hidden-count))
     (values `(suspend ,tg ,(id->label id))
             count2 hidden1
             (cons id guids1))]

    [(struct compiled-delay-goal (goal))
     (define-values (id hidden1) (next-hidden-id hidden-count))
     (define-values (tg count1 hidden2 guids1)
       (transpile-canonical goal count hidden1))
     (values `(suspend ,tg ,(id->label id))
             count1 hidden2
             guids1)]

    [(struct relcall (name terms))
     (define-values (id count1) (next-g-id "r" count))
     (define-values (tname count2 hidden1 guids1)
       (transpile-canonical name count1 hidden-count))
     (define-values (tterms count3 hidden2 guids2)
       (transpile-canonical/list terms count2 hidden1))
     (values `(,tname ,@tterms ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(struct nil ())
     (values 'empty count hidden-count '())]

    [(struct konst (_))
     (values (konst->canonical-term expr) count hidden-count '())]

    [(struct kons (a d))
     (define-values (ta count1 hidden1 guids1)
       (transpile-canonical a count hidden-count))
     (define-values (td count2 hidden2 guids2)
       (transpile-canonical d count1 hidden1))
     (values `(,ta : ,td) count2 hidden2 (append guids1 guids2))]

    [(struct var (v))
     (define v* (unwrap-symbolish v 'transpile-canonical))
     (values (string->symbol (string-append "x:" (symbol->string v*)))
             count hidden-count '())]

    [(struct relname (name))
     (define name* (unwrap-symbolish name 'transpile-canonical))
     (values (string->symbol (string-append "r:" (symbol->string name*)))
             count hidden-count '())]

    [(struct defrel (name lop goal))
     (define-values (tname count1 hidden1 guids1)
       (transpile-canonical name count hidden-count))
     (define-values (tlop count2 hidden2 guids2)
       (transpile-canonical/list lop count1 hidden1))
     (define-values (tgoal count3 hidden3 guids3)
       (transpile-canonical goal count2 hidden2))
     (values `(,tname ,tlop ,tgoal)
             count3 hidden3
             (append guids1 guids2 guids3))]

    [(struct run (_n qs goal))
     (define-values (id count1) (next-g-id "f" count))
     (define-values (tq count2 hidden1 guids1)
       (transpile-canonical/list qs count1 hidden-count))
     (define-values (tg count3 hidden2 guids2)
       (transpile-canonical goal count2 hidden1))
     (values `((∃ ,tq ,tg ,(id->label id))
               (state () () () () (label "s")))
             count3 hidden2
             (cons id (append guids1 guids2)))]))

(define (parse-prog/canonical lst
                              #:source-mode [source-mode default-source-mode]
                              #:compile-profile [compile-profile #f])
  (define-values (ast display-ast _profile)
    (prepare-program lst source-mode compile-profile))
  (define-values (canonical-prog _counter _hidden guid-list)
    (transpile-canonical ast 0 0))
  (define-values (html-prog _rest) (add-guids display-ast 0 guid-list))
  (values canonical-prog html-prog))
