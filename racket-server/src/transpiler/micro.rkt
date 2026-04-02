#lang racket

(require "./ast.rkt"
         "./profile.rkt")

(provide prepare-micro-program
         program->micro-string)

(define (reserved-goal-symbol? sym)
  (and (symbol? sym)
       (member sym '(fresh conde conj disj Zzz delay == =/= succeed fail proceed))))

(define (parse-run/micro r)
  (match r
    [`(run ,n (,q ..1) ,goal)
     (run n (map var q) (parse-goal/micro goal))]
    [`(run* (,q ..1) ,goal)
     (run +inf.0 (map var q) (parse-goal/micro goal))]
    [`(run ,_ (,q ..1) . ,rest)
     (error 'parse-run/micro
            "micro source run expects exactly one query goal, got ~a"
            (length rest))]
    [`(run* (,q ..1) . ,rest)
     (error 'parse-run/micro
            "micro source run* expects exactly one query goal, got ~a"
            (length rest))]
    [_ (error 'parse-run/micro "invalid micro run form: ~e" r)]))

(define (parse-relation-def/micro a-relation)
  (match a-relation
    [`(defrel (,r . ,params) ,goal)
     (defrel (relname r)
             (map var params)
             (parse-goal/micro goal))]
    [`(defrel (,r . ,params) . ,rest)
     (error 'parse-relation-def/micro
            "micro source defrel expects exactly one body goal, got ~a"
            (length rest))]
    [_ (error 'parse-relation-def/micro "invalid micro defrel form: ~e" a-relation)]))

(define (parse-goal/micro goal)
  (match goal
    [`(fresh ,vars ,g)
     (fresh (map var vars) (parse-goal/micro g))]
    [`(conj ,g1 ,g2)
     (conj (parse-goal/micro g1) (parse-goal/micro g2))]
    [`(disj ,g1 ,g2)
     (disj (parse-goal/micro g1) (parse-goal/micro g2))]
    [`(Zzz ,g)
     (delay-goal (parse-goal/micro g))]
    [`(== ,t1 ,t2)
     (unify (parse-term t1) (parse-term t2))]
    [`(=/= ,t1 ,t2)
     (diseq (parse-term t1) (parse-term t2))]
    ['succeed (succeed)]
    ['fail (fail)]
    [`(,r . ,terms)
     #:when (and (symbol? r) (not (reserved-goal-symbol? r)))
     (relcall (relname r) (map parse-term terms))]
    [`(fresh ,_ . ,rest)
     (error 'parse-goal/micro
            "micro fresh expects exactly one body goal, got ~a"
            (length rest))]
    [`(Zzz . ,rest)
     (error 'parse-goal/micro
            "micro Zzz expects exactly one goal, got ~a"
            (length rest))]
    [`(delay . ,_)
     (error 'parse-goal/micro
            "direct micro source uses Zzz for explicit goal delay; delay is internal-only")]
    [`(conde . ,_)
     (error 'parse-goal/micro "conde is not part of direct micro source mode")]
    [`(proceed . ,_)
     (error 'parse-goal/micro "proceed is internal-only and not valid in micro source mode")]
    [_ (error 'parse-goal/micro "invalid micro goal form: ~e" goal)]))

(define (term->micro-datum t)
  (match t
    [(struct konst ((? symbol? k))) `(quote ,k)]
    [(struct konst ((? string? k))) k]
    [(struct konst ((? boolean? k))) k]
    [(struct konst ((? number? k))) k]
    [(struct konst (k))
     (error 'term->micro-datum "unexpected konst payload: ~e" k)]
    [(struct nil ()) '(quote ())]
    [(struct kons (a d)) `(cons ,(term->micro-datum a)
                                ,(term->micro-datum d))]
    [(struct var (v)) v]
    [(struct relname (name)) name]
    [_ (error 'term->micro-datum "unexpected term AST: ~e" t)]))

(define (goal->micro-datum g)
  (match g
    [(struct unify (t1 t2))
     `(== ,(term->micro-datum t1)
          ,(term->micro-datum t2))]
    [(struct diseq (t1 t2))
     `(=/= ,(term->micro-datum t1)
           ,(term->micro-datum t2))]
    [(struct succeed ()) 'succeed]
    [(struct fail ()) 'fail]
    [(struct fresh (vars goal))
     `(fresh ,(map term->micro-datum vars)
        ,(goal->micro-datum goal))]
    [(struct conj (g1 g2))
     `(conj ,(goal->micro-datum g1)
            ,(goal->micro-datum g2))]
    [(struct disj (g1 g2))
     `(disj ,(goal->micro-datum g1)
            ,(goal->micro-datum g2))]
    [(struct delay-goal (goal))
     `(Zzz ,(goal->micro-datum goal))]
    [(struct relcall (name terms))
     `(,(term->micro-datum name)
       ,@(map term->micro-datum terms))]
    [(struct compiled-delay-goal (goal))
     `(Zzz ,(goal->micro-datum goal))]
    [_ (error 'goal->micro-datum "unexpected goal AST: ~e" g)]))

(define (datum->pretty-string datum)
  (define out (open-output-string))
  (pretty-write datum out)
  (regexp-replace #px"\n$" (get-output-string out) ""))

(define (relation->micro-string rel)
  (match rel
    [(defrel name vars goal)
     (datum->pretty-string
      `(defrel (,(term->micro-datum name) ,@(map term->micro-datum vars))
         ,(goal->micro-datum goal)))]
    [_ (error 'relation->micro-string "unexpected relation AST: ~e" rel)]))

(define (run->micro-string q)
  (match q
    [(run n vars goal)
     (datum->pretty-string
      (if (equal? n +inf.0)
          `(run* ,(map term->micro-datum vars) ,(goal->micro-datum goal))
          `(run ,n ,(map term->micro-datum vars) ,(goal->micro-datum goal))))]
    [_ (error 'run->micro-string "unexpected run AST: ~e" q)]))

(define (program->micro-string ast)
  (match ast
    [(prog rels q)
     (string-join
      (append (for/list ([rel (in-list rels)])
                (relation->micro-string rel))
              (list (run->micro-string q)))
      "\n\n")]
    [_ (error 'program->micro-string "unexpected program AST: ~e" ast)]))

(define (prepare-micro-program defrels run-expr)
  (define normalized-ast
    (prog (map parse-relation-def/micro defrels)
          (parse-run/micro run-expr)))
  (values normalized-ast normalized-ast))
