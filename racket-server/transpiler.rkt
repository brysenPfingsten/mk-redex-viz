#lang racket
(require racket/struct
         racket/generic
         redex
         "definitions.rkt")

(provide parse-prog)

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
(struct bool (b) #:transparent)
(struct konst (k) #:transparent)
(struct kons (a d) #:transparent)
(struct var (v) #:transparent)
(struct relname (name) #:transparent)
(struct defrel (name lop goal) #:transparent)
(struct run (n q goal) #:transparent)

(define GUIDS '())

(define next-g-id
  (let ([counter 0])
    (λ ()
      (define next-guid (string-append "g" (number->string counter)))
      (set! counter (add1 counter))
      (set! GUIDS (cons next-guid GUIDS))
      next-guid
      )))

(define (transpile expr)
  (cond
    [(prog? expr)
     (let [(r (prog-relations expr))
           (q (prog-query expr))]
       (term (prog ,(map transpile r) ,(transpile q))))]
    [(fresh? expr)
     (let ((vs (fresh-vars expr))
           (g (fresh-goal expr)))
       (term (∃ ,(map transpile vs) ,(transpile g))))]
    [(conde? expr)
     (let ([clauses (conde-clauses expr)])
       (foldr (λ (c a) (term (,(transpile c) ∨ ,a)))
              (transpile (last clauses))
              (remove-last clauses)))]
    [(conj? expr)
     (let ((g1 (conj-g1 expr))
           (g2 (conj-g2 expr)))
       (term (,(transpile g1) ∧ ,(transpile g2))))]
    [(unify? expr)
     (let ((t1 (unify-t1 expr))
           (t2 (unify-t2 expr))
           (guid (next-g-id)))
       (term (,(transpile t1) =? ,(transpile t2) ,guid)))]
    [(succeed? expr)
     (term ⊤)]
    [(fail? expr)
     (term ⊥)]
    [(relcall? expr)
     (let ((name (relcall-name expr))
           (terms (relcall-terms expr)))
       (term (,(transpile name) ,@(map transpile terms))))]
    [(nil? expr)
     (term empty)]
    [(bool? expr)
     (let ((b (bool-b expr)))
       b)]
    [(konst? expr)
     (let ((k (konst-k expr)))
       k)]
    [(kons? expr)
     (let ((a (kons-a expr))
           (d (kons-d expr)))
       (term (,(transpile a) : ,(transpile d))))]
    [(var? expr)
     (let ((v (var-v expr)))
       (string->symbol (string-append "x:" (symbol->string v))))]
    [(relname? expr)
     (let ((name (relname-name expr)))
       (string->symbol (string-append "r:" (symbol->string name))))]
    [(defrel? expr)
     (let ((name (defrel-name expr))
           (lop (defrel-lop expr))
           (goal (defrel-goal expr)))
       (term (,(transpile name) ,(map transpile lop) ,(transpile goal))))]
    [(run? expr)
     (let ((n (run-n expr))
           (q (run-q expr))
           (goal (run-goal expr)))
       (term ((∃ (,(transpile q)) ,(transpile goal)) (state () 0 ()))))]))

(define (remove-last lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (remove-last (cdr lst)))))

(define (term->string t)
  (match t
    [(var v) #:when (var? t) (format ",~a" v)]
    [else (add-guids t '())]))

(define (kons->string l)
  (match l
    [(kons a nil) #:when (nil? nil)
                  (kons->string a)]
    [(kons a ad) #:when (not (kons? ad))
                 (format "~a . ~a"
                         (kons->string a)
                         (kons->string ad))]
    [(kons a d)
     (format "~a ~a"
             (kons->string a)
             (kons->string d))]
    [(var v) #:when (var? l)
             (format ",~a" (var-v v))]
    [(konst k) #:when (konst? l)
               (format "~a" k)])) 

(define (add-guids expr)
  (cond
    [(prog? expr)
     (let* ([rels (prog-relations expr)]
            [query (prog-query expr)]
            [rel-strings
             (map (λ (rel) (add-guids rel))
                  rels)]
            [rels-joined (string-join rel-strings "\n\n")]
            [query-string (add-guids query)])
       (string-append rels-joined
                      "\n\n"
                      query-string))]

    ;; A "fresh" node:  (fresh (v1 v2 ...) goal)
    [(fresh? expr)
     (let ([vars (fresh-vars expr)]
           [g    (fresh-goal expr)])
       (format "(fresh (~a)\n ~a)"
               ;; Recursively annotate each var’s name
               (string-join (map (λ (v) (add-guids v))
                                 vars)
                            " ")
               ;; Recursively annotate the sub‐goal
               (add-guids g)))]

    ;; A "disj" node: (disj g1 g2)
    [(conde? expr)
     (let ([clauses (conde-clauses expr)])
       (format "(conde\n~a)"
               (foldl (λ (c a) (string-append "[" (add-guids c) "]"
                                              "\n"
                                              a))
                      (add-guids (last clauses))
                      (remove-last clauses))))]

    ;; A "conj" node: (conj g1 g2)
    [(conj? expr)
     (let ([g1 (conj-g1 expr)]
           [g2 (conj-g2 expr)])
       (format "~a\n~a)"
               (add-guids g1)
               (add-guids g2)))]

    ;; unify => insert bracket tags. Then recursively call add-guids on t1/t2.
    [(unify? expr)
     (let* ([t1   (unify-t1 expr)]
            [t2   (unify-t2 expr)]
            [guid (car GUIDS)])
       (set! GUIDS (cdr GUIDS)) 
       ;; Insert bracket tags around the unify
       (format "[[~a]](== ~a ~a)[[/~a]]"
               guid
               (add-guids t1)   
               (add-guids t2)
               guid))]

    ;; Relation call, e.g. (relcall? expr)
    [(relcall? expr)
     (let ([name  (relcall-name expr)]
           [terms (relcall-terms expr)])
       (format "(~a ~a)"
               (add-guids name)
               (string-join (map (λ (t) (add-guids t)) terms)
                            " ")))]

    ;; Nil => '()
    [(nil? expr)
     "'()"]

    ;; Boolean => #t or #f
    [(bool? expr)
     (if (bool-b expr) "#t" "#f")]

    ;; Konstant => just output its contents as a string, or `'abc`
    [(konst? expr)
     (konst-k expr)]

    ;; Kons => produce (cons subA subD), or transform to backtick forms if you like
    [(kons? expr)
     (format "`(~a)" (kons->string expr))]

    ;; A variable => output the symbol name
    [(var? expr)
     (symbol->string (var-v expr))]

    ;; A relation name => output the symbol name
    [(relname? expr)
     (symbol->string (relname-name expr))]

    ;; A defrel => typical minikanren form (defrel (name var...) goal)
    [(defrel? expr)
     (let ([rname (defrel-name expr)]
           [lop   (defrel-lop expr)]
           [goal  (defrel-goal expr)])
       (format "(defrel (~a ~a)\n  ~a)"
               (add-guids rname)
               (string-join
                (map (λ (v) (add-guids v)) lop)
                " ")
               (add-guids goal)))]

    ;; A run => (run N (q) goal)
    [(run? expr)
     (let ([n    (run-n expr)]
           [q    (run-q expr)]
           [goal (run-goal expr)])
       (format "(run~a (~a) ~a)"
               (if (= n +inf.0) "*" (format " ~a" n))
               (add-guids q)
               (add-guids goal)))]
    [else
     (error "Unrecognized AST node in add-guids" expr)]))




(define (parse-run r)
  (match r
    [`(run ,n (,q) ,g) (run n (var q) (parse-goal g))]
    [`(run* (,q) ,g) (run +inf.0 (var q) (parse-goal g))]))

(define (parse-relation-defs a-lor)
  (map parse-relation-def a-lor))

(define (parse-relation-def a-relation)
  (match a-relation
    [`(defrel (,r . ,params) ,g) (defrel
                                   (relname r)
                                   (map var params)
                                   (parse-goal g))]))

;; fresh conde == succeed fail
(define (parse-goal goal)
  (match goal
    [`(fresh ,vars . ,goals) (fresh (map var vars) (conj-goals (map parse-goal goals)))]
    [`(conde . ,clauses) (conde (map parse-clause clauses))]
    [`(== ,t1 ,t2) (unify (parse-term t1) (parse-term t2))]
    ['succeed (succeed)]
    ['fail (fail)]
    [`(,r . ,terms) (relcall (relname r) (map parse-term terms))]))
 
(define (parse-clause goals)
  (conj-goals (map parse-goal goals)))

(define (conj-goals goals)
  (foldr conj (last goals) (remove-last goals)))

(define (disj-goals goals)
  (foldl disj (first goals) (rest goals)))

(define (parse-term-within-quote t)
  (match t
    [(cons qta qtb) (kons (parse-term-within-quote qta)
                          (parse-term-within-quote qtb))]
    [s #:when (symbol? s) (konst (symbol->string s))]
    [s #:when (string? s) (konst s)]
    [b #:when (boolean? b) (bool b)]
    ['() (nil)]))

(define (kons*-terms lot)
  (foldr kons (nil) lot))


(define (parse-term-within-qquote t)
  (match t
    [(list 'unquote expr) (parse-term expr)]
    [(cons qta qtb) (kons (parse-term-within-qquote qta)
                          (parse-term-within-qquote qtb))]
    [s #:when (symbol? s) (konst (symbol->string s))]
    [b #:when (boolean? b) (bool b)]
    [str #:when (string? str) (konst str)]
    ['() (nil)]))


(define (parse-term t)
  (match t
    [`(quote ,subterm) (parse-term-within-quote subterm)]
    [`(quasiquote ,expr) (parse-term-within-qquote expr)]
    [`(cons ,ta ,td) (kons (parse-term ta) (parse-term td))]
    [`(list . ,args) (kons*-terms (map parse-term args))]
    [sym #:when (symbol? sym) (var sym)]
    [boo #:when (boolean? boo) (bool boo)]
    [str #:when (string? str) (konst str)]))

;; defrels run -> model program
;; Translate the relation definitions and run query of a minikanren
;; program into our redex syntax
(define (parse-prog l)
  (define defrels '())
  (define run '())
  (set! GUIDS '())

  (map
   (λ (expr)
     (match expr
       [`(defrel . ,d) (set! defrels (cons expr defrels))]
       [`(run . ,d)    (set! run expr)]
       [`(run* . ,d)   (set! run expr)]
       [else (error "Not a defrel or run form")]))
   l) 

  (define AST (prog (parse-relation-defs (reverse defrels)) (parse-run run)))
  (define REDEX-PROG (transpile AST))
  (set! GUIDS (reverse GUIDS))
  (define GUID-PROG (add-guids AST))
  `(,REDEX-PROG . ,GUID-PROG))
 
#;(parse-relation-defs '( 
                         (defrel (assoco key table value)
                           (fresh (car table-cdr)
                             (== table `(,car . ,table-cdr))
                             (conde ((== `(,key . ,value) car))
                                    ((assoco key table-cdr value)))))
                         (defrel (same-lengtho l1 l2)
                           (conde ((== l1 '()) (== l1 '()))
                                  ((fresh (car1 cdr1 car2 cdr2)
                                     (== l1 `(,car1 . ,cdr1))
                                     (== l2 `(,car2 . ,cdr2))
                                     (same-lengtho cdr1 cdr2)))))
                         (defrel (make-assoc-tableo l1 l2 table)
                           (conde ((== l1 '()) (== l1 '()) (== table '()))
                                  ((fresh (car1 cdr1 car2 cdr2 cdr3)
                                     (== l1 `(,car1 . ,cdr1))
                                     (== l2 `(,car2 . ,cdr2))
                                     (== table `((,car1 . ,car2) . ,cdr3))
                                     (make-assoc-tableo cdr1 cdr2 cdr3)))))))


#;(parse-prog
   '(defrel (assoco key table value)
      (fresh (car table-cdr)
        (== table `(,car . ,table-cdr))
        (conde ((== `(,key . ,value) car))
               ((assoco key table-cdr value)))))
   '(defrel (same-lengtho l1 l2)
      (conde ((== l1 '()) (== l1 '()))
             ((fresh (car1 cdr1 car2 cdr2)
                (== l1 `(,car1 . ,cdr1))
                (== l2 `(,car2 . ,cdr2))
                (same-lengtho cdr1 cdr2)))))
   '(defrel (make-assoc-tableo l1 l2 table)
      (conde ((== l1 '()) (== l1 '()) (== table '()))
             ((fresh (car1 cdr1 car2 cdr2 cdr3)
                (== l1 `(,car1 . ,cdr1))
                (== l2 `(,car2 . ,cdr2))
                (== table `((,car1 . ,car2) . ,cdr3))
                (make-assoc-tableo cdr1 cdr2 cdr3)))))
   '(run 5 (q) (same-lengtho '(abc def ghi) q)))


#;'(prog ((r:make-assoc-tableo x:l1 x:l2 x:table
                               (((x:l1 =? empty) ∧ ((x:l1 =? empty) ∧ (x:table =? empty)))
                                ∨
                                (∃ x:car1 x:cdr1 x:car2 x:cdr2 x:cdr3
                                   ((x:l1 =? (x:car1 : x:cdr1))
                                    ∧
                                    ((x:l2 =? (x:car2 : x:cdr2))
                                     ∧
                                     ((x:table =? ((x:car1 : x:car2) : x:cdr3))
                                      ∧
                                      (r:make-assoc-tableo x:cdr1 x:cdr2 x:cdr3)))))))
          (r:same-lengtho x:l1 x:l2
                          (((x:l1 =? empty) ∧ (x:l1 =? empty))
                           ∨
                           (∃ x:car1 x:cdr1 x:car2 x:cdr2
                              ((x:l1 =? (x:car1 : x:cdr1)) ∧ ((x:l2 =? (x:car2 : x:cdr2)) ∧ (r:same-lengtho x:cdr1 x:cdr2))))))
          (r:assoco x:key x:table x:value
                    (∃ x:car x:table-cdr
                       ((x:table =? (x:car : x:table-cdr)) ∧ (((x:key : x:value) =? x:car) ∨ (r:assoco x:key x:table-cdr x:value))))))
         ((∃ x:q (r:same-length (abc : (def : (ghi : empty))) x:q)) (state () 0)))

(parse-prog
 '((defrel (appendo l s out)
     (conde
      [(== l '()) (== out s)]
      [(fresh (a d res)
         (== l `(,a . ,d))
         (== out `(,a . ,res))
         (appendo d s res))]))

   (defrel (reverseo ls out)
     (conde
      [(== ls '()) (== out '())]
      [(fresh (a d res)
         (== ls `(,a . ,d))
         (reverseo d res)
         (appendo res `(,a) out))]))

   (run* (q) (reverseo '(dog cat bear lion) q))))

    
