#lang racket
(require racket/struct
         racket/generic
         redex
         syntax/to-string
         racket/pretty
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
    (λ (s)
      (define next-guid (string-append s (number->string counter)))
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
           (g (fresh-goal expr))
           (id (next-g-id "f")))
       (term (∃ ,(map transpile vs) ,(transpile g) ,id)))]
    [(conde? expr)
     (let ([clauses (reverse (conde-clauses expr))]
           [id (next-g-id "d")])
       (foldl (λ (c a) (term (,(transpile c) ∨ ,a ,id)))
              (transpile (car clauses))
              (cdr clauses)))]
    [(conj? expr)
     (let ((g1 (conj-g1 expr))
           (g2 (conj-g2 expr))
           (id (next-g-id "c")))
       (term (,(transpile g1) ∧ ,(transpile g2) ,id)))]
    [(unify? expr)
     (let ((t1 (unify-t1 expr))
           (t2 (unify-t2 expr))
           (id (next-g-id "u")))
       (term (,(transpile t1) =? ,(transpile t2) ,id)))]
    [(succeed? expr)
     (term ⊤)]
    [(fail? expr)
     (term ⊥)]
    [(relcall? expr)
     (let ((name (relcall-name expr))
           (terms (relcall-terms expr))
           (id (next-g-id "r")))
       (term (,(transpile name) ,@(map transpile terms) ,id)))]
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
           (goal (run-goal expr))
           (id (next-g-id "f")))
       (term ((∃ (,(transpile q)) ,(transpile goal) ,id) (state () 0 ()))))]))

(define (remove-last lst)
  (if (null? (cdr lst))
      '()
      (cons (car lst) (remove-last (cdr lst)))))

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

(define (add2 n) (+ n 2))

(define (remove-tag-spaces str)
  (regexp-replace #px"\\]\\]\\s+" str "]]"))


(define (add-guids expr s)
  (cond
    [(prog? expr)
     (let* ([rels (prog-relations expr)]
            [query (prog-query expr)]
            [rel-strings
             (map (λ (rel) (add-guids rel 0))
                  rels)]
            [rels-joined (string-join rel-strings "\n\n")]
            [query-string (add-guids query 0)])
       (string-append rels-joined
                      "\n\n"
                      query-string))]

    [(fresh? expr)
     (let ([vars (fresh-vars expr)]
           [g    (fresh-goal expr)]
           [id   (car GUIDS)])
       (set! GUIDS (cdr GUIDS))
       (format "~a[[~a]](fresh (~a)\n~a)[[/~a]]"
               (make-string s #\space)
               id
               (string-join (map (λ (v) (add-guids v 0)) vars) " ")
               (add-guids g (add2 s))
               id))]

    [(conde? expr)
     (let* ([clauses (reverse (conde-clauses expr))]
            [first-clause (first clauses)]
            [rest-clauses (rest clauses)]
            [id (car GUIDS)])
       (set! GUIDS (cdr GUIDS))
       (define (indent n) (make-string n #\space))
       (define (format-clause clause indent-size)
         (string-append "\n" (indent indent-size)
                        "["
                        (remove-tag-spaces (string-trim (add-guids clause (add2 s))))
                        "]"))
       (format "~a[[~a]](conde~a)[[/~a]]"
               (indent s)
               id
               (foldl (λ (clause acc)
                        (string-append (format-clause clause (add2 s)) acc))
                      (format-clause first-clause (add2 s))
                      rest-clauses)
               id))]


    [(conj? expr)
     (let ([g1 (conj-g1 expr)]
           [g2 (conj-g2 expr)]
           [id (car GUIDS)])
       (set! GUIDS (cdr GUIDS))
       (format "~a[[~a]]~a\n~a[[/~a]]"
               (make-string s #\space)
               id
               (remove-tag-spaces (add-guids g1 s))
               (add-guids g2 s)
               id))]
    
    [(unify? expr)
     (let* ([t1   (unify-t1 expr)]
            [t2   (unify-t2 expr)]
            [id (car GUIDS)])
       (set! GUIDS (cdr GUIDS)) 
       (format "~a[[~a]](== ~a ~a)[[/~a]]"
               (make-string s #\space)
               id
               (add-guids t1 0)   
               (add-guids t2 0)
               id))]

    [(relcall? expr)
     (let ([name  (relcall-name expr)]
           [terms (relcall-terms expr)]
           [id (car GUIDS)])
       (set! GUIDS (cdr GUIDS))
       (format "~a[[~a]](~a ~a)[[/~a]]"
               (make-string s #\space)
               id
               (add-guids name 0)
               (string-join (map (λ (t) (add-guids t 0)) terms)
                            " ")
               id))]

    [(nil? expr)
     "'()"]

    [(bool? expr)
     (if (bool-b expr) "#t" "#f")]

    [(konst? expr)
     (format "\"~a\"" (konst-k expr))]

    [(kons? expr)
     (format "`(~a)" (kons->string expr))]

    [(var? expr)
     (symbol->string (var-v expr))]

    [(relname? expr)
     (symbol->string (relname-name expr))]

    [(defrel? expr)
     (let ([rname (defrel-name expr)]
           [lop   (defrel-lop expr)]
           [goal  (defrel-goal expr)])
       (format "(defrel (~a ~a)\n~a)"
               (add-guids rname 0)
               (string-join
                (map (λ (v) (add-guids v 0)) lop)
                " ")
               (add-guids goal 2)))]

    [(run? expr)
     (let ([n    (run-n expr)]
           [q    (run-q expr)]
           [goal (run-goal expr)]
           [id (car GUIDS)])
       (set! GUIDS (cdr GUIDS))
       (format "[[~a]](run~a (~a) ~a)[[/~a]]"
               id
               (if (= n +inf.0) "*" (format " ~a" n))
               (add-guids q 0)
               (add-guids goal 0)
               id))]
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
  (foldl (λ (c a) (conj a c)) (car goals) (cdr goals)))

;; Deprecated
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
  (define GUID-PROG (add-guids AST 0))
  `(,REDEX-PROG . ,GUID-PROG))
 
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


(module+ test
  (require rackunit)

  #;(check-equal?
     (car (parse-prog
           '((run* (q) (fresh () (== 'dog1 'cat) (== 'bear1 lion) (== 'dog 'cat) (== 'bear 'lion))))))
     '(prog
       ()
       ((∃
         (x:q)
         (∃
          ()
          (("dog1" =? "cat" "u34")
           ∧
           (("bear1" =? x:lion "u36") ∧ (("dog" =? "cat" "u38") ∧ ("bear" =? "lion" "u39") "c37") "c35")
           "c33")
          "f32")
         "f31")
        (state () 0 ()))))
  )
