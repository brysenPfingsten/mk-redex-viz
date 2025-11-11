#lang racket
(require redex)

(provide parse-prog)

;-----------------Structures-------------------
(struct prog (relations query) #:transparent)
(struct fresh (vars goal) #:transparent)
(struct conde (clauses) #:transparent)
(struct disj (g1 g2) #:transparent)
(struct conj (g1 g2) #:transparent)
(struct unify (t1 t2) #:transparent)
(struct diseq (t1 t2) #:transparent)
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

;; map/fold: (T A -> (values R A)) (listof T) A -> (values (listof R) A)
;; Purpose: Like map, but threads an accumulator state through each call.
;;          The function f takes an element of the list and a state,
;;          and returns a result and an updated state.
;;
;; Example:
;;   (map/fold (λ (x s) (values (+ x s) (* s 2))) '(1 2 3) 1)
;;    => values '(2 4 7), 8
(define (map/fold f lst init-state)
  (let loop ([lst lst] [acc '()] [state init-state])
    (if (null? lst)
        (values (reverse acc) state)
        (let-values ([(v s1) (f (car lst) state)])
          (loop (cdr lst) (cons v acc) s1)))))

;; map/fold-with-guids: (T Nat -> (values R Nat (listof String))) (listof T) Nat
;;                      -> (values (listof R) Nat (listof String))
;;
;; Purpose: Like map/fold, but threads an integer counter through each call and
;;          accumulates a list of generated GUIDs. The function f takes
;;          an element of the list and a counter, and returns a result, an updated
;;          counter, and a list of GUIDs associated with that element.
;;
;; Example:
;;   (map/fold-with-guids
;;    (λ (x n) (values (* x 2) (+ n 1) (list (format "g~a" n))))
;;    '(1 2 3) 0)
;;   => values '(2 4 6), 3, '("g0" "g1" "g2")
(define (map/fold-with-guids f lst init-counter)
  (let loop ([lst lst] [acc '()] [count init-counter] [guids '()])
    (if (null? lst)
        (values (reverse acc) count guids)
        (let-values ([(v c2 g2) (f (car lst) count)])
          (loop (cdr lst)
                (cons v acc)
                c2
                (append guids g2))))))


;; next-g-id: String Number -> (values String Number)
;; Purpose: Creates a GUID based on the given prefix and counter and returns
;;          the GUID and the next count
(define (next-g-id prefix counter)
  (values (string-append prefix (number->string counter)) (add1 counter)))

;; konst->term: konst -> term
;; Purpose: Convert a konst structure to a term
(define (konst->term const)
  (match const
    [(konst s) #:when (symbol? s) `(sym ,(symbol->string s))]
    [(konst s) #:when (string? s) s]
    [(konst b) #:when (boolean? b) b]
    [(konst n) #:when (number? n) `(nat ,n)]))

;; transpile: struct Nat -> (values model-term Nat (listof String))
;; Purpose: To compile the nested structures into the language of our model
(define (transpile expr count)
  (match expr

    [(prog rels q)
     #:when (prog? expr)
     (define-values (trs count1 guids1)
       (map/fold-with-guids transpile rels count))
     (define-values (tq count2 guids2)
       (transpile q count1))
     (values `(,tq ,trs) count2 (append guids1 guids2))]

    [(fresh vars goal)
     #:when (fresh? expr)
     (define-values (id count1) (next-g-id "f" count))
     (define-values (tvars count2 guids1)
       (map/fold-with-guids transpile vars count1))
     (define-values (tgoal count3 guids2)
       (transpile goal count2))
     (values `(∃ ,tvars ,tgoal ,id) count3 (cons id (append guids1 guids2)))]


    [(conde clauses)
     #:when (conde? expr)
     (define-values (id count1) (next-g-id "d" count))
     (struct acc (expr count guids))
     (define final-acc
       (foldr
        (λ (clause accum)
          (define-values (t-clause new-count new-guids)
            (transpile clause (acc-count accum)))
          (acc (if (null? (acc-expr accum))
                   t-clause
                   `(,t-clause ∨ ,(acc-expr accum) ,id))
               new-count
               (append new-guids (acc-guids accum))))
        (acc '() count1 '())
        clauses))
     (define final-expr (acc-expr final-acc))
     (define final-count (acc-count final-acc))
     (define final-guids (acc-guids final-acc))
     (values final-expr final-count  (cons id final-guids))]

    [(conj g1 g2)
     #:when (conj? expr)
     (define-values (id count1) (next-g-id "c" count))
     (define-values (tg1 count2 guids1) (transpile g1 count1))
     (define-values (tg2 count3 guids2) (transpile g2 count2))
     (values `(,tg1 ∧ ,tg2 ,id) count3 (cons id (append guids1 guids2)))]

    [(unify t1 t2)
     #:when (unify? expr)
     (define-values (id count1) (next-g-id "u" count))
     (define-values (tt1 count2 guids1) (transpile t1 count1))
     (define-values (tt2 count3 guids2) (transpile t2 count2))
     (values `(,tt1 =? ,tt2 ,id) count3 (cons id (append guids1 guids2)))]

    [(diseq t1 t2)
     #:when (diseq? expr)
     (define-values (id count1) (next-g-id "u" count))
     (define-values (tt1 count2 guids1) (transpile t1 count1))
     (define-values (tt2 count3 guids2) (transpile t2 count2))
     (values `(,tt1 != ,tt2 ,id) count3 (cons id (append guids1 guids2)))]

    [(succeed) #:when (succeed? expr) (values (term ⊤) count '())]
    [(fail)    #:when (fail? expr)    (values (term ⊥) count '())]

    [(relcall name terms)
     #:when (relcall? expr)
     (define-values (id count1) (next-g-id "r" count))
     (define-values (tname count2 guids1) (transpile name count1))
     (define-values (tterms count3 guids2)
       (map/fold-with-guids transpile terms count2))
     (values `(,tname ,@tterms ,id) count3 (cons id (append guids1 guids2)))]

    [(nil) #:when (nil? expr) (values (term empty) count '())]
    
    [(konst k) #:when (konst? expr) (values (konst->term expr) count '())]
    
    [(kons a d)
     #:when (kons? expr)
     (define-values (ta count1 guids1) (transpile a count))
     (define-values (td count2 guids2) (transpile d count1))
     (values `(,ta : ,td) count2 (append guids1 guids2))]

    [(var v)
     #:when (var? expr)
     (values (string->symbol (string-append "x:" (symbol->string (var-v v))))
             count
             '())]

    [(relname name)
     #:when (relname? expr)
     (values (string->symbol (string-append "r:" (symbol->string name)))
             count
             '())]

    [(defrel name lop goal)
     #:when (defrel? expr)
     (define-values (tname count1 guids1) (transpile name count))
     (define-values (tlop count2 guids2)
       (map/fold-with-guids transpile lop count1))
     (define-values (tgoal count3 guids3) (transpile goal count2))
     (values `(,tname ,tlop ,tgoal)
             count3
             (append guids1 guids2 guids3))]

    [(run n qs goal)
     #:when (run? expr)
     (define-values (id count1) (next-g-id "f" count))
     (define-values (tq count2 guids1) (map/fold-with-guids transpile qs count1))
     (define-values (tg count3 guids2) (transpile goal count2))
     (values `((∃ ,tq ,tg ,id) (state () () 0 () "s"))
             count3
             (cons id (append guids1 guids2)))]))

;; konst->string: konst -> string
;; Purpose: Convert a konst structure to a string
(define (konst->string const)
  (match const
    [(konst s) #:when (symbol? s) (format "'~a" (symbol->string s))]
    [(konst s) #:when (string? s) s]
    [(konst b) #:when (boolean? b) (if b "#t" "#f")]
    [(konst n) #:when (number? n) (number->string n)]))

;; TODO: What the hell is going on here. Seems like way to much edge casing just to get it wrong sometimes.
;; Should probably (within compilation) capture what `type` of list/pair we are parsing.
;; Something like cons list, quasi pair, quote pair, (list ...), etc and so the lists are actually reproducable
;; Or, rather than building an AST, build both a CST so the output is the same as its input except with tags

(define (term->string t)
  (cond
    [(konst? t) (konst->string t)]
    [(nil? t) "'()"]
    [(var? t) (symbol->string (var-v t))]
    [(relname? t) (symbol->string (relname-name t))]
    [(kons? t) (kons->string t)]
    [else t]
    ))

;; map/kons: (T -> R) (kons of T) -> (kons of R)
;; Purpose: map but for our cons
(define (map/kons f k)
  (match k
    [_ #:when (nil? k) '()]
    [(kons a d) #:when (kons? k)
                (cons (f a) (map/kons f d))]
    [_ (list (f k))]
    ))

;; kons->string: kons -> string
;; Purpose: Convert a kons structure to a string
(define (kons->string l)
  (let* ([l^ (map/kons term->string l)]
         [l^^ (string-join l^ " ")]
         [d (kons-d l)])
    (if (or (kons? d) (nil? d))
        (format "(list ~a)" l^^)
        (format "(cons ~a)" l^^))))

(define (kons->string/help l)
  (match l
    [(kons a nil) #:when (nil? nil)
                  (kons->string/help a)]
    [(var v) #:when (var? l)
                (format "~a" (var-v v))]
    [(konst k) #:when (konst? l)
               (konst->string l)]
    [(kons a d)
     (format "~a ~a"
             (kons->string/help a)
             (kons->string/help d))]
    ))

(define (add2 n) (+ n 2))

(define (remove-tag-spaces str)
  (regexp-replace #px"\\]\\]\\s+" str "]]"))


(define (add-guids expr s guids)
  (match expr
    [(prog rels query)
     #:when (prog? expr)
     (define-values (rel-strings guids1)
       (map/fold (λ (r g) (add-guids r 0 g)) rels guids))
     (define-values (query-str guids2)
       (add-guids query 0 guids1))
     (values (string-append (string-join rel-strings "\n\n")
                            "\n\n"
                            query-str)
             guids2)]

    [(fresh vars goal)
     #:when (fresh? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define-values (vars-str rest1)
       (map/fold (λ (v gs) (add-guids v 0 gs)) vars rest))
     (define-values (goal-str rest2)
       (add-guids goal (add2 s) rest1))
     (values (format "~a[[~a]](fresh (~a)\n~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     (string-join vars-str " ")
                     goal-str
                     id)
             rest2)]

    [(conde clauses)
     #:when (conde? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define (indent n) (make-string n #\space))

 
     (define-values (clause-strs remaining-guids)
       (map/fold
        (λ (clause g)
          (define-values (clause-str new-g) (add-guids clause (+ s 2) g))
          (values (format "~a[~a]"
                          (indent (+ s 2))
                          (remove-tag-spaces (string-trim clause-str)))
                  new-g))
        clauses
        rest))

     (define body (string-join clause-strs "\n"))

     (values (format "~a[[~a]](conde\n~a\n~a)[[/~a]]"
                     (indent s)
                     id
                     body
                     (indent s)
                     id)
             remaining-guids)]


    [(conj g1 g2)
     #:when (conj? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define-values (tg1 rest1) (add-guids g1 s rest))
     (define-values (tg2 rest2) (add-guids g2 s rest1))
     (values (format "~a[[~a]]~a\n~a[[/~a]]"
                     (make-string s #\space)
                     id
                     (remove-tag-spaces tg1)
                     tg2
                     id)
             rest2)]

    [(unify t1 t2)
     #:when (unify? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define-values (tt1 rest1) (add-guids t1 0 rest))
     (define-values (tt2 rest2) (add-guids t2 0 rest1))
     (values (format "~a[[~a]](== ~a ~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     tt1
                     tt2
                     id)
             rest2)]

    [(diseq t1 t2)
     #:when (diseq? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define-values (tt1 rest1) (add-guids t1 0 rest))
     (define-values (tt2 rest2) (add-guids t2 0 rest1))
     (values (format "~a[[~a]](=/= ~a ~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     tt1
                     tt2
                     id)
             rest2)]

    [(relcall name terms)
     #:when (relcall? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define-values (tname rest1) (add-guids name 0 rest))
     (define-values (tterms rest2)
       (map/fold (λ (t g) (add-guids t 0 g)) terms rest1))
     (values (format "~a[[~a]](~a ~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     tname
                     (string-join tterms " ")
                     id)
             rest2)]

    [(nil)          #:when (nil? expr)     (values "'()" guids)]
    [(konst k)      #:when (konst? expr)   (values (konst->string expr) guids)]
    [(kons _ _)     #:when (kons? expr)    (values (kons->string expr) guids)]
    [(var v)        #:when (var? expr)     (values (symbol->string (var-v v)) guids)]
    [(relname name) #:when (relname? expr) (values (symbol->string name) guids)]

    [(defrel rname lop goal)
     #:when (defrel? expr)
     (define-values (trname g1) (add-guids rname 0 guids))
     (define-values (tlop g2)
       (map/fold (λ (v g) (add-guids v 0 g)) lop g1))
     (define-values (tgoal g3) (add-guids goal 2 g2))
     (values (format "(defrel (~a ~a)\n~a)"
                     trname
                     (string-join tlop " ")
                     tgoal)
             g3)]

    [(run n qs goal)
     #:when (run? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define-values (tq r1)
       (map/fold (λ (q g) (add-guids q 0 g)) qs rest))
     (define-values (tg r2) (add-guids goal 0 r1))
     (values (format "[[~a]](run~a ~a ~a)[[/~a]]"
                     id
                     (if (= n +inf.0) "*" (format " ~a" n))
                     tq
                     tg
                     id)
             r2)]

    [_ (error "Unrecognized AST node in add-guids" expr)]))

(define (parse-run r)
  (match r
    [`(run ,n (,q ..1) . ,gs) (run n (map var q) (conj-goals (map parse-goal gs)))]
    [`(run* (,q ..1) . ,gs) (run +inf.0 (map var q) (conj-goals (map parse-goal gs)))]))

(define (parse-relation-defs a-lor)
  (map parse-relation-def a-lor))

(define (parse-relation-def a-relation)
  (match a-relation
    [`(defrel (,r . ,params) . ,gs) (defrel
                                      (relname r)
                                      (map var params)
                                      (conj-goals (map parse-goal gs)))]))

;; fresh conde == succeed fail
(define (parse-goal goal)
  (match goal
    [`(fresh ,vars . ,goals) (fresh (map var vars) (conj-goals (map parse-goal goals)))]
    [`(conde . ,clauses) (conde (map parse-clause clauses))]
    [`(== ,t1 ,t2) (unify (parse-term t1) (parse-term t2))]
    [`(=/= ,t1 ,t2) (diseq (parse-term t1) (parse-term t2))]
    ['succeed (succeed)]
    ['fail (fail)]
    [`(,r . ,terms) (relcall (relname r) (map parse-term terms))]))
 
(define (parse-clause goals)
  (conj-goals (map parse-goal goals)))

(define (conj-goals goals)
  (foldl (λ (c a) (conj a c)) (car goals) (cdr goals)))

;; primitive-value?: any -> bool
;; True if the given value is a symbol, string, boolean, or number. False otherwise.
(define (primitive-value? v)
  (or (symbol? v)
      (string? v)
      (boolean? v)
      (number? v)))


(define (parse-term-within-quote t)
  (match t
    [(cons qta qtb) (kons (parse-term-within-quote qta)
                          (parse-term-within-quote qtb))]
    [k #:when (primitive-value? k) (konst k)]
    ['() (nil)]))

(define (kons*-terms lot)
  (foldr kons (nil) lot))

(define (parse-term-within-qquote t)
  (match t
    [(list 'unquote expr) (parse-term expr)]
    [(cons qta qtb) (kons (parse-term-within-qquote qta)
                          (parse-term-within-qquote qtb))]
    [k #:when (primitive-value? k) (konst k)]
    ['() (nil)]))


(define (parse-term t)
  (match t
    [`(quote ,subterm) (parse-term-within-quote subterm)]
    [`(quasiquote ,expr) (parse-term-within-qquote expr)]
    [`(cons ,ta ,td) (kons (parse-term ta) (parse-term td))]
    [`(list . ,args) (kons*-terms (map parse-term args))]
    [sym #:when (symbol? sym) (var sym)]
    [k #:when (primitive-value? k) (konst k)]))

;; defrels run -> model program
;; Translate the relation definitions and run query of a minikanren
;; program into our redex syntax
(define (parse-prog lst)
  (define-values (defrels run)
    (let ([result
           (foldl
            (λ (expr acc)
              (match acc
                [(cons defs run-expr)
                 (match expr
                   [`(defrel . ,_) (cons (cons expr defs) run-expr)]
                   [`(run . ,_)    (cons defs expr)]
                   [`(run* . ,_)   (cons defs expr)]
                   [else (error "Not a defrel or run form" expr)])]))
            (cons '() #f)
            lst)])
      (values (reverse (car result)) (cdr result))))

  ;; Parse AST
  (define AST
    (prog (parse-relation-defs defrels)
          (parse-run run)))

  ;; Transpile AST to redex program and collect generated GUIDs
  (define-values (REDEX-PROG counter guid-list)
    (transpile AST 0))

  ;; Tag AST with guids
  (define-values (GUID-PROG _) (add-guids AST 0 guid-list))

  ;; Return both programs
  (values REDEX-PROG GUID-PROG))

 
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

(parse-prog
  '((run* (q) (=/= q 'bear) (== q 'bear))))

(parse-prog
  '((defrel (membero l x out)
  (fresh (a d)
    (== (cons a d) l)
    (conde
      [(== a x) (== l out)]
      [(=/= a x) (membero d x out)])))

(run* (q) (membero (list 1 2 3 4) 2 q))))

#|
(defrel (membero l x out)
  (fresh (a d)
    (== (cons a d) l)
    (conde
      [(== a x) (== l out)]
      [(=/= a x) (membero d x out)])))

(run* (q) (membero (list 1 2 3 4) 2 q))
|#


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

#;(parse-prog
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

  (define-values (model-prog html-prog)
    (parse-prog '((run* (q) (fresh () (== 'dog1 'cat) (== 'bear1 lion) (== 'dog 'cat) (== 'bear 'lion))))))

  (check-equal?
   model-prog
   '(prog ()
          ((∃ (x:q)
              (∃ ()
                 (((((sym "dog1") =? (sym "cat") "u5")
                    ∧ ((sym "bear1") =? x:lion "u6") "c4")
                   ∧ ((sym "dog") =? (sym "cat") "u7") "c3")
                  ∧ ((sym "bear") =? (sym "lion") "u8") "c2") "f1") "f0")
           (state () 0 () "s"))))

  (check-equal?
   html-prog
   "\n\n[[f0]](run* (q) [[f1]](fresh ()\n  [[c2]]  [[c3]][[c4]][[u5]](== dog1 cat)[[/u5]]\n  [[u6]](== bear1 lion)[[/u6]][[/c4]]\n  [[u7]](== dog cat)[[/u7]][[/c3]]\n  [[u8]](== bear lion)[[/u8]][[/c2]])[[/f1]])[[/f0]]"
   )

  )
