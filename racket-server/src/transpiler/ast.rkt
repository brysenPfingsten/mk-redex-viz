#lang racket

(provide (struct-out prog)
         (struct-out fresh)
         (struct-out conde)
         (struct-out disj)
         (struct-out conj)
         (struct-out unify)
         (struct-out diseq)
         (struct-out delay-goal)
         (struct-out compiled-delay-goal)
         (struct-out succeed)
         (struct-out fail)
         (struct-out relcall)
         (struct-out nil)
         (struct-out konst)
         (struct-out kons)
         (struct-out var)
         (struct-out relname)
         (struct-out defrel)
         (struct-out run)
         map/fold
         next-g-id
         konst->string
         term->string
         map/kons
         kons->string
         add2
         remove-tag-spaces
         add-guids
         primitive-value?
         parse-term-within-quote
         kons*-terms
         parse-term-within-qquote
         parse-term)

(struct prog (relations query) #:transparent)
(struct fresh (vars goal) #:transparent)
(struct conde (clauses) #:transparent)
(struct disj (g1 g2) #:transparent)
(struct conj (g1 g2) #:transparent)
(struct unify (t1 t2) #:transparent)
(struct diseq (t1 t2) #:transparent)
(struct delay-goal (goal) #:transparent)
(struct compiled-delay-goal (goal) #:transparent)
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

(define (map/fold f lst init-state)
  (define-values (rev-acc state)
    (for/fold ([rev-acc '()]
               [state init-state])
              ([x (in-list lst)])
      (define-values (v next-state) (f x state))
      (values (cons v rev-acc) next-state)))
  (values (reverse rev-acc) state))

(define (next-g-id prefix counter)
  (values (string-append prefix (number->string counter)) (add1 counter)))

(define (konst->string const)
  (match const
    [(struct konst (s)) #:when (symbol? s) (format "'~a" (symbol->string s))]
    [(struct konst (s)) #:when (string? s) s]
    [(struct konst (b)) #:when (boolean? b) (if b "#t" "#f")]
    [(struct konst (n)) #:when (number? n) (number->string n)]))

(define (term->string t)
  (cond
    [(konst? t) (konst->string t)]
    [(nil? t) "'()"]
    [(var? t) (symbol->string (var-v t))]
    [(relname? t) (symbol->string (relname-name t))]
    [(kons? t) (kons->string t)]
    [else t]))

(define (map/kons f k)
  (match k
    [_ #:when (nil? k) '()]
    [(struct kons (a d))
     (cons (f a) (map/kons f d))]
    [_ (list (f k))]))

(define (kons->string l)
  (let* ([l^ (map/kons term->string l)]
         [l^^ (string-join l^ " ")]
         [d (kons-d l)])
    (if (or (kons? d) (nil? d))
        (format "(list ~a)" l^^)
        (format "(cons ~a)" l^^))))

(define (kons->string/help l)
  (match l
    [(struct kons (a nil-tail)) #:when (nil? nil-tail)
     (kons->string/help a)]
    [(struct var (v))
     (format "~a" v)]
    [(struct konst (_k))
     (konst->string l)]
    [(struct kons (a d))
     (format "~a ~a"
             (kons->string/help a)
             (kons->string/help d))]))

(define (add2 n) (+ n 2))

(define (remove-tag-spaces str)
  (regexp-replace #px"\\]\\]\\s+" str "]]"))

(define (add-guids expr s guids)
  (match expr
    [(struct prog (rels query))
     (define-values (rel-strings guids1)
       (map/fold (lambda (r g) (add-guids r 0 g)) rels guids))
     (define-values (query-str guids2)
       (add-guids query 0 guids1))
     (values (string-append (string-join rel-strings "\n\n")
                            "\n\n"
                            query-str)
             guids2)]

    [(struct fresh (vars goal))
     (match-define (cons id rest) guids)
     (define-values (vars-str rest1)
       (map/fold (lambda (v gs) (add-guids v 0 gs)) vars rest))
     (define-values (goal-str rest2)
       (add-guids goal (add2 s) rest1))
     (values (format "~a[[~a]](fresh (~a)\n~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     (string-join vars-str " ")
                     goal-str
                     id)
             rest2)]

    [(struct conde (clauses))
     (match-define (cons id rest) guids)
     (define (indent n) (make-string n #\space))
     (define-values (clause-strs remaining-guids)
       (map/fold
        (lambda (clause g)
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

    [(struct conj (g1 g2))
     (match-define (cons id rest) guids)
     (define-values (tg1 rest1) (add-guids g1 s rest))
     (define-values (tg2 rest2) (add-guids g2 s rest1))
     (values (format "~a[[~a]]~a\n~a[[/~a]]"
                     (make-string s #\space)
                     id
                     (remove-tag-spaces tg1)
                     tg2
                     id)
             rest2)]

    [(struct unify (t1 t2))
     (match-define (cons id rest) guids)
     (define-values (tt1 rest1) (add-guids t1 0 rest))
     (define-values (tt2 rest2) (add-guids t2 0 rest1))
     (values (format "~a[[~a]](== ~a ~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     tt1
                     tt2
                     id)
             rest2)]

    [(struct diseq (t1 t2))
     (match-define (cons id rest) guids)
     (define-values (tt1 rest1) (add-guids t1 0 rest))
     (define-values (tt2 rest2) (add-guids t2 0 rest1))
     (values (format "~a[[~a]](=/= ~a ~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     tt1
                     tt2
                     id)
             rest2)]

    [(struct succeed ())
     (values (format "~a(succeed)" (make-string s #\space)) guids)]

    [(struct fail ())
     (values (format "~a(fail)" (make-string s #\space)) guids)]

    [(struct disj (g1 g2))
     (match-define (cons id rest) guids)
     (define-values (tg1 rest1) (add-guids g1 (+ s 2) rest))
     (define-values (tg2 rest2) (add-guids g2 (+ s 2) rest1))
     (values (format "~a[[~a]](disj\n~a\n~a\n~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     tg1
                     tg2
                     (make-string s #\space)
                     id)
             rest2)]

    [(struct relcall (name terms))
     (match-define (cons id rest) guids)
     (define-values (tname rest1) (add-guids name 0 rest))
     (define-values (tterms rest2)
       (map/fold (lambda (t g) (add-guids t 0 g)) terms rest1))
     (values (format "~a[[~a]](~a ~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     tname
                     (string-join tterms " ")
                     id)
             rest2)]

    [(struct nil ())          (values "'()" guids)]
    [(struct konst (_))       (values (konst->string expr) guids)]
    [(struct kons (_ _))      (values (kons->string expr) guids)]
    [(struct var (v))         (values (symbol->string v) guids)]
    [(struct relname (name))  (values (symbol->string name) guids)]

    [(struct defrel (rname lop goal))
     (define-values (trname g1) (add-guids rname 0 guids))
     (define-values (tlop g2)
       (map/fold (lambda (v g) (add-guids v 0 g)) lop g1))
     (define-values (tgoal g3) (add-guids goal 2 g2))
     (values (format "(defrel (~a ~a)\n~a)"
                     trname
                     (string-join tlop " ")
                     tgoal)
             g3)]

    [(struct run (n qs goal))
     (match-define (cons id rest) guids)
     (define-values (tq r1)
       (map/fold (lambda (q g) (add-guids q 0 g)) qs rest))
     (define-values (tg r2) (add-guids goal 0 r1))
     (values (format "[[~a]](run~a ~a ~a)[[/~a]]"
                     id
                     (if (= n +inf.0) "*" (format " ~a" n))
                     tq
                     tg
                     id)
             r2)]

    [(struct delay-goal (goal))
     (match-define (cons id rest) guids)
     (define-values (goal-str rest1) (add-guids goal (+ s 2) rest))
     (values (format "~a[[~a]](Zzz\n~a\n~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     goal-str
                     (make-string s #\space)
                     id)
             rest1)]

    [_ (error "Unrecognized AST node in add-guids" expr)]))

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
