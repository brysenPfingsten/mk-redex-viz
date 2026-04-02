#lang racket
(require racket/struct
         racket/generic
         racket/set
         redex/reduction-semantics
         syntax/to-string
         racket/pretty)

(provide parse-prog
         parse-prog/canonical
         parse-prog->ast
         render-micro-source
         default-source-mode
         source-mode?
         normalize-source-mode
         compile-profile
         compile-profile?
         compile-profile-conj-assoc
         compile-profile-disj-assoc
         compile-profile-delay-placement
         canonical-compile-profile
         canonical-compile-profile-jsexpr
         normalize-compile-profile
         compile-profile->jsexpr
         REQ-CORE
         REQ-RELCALL
         REQ-DISJUNCTION
         REQ-FRESH
         REQ-DELAY
         ast->requirements
         canonical-parser-profile
         canonical-parser-target-id)

;-----------------Structures-------------------
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
(struct compile-profile (conj-assoc disj-assoc delay-placement) #:transparent)
;-----------------------------------------------

;; Canonical parser target for backend stepping.
(define canonical-parser-profile "surface->l4")
(define canonical-parser-target-id "L4/config")

(define default-source-mode "mini")

(define canonical-compile-profile
  (compile-profile "left" "right" "relbody"))

(define canonical-compile-profile-jsexpr
  (hasheq 'conjAssoc "left"
          'disjAssoc "right"
          'delayPlacement "relbody"))

;; Capability requirements (used for model compatibility checks).
(define REQ-CORE "req/core")
(define REQ-RELCALL "req/relcall")
(define REQ-DISJUNCTION "req/disjunction")
(define REQ-FRESH "req/fresh")
(define REQ-DELAY "req/delay")

(define (source-mode? v)
  (and (string? v)
       (member v '("mini" "micro"))))

(define (normalize-source-mode maybe-mode)
  (cond
    [(or (not maybe-mode) (equal? maybe-mode "")) default-source-mode]
    [(source-mode? maybe-mode) maybe-mode]
    [else
     (error 'normalize-source-mode
            "unsupported sourceMode ~e; expected \"mini\" or \"micro\""
            maybe-mode)]))

(define (compile-profile->jsexpr profile)
  (hasheq 'conjAssoc (compile-profile-conj-assoc profile)
          'disjAssoc (compile-profile-disj-assoc profile)
          'delayPlacement (compile-profile-delay-placement profile)))

(define (normalize-axis maybe-value valid-values key)
  (cond
    [(not maybe-value) #f]
    [(and (string? maybe-value) (member maybe-value valid-values)) maybe-value]
    [else
     (error 'normalize-compile-profile
            "invalid compileProfile.~a ~e; expected one of ~e"
            key
            maybe-value
            valid-values)]))

(define (normalize-compile-profile maybe-profile [source-mode default-source-mode])
  (define source-mode* (normalize-source-mode source-mode))
  (cond
    [(equal? source-mode* "micro")
     (when maybe-profile
       (error 'normalize-compile-profile
              "compileProfile is only valid when sourceMode is \"mini\""))
     #f]
    [(not maybe-profile) canonical-compile-profile]
    [(compile-profile? maybe-profile) maybe-profile]
    [(hash? maybe-profile)
     (define conj-assoc
       (normalize-axis (hash-ref maybe-profile 'conjAssoc #f)
                       '("left" "right")
                       'conjAssoc))
     (define disj-assoc
       (normalize-axis (hash-ref maybe-profile 'disjAssoc #f)
                       '("left" "right")
                       'disjAssoc))
     (define delay-placement
       (normalize-axis (hash-ref maybe-profile 'delayPlacement #f)
                       '("relbody" "relcall" "disj")
                       'delayPlacement))
     (unless (and conj-assoc disj-assoc delay-placement)
       (error 'normalize-compile-profile
              "compileProfile must contain conjAssoc, disjAssoc, and delayPlacement"))
     (compile-profile conj-assoc disj-assoc delay-placement)]
    [else
     (error 'normalize-compile-profile
            "compileProfile must be a hash or compile-profile, got ~e"
            maybe-profile)]))

(define (goal->requirements g)
  (match g
    [(fresh _ goal)
     (set-add (goal->requirements goal) REQ-FRESH)]
    [(conde clauses)
     (for/fold ([acc (set REQ-DISJUNCTION)])
               ([clause (in-list clauses)])
       (set-union acc (goal->requirements clause)))]
    [(disj g1 g2)
     (set-union (set REQ-DISJUNCTION)
                (goal->requirements g1)
                (goal->requirements g2))]
    [(conj g1 g2)
     (set-union (goal->requirements g1)
                (goal->requirements g2))]
    [(delay-goal goal)
     (set-add (goal->requirements goal) REQ-DELAY)]
    [(compiled-delay-goal goal)
     (set-add (goal->requirements goal) REQ-DELAY)]
    [(relcall _ _)
     (set REQ-RELCALL)]
    [_ (set)]))

(define (ast->requirements ast)
  (match ast
    [(prog rels (run _ _ query-goal))
     (define reqs-from-rels
       (for/fold ([acc (set REQ-CORE)])
                 ([rel (in-list rels)])
         (match rel
           [(defrel _ _ goal) (set-union acc (goal->requirements goal))]
           [_ acc])))
     (sort (set->list (set-union reqs-from-rels (goal->requirements query-goal)))
           string<?)]
    [_ (list REQ-CORE)]))

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
     (define-values (id count1) (next-g-id "n" count))
     (define-values (tt1 count2 guids1) (transpile t1 count1))
     (define-values (tt2 count3 guids2) (transpile t2 count2))
     (values `(,tt1 != ,tt2 ,id) count3 (cons id (append guids1 guids2)))]

    [(succeed) #:when (succeed? expr) (values '⊤ count '())]
    [(fail)    #:when (fail? expr)    (values '⊥ count '())]

    [(relcall name terms)
     #:when (relcall? expr)
     (define-values (id count1) (next-g-id "r" count))
     (define-values (tname count2 guids1) (transpile name count1))
     (define-values (tterms count3 guids2)
       (map/fold-with-guids transpile terms count2))
     (values `(,tname ,@tterms ,id) count3 (cons id (append guids1 guids2)))]

    [(nil) #:when (nil? expr) (values 'empty count '())]
    
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
     (values `((∃ ,tq ,tg ,id) (state () 0 () "s"))
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

    [(succeed)
     #:when (succeed? expr)
     (values (format "~a(succeed)" (make-string s #\space)) guids)]

    [(fail)
     #:when (fail? expr)
     (values (format "~a(fail)" (make-string s #\space)) guids)]

    [(disj g1 g2)
     #:when (disj? expr)
     (define id (car guids))
     (define rest (cdr guids))
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

    [(delay-goal goal)
     #:when (delay-goal? expr)
     (define id (car guids))
     (define rest (cdr guids))
     (define-values (goal-str rest1) (add-guids goal (+ s 2) rest))
     (values (format "~a[[~a]](Zzz\n~a\n~a)[[/~a]]"
                     (make-string s #\space)
                     id
                     goal-str
                     (make-string s #\space)
                     id)
             rest1)]

    [_ (error "Unrecognized AST node in add-guids" expr)]))

(define (reserved-goal-symbol? sym)
  (and (symbol? sym)
       (member sym '(fresh conde conj disj Zzz delay == =/= succeed fail proceed))))

(define (conj-goals/left goals)
  (match goals
    [(list goal) goal]
    [(cons goal more)
     (conj-goals/left
      (cons (conj goal (car more))
            (cdr more)))]
    [_ (error 'conj-goals/left "expected a non-empty goal sequence")]))

(define (combine-conj goals assoc)
  (match goals
    [(list goal) goal]
    [(cons goal more)
     (if (equal? assoc "left")
         (combine-conj (cons (conj goal (car more))
                             (cdr more))
                       assoc)
         (conj goal (combine-conj more assoc)))]
    [_ (error 'combine-conj "expected a non-empty goal sequence")]))

(define (combine-disj goals assoc)
  (match goals
    [(list goal) goal]
    [(cons goal more)
     (if (equal? assoc "left")
         (combine-disj (cons (disj goal (car more))
                             (cdr more))
                       assoc)
         (disj goal (combine-disj more assoc)))]
    [_ (error 'combine-disj "expected a non-empty clause sequence")]))

(define (flatten-conj-tree goal [acc '()])
  (match goal
    [(conj g1 g2)
     (flatten-conj-tree g1 (flatten-conj-tree g2 acc))]
    [_ (cons goal acc)]))

(define (contains-delay-goal? goal [seen #f])
  (match goal
    [(delay-goal g) (contains-delay-goal? g #t)]
    [(compiled-delay-goal g) (contains-delay-goal? g #t)]
    [(fresh _ g) (contains-delay-goal? g seen)]
    [(conde clauses)
     (for/or ([clause (in-list clauses)])
       (contains-delay-goal? clause seen))]
    [(conj g1 g2)
     (or (contains-delay-goal? g1 seen)
         (contains-delay-goal? g2 seen))]
    [(disj g1 g2)
     (or (contains-delay-goal? g1 seen)
         (contains-delay-goal? g2 seen))]
    [_ seen]))

(define (wrap-relcalls goal [wrapper delay-goal])
  (match goal
    [(fresh vars g)
     (fresh vars (wrap-relcalls g wrapper))]
    [(conj g1 g2)
     (conj (wrap-relcalls g1 wrapper) (wrap-relcalls g2 wrapper))]
    [(disj g1 g2)
     (disj (wrap-relcalls g1 wrapper) (wrap-relcalls g2 wrapper))]
    [(delay-goal g)
     (delay-goal (wrap-relcalls g wrapper))]
    [(compiled-delay-goal g)
     (compiled-delay-goal (wrap-relcalls g wrapper))]
    [(relcall _ _)
     (wrapper goal)]
    [_ goal]))

(define (wrap-disjs goal [wrapper delay-goal])
  (match goal
    [(fresh vars g)
     (fresh vars (wrap-disjs g wrapper))]
    [(conj g1 g2)
     (conj (wrap-disjs g1 wrapper) (wrap-disjs g2 wrapper))]
    [(disj g1 g2)
     (wrapper (disj (wrap-disjs g1 wrapper) (wrap-disjs g2 wrapper)))]
    [(delay-goal g)
     (delay-goal (wrap-disjs g wrapper))]
    [(compiled-delay-goal g)
     (compiled-delay-goal (wrap-disjs g wrapper))]
    [_ goal]))

(define (surface-goal->micro goal profile)
  (match goal
    [(fresh vars g)
     (fresh vars (surface-goal->micro g profile))]
    [(conde clauses)
     (combine-disj
      (for/list ([clause (in-list clauses)])
        (surface-goal->micro clause profile))
      (compile-profile-disj-assoc profile))]
    [(conj _ _)
     (combine-conj
      (for/list ([piece (in-list (flatten-conj-tree goal))])
        (surface-goal->micro piece profile))
      (compile-profile-conj-assoc profile))]
    [(disj g1 g2)
     (disj (surface-goal->micro g1 profile)
           (surface-goal->micro g2 profile))]
    [(delay-goal g)
     (delay-goal (surface-goal->micro g profile))]
    [_ goal]))

(define (apply-delay-placement goal placement [wrapper delay-goal])
  (case (string->symbol placement)
    [(relcall) (wrap-relcalls goal wrapper)]
    [(disj) (wrap-disjs goal wrapper)]
    [else goal]))

(define (mini-ast->normalized-micro ast profile)
  (match ast
    [(prog rels (run n q goal))
     (define normalized-rels
       (for/list ([rel (in-list rels)])
         (match-define (defrel name lop rel-goal) rel)
         (define normalized-goal
           (surface-goal->micro rel-goal profile))
         (defrel name
                 lop
                 (if (equal? (compile-profile-delay-placement profile) "relbody")
                     (compiled-delay-goal normalized-goal)
                     (apply-delay-placement normalized-goal
                                            (compile-profile-delay-placement profile)
                                            compiled-delay-goal)))))
     (prog normalized-rels
           (run n
                q
                (apply-delay-placement (surface-goal->micro goal profile)
                                       (compile-profile-delay-placement profile)
                                       compiled-delay-goal)))]
    [_ (error 'mini-ast->normalized-micro
              "unexpected source AST shape: ~e"
              ast)]))

(define (relbody-certifies? goal)
  (match goal
    [(compiled-delay-goal inner)
     (not (contains-delay-goal? inner))]
    [_ #f]))

(define (relcall-certifies? goal)
  (match goal
    [(delay-goal inner)
     (and (relcall? inner) #t)]
    [(compiled-delay-goal inner)
     (and (relcall? inner) #t)]
    [(relcall _ _) #f]
    [(fresh _ g) (relcall-certifies? g)]
    [(conj g1 g2)
     (and (relcall-certifies? g1)
          (relcall-certifies? g2))]
    [(disj g1 g2)
     (and (relcall-certifies? g1)
          (relcall-certifies? g2))]
    [_ #t]))

(define (disj-certifies? goal)
  (match goal
    [(delay-goal inner)
     (and (disj? inner)
          (match inner
            [(disj g1 g2)
             (and (disj-certifies? g1)
                  (disj-certifies? g2))]
            [_ #f]))]
    [(compiled-delay-goal inner)
     (and (disj? inner)
          (match inner
            [(disj g1 g2)
             (and (disj-certifies? g1)
                  (disj-certifies? g2))]
            [_ #f]))]
    [(disj _ _) #f]
    [(fresh _ g) (disj-certifies? g)]
    [(conj g1 g2)
     (and (disj-certifies? g1)
          (disj-certifies? g2))]
    [_ #t]))

(define (certify-guarded-mini-program ast profile)
  (define placement (compile-profile-delay-placement profile))
  (match ast
    [(prog rels (run _ _ query-goal))
     (case (string->symbol placement)
       [(relbody)
        (unless (andmap (lambda (rel)
                          (match rel
                            [(defrel _ _ goal) (relbody-certifies? goal)]
                            [_ #f]))
                        rels)
          (error 'certify-guarded-mini-program
                 "relbody profile did not produce delayed relation bodies"))
        (when (contains-delay-goal? query-goal)
          (error 'certify-guarded-mini-program
                 "relbody profile should not delay the query"))]
       [(relcall)
        (unless (andmap (lambda (rel)
                          (match rel
                            [(defrel _ _ goal) (relcall-certifies? goal)]
                            [_ #f]))
                        rels)
          (error 'certify-guarded-mini-program
                 "relcall profile did not delay every relation call in bodies"))
        (unless (relcall-certifies? query-goal)
          (error 'certify-guarded-mini-program
                 "relcall profile did not delay every relation call in query"))]
       [(disj)
        (unless (andmap (lambda (rel)
                          (match rel
                            [(defrel _ _ goal) (disj-certifies? goal)]
                            [_ #f]))
                        rels)
          (error 'certify-guarded-mini-program
                 "disj profile did not delay every disjunction in bodies"))
        (unless (disj-certifies? query-goal)
          (error 'certify-guarded-mini-program
                 "disj profile did not delay every disjunction in query"))]
       [else
        (error 'certify-guarded-mini-program
               "unknown delay placement ~e"
               placement)])
     ast]
    [_ (error 'certify-guarded-mini-program
              "unexpected normalized program shape: ~e"
              ast)]))

(define (parse-run/surface-mini r)
  (match r
    [`(run ,n (,q ..1) . ,gs)
     (run n (map var q) (conj-goals/left (map parse-goal/surface-mini gs)))]
    [`(run* (,q ..1) . ,gs)
     (run +inf.0 (map var q) (conj-goals/left (map parse-goal/surface-mini gs)))]
    [_ (error 'parse-run/surface-mini "invalid run form: ~e" r)]))

(define (parse-relation-def/surface-mini a-relation)
  (match a-relation
    [`(defrel (,r . ,params) . ,gs)
     (defrel (relname r)
             (map var params)
             (conj-goals/left (map parse-goal/surface-mini gs)))]
    [_ (error 'parse-relation-def/surface-mini
              "invalid defrel form: ~e"
              a-relation)]))

(define (parse-goal/surface-mini goal)
  (match goal
    [`(fresh ,vars . ,goals)
     (fresh (map var vars) (conj-goals/left (map parse-goal/surface-mini goals)))]
    [`(conde . ,clauses)
     (conde (map parse-clause/surface-mini clauses))]
    [`(== ,t1 ,t2)
     (unify (parse-term t1) (parse-term t2))]
    [`(=/= ,t1 ,t2)
     (diseq (parse-term t1) (parse-term t2))]
    ['succeed (succeed)]
    ['fail (fail)]
    [`(,r . ,terms)
     #:when (symbol? r)
     (relcall (relname r) (map parse-term terms))]
    [_ (error 'parse-goal/surface-mini "invalid goal form: ~e" goal)]))

(define (parse-clause/surface-mini goals)
  (conj-goals/left (map parse-goal/surface-mini goals)))

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
    [`(fresh ,vars . ,rest)
     (error 'parse-goal/micro
            "micro fresh expects exactly one body goal, got ~a"
            (length rest))]
    [`(conj ,g1 ,g2)
     (conj (parse-goal/micro g1) (parse-goal/micro g2))]
    [`(disj ,g1 ,g2)
     (disj (parse-goal/micro g1) (parse-goal/micro g2))]
    [`(Zzz ,g)
     (delay-goal (parse-goal/micro g))]
    [`(Zzz . ,rest)
     (error 'parse-goal/micro
            "micro Zzz expects exactly one goal, got ~a"
            (length rest))]
    [`(delay . ,_)
     (error 'parse-goal/micro
            "direct micro source uses Zzz for explicit goal delay; delay is internal-only")]
    [`(== ,t1 ,t2)
     (unify (parse-term t1) (parse-term t2))]
    [`(=/= ,t1 ,t2)
     (diseq (parse-term t1) (parse-term t2))]
    ['succeed (succeed)]
    ['fail (fail)]
    [`(conde . ,_)
     (error 'parse-goal/micro "conde is not part of direct micro source mode")]
    [`(proceed . ,_)
     (error 'parse-goal/micro "proceed is internal-only and not valid in micro source mode")]
    [`(,r . ,terms)
     #:when (and (symbol? r) (not (reserved-goal-symbol? r)))
     (relcall (relname r) (map parse-term terms))]
    [_ (error 'parse-goal/micro "invalid micro goal form: ~e" goal)]))

(define (term->micro-datum t)
  (cond
    [(konst? t)
     (define k (konst-k t))
     (cond
       [(symbol? k) `(quote ,k)]
       [(string? k) k]
       [(boolean? k) k]
       [(number? k) k]
       [else (error 'term->micro-datum "unexpected konst payload: ~e" k)])]
    [(nil? t) '(quote ())]
    [(kons? t) `(cons ,(term->micro-datum (kons-a t))
                      ,(term->micro-datum (kons-d t)))]
    [(var? t) (var-v t)]
    [(relname? t) (relname-name t)]
    [else (error 'term->micro-datum "unexpected term AST: ~e" t)]))

(define (goal->micro-datum g)
  (cond
    [(unify? g)
     `(== ,(term->micro-datum (unify-t1 g))
          ,(term->micro-datum (unify-t2 g)))]
    [(diseq? g)
     `(=/= ,(term->micro-datum (diseq-t1 g))
           ,(term->micro-datum (diseq-t2 g)))]
    [(succeed? g) 'succeed]
    [(fail? g) 'fail]
    [(fresh? g)
     `(fresh ,(map term->micro-datum (fresh-vars g))
        ,(goal->micro-datum (fresh-goal g)))]
    [(conj? g)
     `(conj ,(goal->micro-datum (conj-g1 g))
            ,(goal->micro-datum (conj-g2 g)))]
    [(disj? g)
     `(disj ,(goal->micro-datum (disj-g1 g))
            ,(goal->micro-datum (disj-g2 g)))]
    [(delay-goal? g)
     `(Zzz ,(goal->micro-datum (delay-goal-goal g)))]
    [(compiled-delay-goal? g)
     `(Zzz ,(goal->micro-datum (compiled-delay-goal-goal g)))]
    [(relcall? g)
     `(,(term->micro-datum (relcall-name g))
       ,@(map term->micro-datum (relcall-terms g)))]
    [else (error 'goal->micro-datum "unexpected goal AST: ~e" g)]))

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

(define (render-micro-source lst
                             #:source-mode [source-mode default-source-mode]
                             #:compile-profile [compile-profile #f])
  (program->micro-string
   (parse-prog->ast lst
                    #:source-mode source-mode
                    #:compile-profile compile-profile)))

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
(define (prepare-program lst
                         [source-mode default-source-mode]
                         [compile-profile #f])
  (define source-mode* (normalize-source-mode source-mode))
  (define compile-profile*
    (normalize-compile-profile compile-profile source-mode*))
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

  (case (string->symbol source-mode*)
    [(mini)
     (define display-ast
       (prog (map parse-relation-def/surface-mini defrels)
             (parse-run/surface-mini run)))
     (define normalized-ast
       (certify-guarded-mini-program
        (mini-ast->normalized-micro display-ast compile-profile*)
        compile-profile*))
     (values normalized-ast display-ast compile-profile*)]
    [(micro)
     (define normalized-ast
       (prog (map parse-relation-def/micro defrels)
             (parse-run/micro run)))
     (values normalized-ast normalized-ast compile-profile*)]
    [else
     (error 'prepare-program
            "unsupported source mode ~e"
            source-mode*)]))

(define (parse-prog->ast lst
                         #:source-mode [source-mode default-source-mode]
                         #:compile-profile [compile-profile #f])
  (define-values (normalized-ast _display-ast _profile)
    (prepare-program lst source-mode compile-profile))
  normalized-ast)

(define (parse-prog lst)
  ;; Parse AST
  (define-values (_normalized-ast display-ast _profile)
    (prepare-program lst default-source-mode #f))

  ;; Transpile AST to redex program and collect generated GUIDs
  (define-values (REDEX-PROG counter guid-list)
    (transpile display-ast 0))

  ;; Tag AST with guids
  (define-values (GUID-PROG _) (add-guids display-ast 0 guid-list))

  ;; Return both programs
  (values REDEX-PROG GUID-PROG))

;; ---------- Canonical parser projection ----------

(define (id->label id)
  `(label ,id))

(define (konst->canonical-term const)
  (match const
    [(konst s) #:when (symbol? s) `(sym ,(symbol->string s))]
    [(konst s) #:when (string? s) `(str ,s)]
    [(konst b) #:when (boolean? b) b]
    [(konst n) #:when (number? n) `(nat ,n)]))

(define (unwrap-symbolish v who)
  (cond
    [(symbol? v) v]
    [(var? v) (unwrap-symbolish (var-v v) who)]
    [else (error who "expected symbol-like value, got ~a" v)]))

(define (next-hidden-id counter)
  (values (string-append "y" (number->string counter)) (add1 counter)))

(define (transpile-canonical expr count hidden-count)
  (match expr
    [(prog rels q)
     #:when (prog? expr)
     (define-values (trs count1 hidden1 guids1)
       (let loop ([rest rels] [acc '()] [count* count] [hidden* hidden-count] [guids '()])
         (match rest
           ['()
            (values (reverse acc) count* hidden* guids)]
           [(cons rel rels-tail)
            (define-values (trel count** hidden** rel-guids)
              (transpile-canonical rel count* hidden*))
            (loop rels-tail
                  (cons trel acc)
                  count**
                  hidden**
                  (append guids rel-guids))])))
     (define-values (tq count2 hidden2 guids2)
       (transpile-canonical q count1 hidden1))
     (values `(,trs ,tq (empty-stream)) count2 hidden2 (append guids1 guids2))]

    [(fresh vars goal)
     #:when (fresh? expr)
     (define-values (id count1) (next-g-id "f" count))
     (define-values (tvars count2 hidden1 guids1)
       (let loop ([rest vars] [acc '()] [count* count1] [hidden* hidden-count] [guids '()])
         (match rest
           ['()
            (values (reverse acc) count* hidden* guids)]
           [(cons v vars-tail)
            (define-values (tv count** hidden** var-guids)
              (transpile-canonical v count* hidden*))
            (loop vars-tail
                  (cons tv acc)
                  count**
                  hidden**
                  (append guids var-guids))])))
     (define-values (tgoal count3 hidden2 guids2)
       (transpile-canonical goal count2 hidden1))
     (values `(∃ ,tvars ,tgoal ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(conde clauses)
     #:when (conde? expr)
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

    [(conj g1 g2)
     #:when (conj? expr)
     (define-values (id count1) (next-g-id "c" count))
     (define-values (tg1 count2 hidden1 guids1)
       (transpile-canonical g1 count1 hidden-count))
     (define-values (tg2 count3 hidden2 guids2)
       (transpile-canonical g2 count2 hidden1))
     (values `(,tg1 ∧ ,tg2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(disj g1 g2)
     #:when (disj? expr)
     (define-values (id count1) (next-g-id "d" count))
     (define-values (tg1 count2 hidden1 guids1)
       (transpile-canonical g1 count1 hidden-count))
     (define-values (tg2 count3 hidden2 guids2)
       (transpile-canonical g2 count2 hidden1))
     (values `(,tg1 ∨ ,tg2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(unify t1 t2)
     #:when (unify? expr)
     (define-values (id count1) (next-g-id "u" count))
     (define-values (tt1 count2 hidden1 guids1)
       (transpile-canonical t1 count1 hidden-count))
     (define-values (tt2 count3 hidden2 guids2)
       (transpile-canonical t2 count2 hidden1))
     (values `(,tt1 =? ,tt2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(diseq t1 t2)
     #:when (diseq? expr)
     (define-values (id count1) (next-g-id "n" count))
     (define-values (tt1 count2 hidden1 guids1)
       (transpile-canonical t1 count1 hidden-count))
     (define-values (tt2 count3 hidden2 guids2)
       (transpile-canonical t2 count2 hidden1))
     (values `(,tt1 != ,tt2 ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(succeed)
     #:when (succeed? expr)
     (values `(succeed (label "succeed")) count hidden-count '())]

    [(fail)
     #:when (fail? expr)
     (values `(fail (label "fail")) count hidden-count '())]

    [(delay-goal goal)
     #:when (delay-goal? expr)
     (define-values (id count1) (next-g-id "y" count))
     (define-values (tg count2 hidden1 guids1)
       (transpile-canonical goal count1 hidden-count))
     (values `(sdelay ,tg ,(id->label id))
             count2 hidden1
             (cons id guids1))]

    [(compiled-delay-goal goal)
     #:when (compiled-delay-goal? expr)
     (define-values (id hidden1) (next-hidden-id hidden-count))
     (define-values (tg count1 hidden2 guids1)
       (transpile-canonical goal count hidden1))
     (values `(sdelay ,tg ,(id->label id))
             count1 hidden2
             guids1)]

    [(relcall name terms)
     #:when (relcall? expr)
     (define-values (id count1) (next-g-id "r" count))
     (define-values (tname count2 hidden1 guids1)
       (transpile-canonical name count1 hidden-count))
     (define-values (tterms count3 hidden2 guids2)
       (let loop ([rest terms] [acc '()] [count* count2] [hidden* hidden1] [guids '()])
         (match rest
           ['()
            (values (reverse acc) count* hidden* guids)]
           [(cons t terms-tail)
            (define-values (tt count** hidden** term-guids)
              (transpile-canonical t count* hidden*))
            (loop terms-tail
                  (cons tt acc)
                  count**
                  hidden**
                  (append guids term-guids))])))
     (values `(,tname ,@tterms ,(id->label id))
             count3 hidden2
             (cons id (append guids1 guids2)))]

    [(nil)
     #:when (nil? expr)
     (values 'empty count hidden-count '())]

    [(konst _)
     #:when (konst? expr)
     (values (konst->canonical-term expr) count hidden-count '())]

    [(kons a d)
     #:when (kons? expr)
     (define-values (ta count1 hidden1 guids1)
       (transpile-canonical a count hidden-count))
     (define-values (td count2 hidden2 guids2)
       (transpile-canonical d count1 hidden1))
     (values `(,ta : ,td) count2 hidden2 (append guids1 guids2))]

    [(var v)
     #:when (var? expr)
     (define v* (unwrap-symbolish v 'transpile-canonical))
     (values (string->symbol (string-append "x:" (symbol->string v*)))
             count hidden-count '())]

    [(relname name)
     #:when (relname? expr)
     (define name* (unwrap-symbolish name 'transpile-canonical))
     (values (string->symbol (string-append "r:" (symbol->string name*)))
             count hidden-count '())]

    [(defrel name lop goal)
     #:when (defrel? expr)
     (define-values (tname count1 hidden1 guids1)
       (transpile-canonical name count hidden-count))
     (define-values (tlop count2 hidden2 guids2)
       (let loop ([rest lop] [acc '()] [count* count1] [hidden* hidden1] [guids '()])
         (match rest
           ['()
            (values (reverse acc) count* hidden* guids)]
           [(cons v lop-tail)
            (define-values (tv count** hidden** lop-guids)
              (transpile-canonical v count* hidden*))
            (loop lop-tail
                  (cons tv acc)
                  count**
                  hidden**
                  (append guids lop-guids))])))
     (define-values (tgoal count3 hidden3 guids3)
       (transpile-canonical goal count2 hidden2))
     (values `(,tname ,tlop ,tgoal)
             count3 hidden3
             (append guids1 guids2 guids3))]

    [(run _n qs goal)
     #:when (run? expr)
     (define-values (id count1) (next-g-id "f" count))
     (define-values (tq count2 hidden1 guids1)
       (let loop ([rest qs] [acc '()] [count* count1] [hidden* hidden-count] [guids '()])
         (match rest
           ['()
            (values (reverse acc) count* hidden* guids)]
           [(cons q qs-tail)
            (define-values (tqv count** hidden** query-guids)
              (transpile-canonical q count* hidden*))
            (loop qs-tail
                  (cons tqv acc)
                  count**
                  hidden**
                  (append guids query-guids))])))
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
   '(((∃ (x:q)
       (∃ ()
          (((((sym "dog1") =? (sym "cat") "u5")
             ∧ ((sym "bear1") =? x:lion "u6") "c4")
            ∧ ((sym "dog") =? (sym "cat") "u7") "c3")
           ∧ ((sym "bear") =? (sym "lion") "u8") "c2") "f1") "f0")
      (state () 0 () "s"))
     ()))

  (check-equal?
   html-prog
   "\n\n[[f0]](run* (q) [[f1]](fresh ()\n  [[c2]]  [[c3]][[c4]][[u5]](== 'dog1 'cat)[[/u5]]\n  [[u6]](== 'bear1 lion)[[/u6]][[/c4]]\n  [[u7]](== 'dog 'cat)[[/u7]][[/c3]]\n  [[u8]](== 'bear 'lion)[[/u8]][[/c2]])[[/f1]])[[/f0]]"
   )

  )
