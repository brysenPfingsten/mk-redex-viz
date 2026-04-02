#lang racket

(provide parse-prog/canonical
         parse-prog->ast
         render-micro-source
         default-source-mode
         normalize-source-mode
         (struct-out compile-profile)
         canonical-compile-profile
         canonical-compile-profile-jsexpr
         normalize-compile-profile
         compile-profile->jsexpr
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
(define canonical-parser-profile "surface->canonical")
(define canonical-parser-target-id "canonical/config")

(define default-source-mode "mini")

(define/match (compile-profile->jsexpr profile)
  [((compile-profile conj-assoc disj-assoc delay-placement))
  (hasheq 'conjAssoc conj-assoc
          'disjAssoc disj-assoc
          'delayPlacement delay-placement)])

(define canonical-compile-profile
  (compile-profile "left" "right" "relbody"))

(define canonical-compile-profile-jsexpr
  (compile-profile->jsexpr canonical-compile-profile))

(define (normalize-source-mode maybe-mode)
  (match maybe-mode
    [(or #f "") default-source-mode]
    [(or "mini" "micro") maybe-mode]
    [_
     (error 'normalize-source-mode
            "unsupported sourceMode ~e; expected \"mini\" or \"micro\""
            maybe-mode)]))

(define (normalize-axis maybe-value valid-values key)
  (match maybe-value
    [#f #f]
    [`,v #:when (member v valid-values) maybe-value]
    [_
     (error 'normalize-compile-profile
            "invalid compileProfile.~a ~e; expected one of ~e"
            key
            maybe-value
            valid-values)]))

(define (normalize-mini-compile-profile maybe-profile)
  (match maybe-profile
    [#f canonical-compile-profile]
    [(? compile-profile?) maybe-profile]
    [(? hash? profile)
     (match (list (normalize-axis (hash-ref profile 'conjAssoc #f)
                                  '("left" "right")
                                  'conjAssoc)
                  (normalize-axis (hash-ref profile 'disjAssoc #f)
                                  '("left" "right")
                                  'disjAssoc)
                  (normalize-axis (hash-ref profile 'delayPlacement #f)
                                  '("relbody" "relcall" "disj")
                                  'delayPlacement))
       [(list (? string? conj-assoc)
              (? string? disj-assoc)
              (? string? delay-placement))
        (compile-profile conj-assoc disj-assoc delay-placement)]
       [_ 
        (error 'normalize-compile-profile
               "compileProfile must contain conjAssoc, disjAssoc, and delayPlacement")])]
    [_
     (error 'normalize-compile-profile
            "compileProfile must be a hash or compile-profile, got ~e"
            maybe-profile)]))

(define (normalize-compile-profile maybe-profile [source-mode default-source-mode])
  (match source-mode
    ["micro"
     (when maybe-profile
       (error 'normalize-compile-profile
              "compileProfile is only valid when sourceMode is \"mini\""))
     #f]
    ["mini"
     (normalize-mini-compile-profile maybe-profile)]
    [_
     (error 'normalize-compile-profile
            "unsupported sourceMode ~e; expected \"mini\" or \"micro\""
            source-mode)]))

;; map/fold: (T A -> (values R A)) (listof T) A -> (values (listof R) A)
;; Purpose: Like map, but threads an accumulator state through each call.
;;          The function f takes an element of the list and a state,
;;          and returns a result and an updated state.
;;
;; Example:
;;   (map/fold (λ (x s) (values (+ x s) (* s 2))) '(1 2 3) 1)
;;    => values '(2 4 7), 8
(define (map/fold f lst init-state)
  (define-values (rev-acc state)
    (for/fold ([rev-acc '()]
               [state init-state])
              ([x (in-list lst)])
      (define-values (v next-state) (f x state))
      (values (cons v rev-acc) next-state)))
  (values (reverse rev-acc) state))

;; next-g-id: String Number -> (values String Number)
;; Purpose: Creates a GUID based on the given prefix and counter and returns
;;          the GUID and the next count
(define (next-g-id prefix counter)
  (values (string-append prefix (number->string counter)) (add1 counter)))

;; konst->string: konst -> string
;; Purpose: Convert a konst structure to a string
(define (konst->string const)
  (match const
    [(konst s) #:when (symbol? s) (format "'~a" (symbol->string s))]
    [(konst s) #:when (string? s) s]
    [(konst b) #:when (boolean? b) (if b "#t" "#f")]
    [(konst n) #:when (number? n) (number->string n)]))

;; `kons` currently loses some concrete source distinctions among list/pair forms.
;; Reconstructing the original surface spelling would require carrying either
;; pair/list provenance through compilation or a parallel CST-style structure.

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
     (match-define (cons id rest) guids)
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
     (match-define (cons id rest) guids)
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

    [(unify t1 t2)
     #:when (unify? expr)
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

    [(diseq t1 t2)
     #:when (diseq? expr)
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

    [(succeed)
     #:when (succeed? expr)
     (values (format "~a(succeed)" (make-string s #\space)) guids)]

    [(fail)
     #:when (fail? expr)
     (values (format "~a(fail)" (make-string s #\space)) guids)]

    [(disj g1 g2)
     #:when (disj? expr)
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

    [(relcall name terms)
     #:when (relcall? expr)
     (match-define (cons id rest) guids)
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
     (match-define (cons id rest) guids)
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

(define (reserved-goal-symbol? sym)
  (and (symbol? sym)
       (member sym '(fresh conde conj disj Zzz delay == =/= succeed fail proceed))))

(define (conj-goals/left goals)
  (match goals
    [(list goal) goal]
    [(cons goal (cons next more))
     (conj-goals/left
      (cons (conj goal next) more))]
    [_ (error 'conj-goals/left "expected a non-empty goal sequence")]))

(define (combine-conj goals assoc)
  (match goals
    [(list goal) goal]
    [(cons goal (cons next more))
     (if (equal? assoc "left")
         (combine-conj (cons (conj goal next) more)
                       assoc)
         (conj goal (combine-conj (cons next more) assoc)))]
    [_ (error 'combine-conj "expected a non-empty goal sequence")]))

(define (combine-disj goals assoc)
  (match goals
    [(list goal) goal]
    [(cons goal (cons next more))
     (if (equal? assoc "left")
         (combine-disj (cons (disj goal next) more)
                       assoc)
         (disj goal (combine-disj (cons next more) assoc)))]
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

(define/match (surface-goal->micro goal profile)
  [(goal (and profile (compile-profile conj-assoc disj-assoc _)))
  (match goal
    [(fresh vars g)
     (fresh vars (surface-goal->micro g profile))]
    [(conde clauses)
     (combine-disj
      (for/list ([clause (in-list clauses)])
        (surface-goal->micro clause profile))
      disj-assoc)]
    [(conj _ _)
     (combine-conj
      (for/list ([piece (in-list (flatten-conj-tree goal))])
        (surface-goal->micro piece profile))
      conj-assoc)]
    [(disj g1 g2)
     (disj (surface-goal->micro g1 profile)
           (surface-goal->micro g2 profile))]
    [(delay-goal g)
     (delay-goal (surface-goal->micro g profile))]
    [_ goal])])

(define (apply-delay-placement goal placement [wrapper delay-goal])
  (case (string->symbol placement)
    [(relcall) (wrap-relcalls goal wrapper)]
    [(disj) (wrap-disjs goal wrapper)]
    [else goal]))

(define/match (mini-ast->normalized-micro ast profile)
  [((prog rels (run n q goal))
    (and profile (compile-profile _ _ delay-placement)))
   (define normalized-rels
     (for/list ([rel (in-list rels)])
       (match-define (defrel name lop rel-goal) rel)
       (define normalized-goal (surface-goal->micro rel-goal profile))
       (defrel name
               lop
               (if (equal? delay-placement "relbody")
                   (compiled-delay-goal normalized-goal)
                   (apply-delay-placement normalized-goal
                                          delay-placement
                                          compiled-delay-goal)))))
   (prog normalized-rels
         (run n
              q
              (apply-delay-placement (surface-goal->micro goal profile)
                                     delay-placement
                                     compiled-delay-goal)))]
  [(ast _)
   (error 'mini-ast->normalized-micro
          "unexpected source AST shape: ~e"
          ast)])

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
    [(or (delay-goal (disj g1 g2))
         (compiled-delay-goal (disj g1 g2)))
     (and (disj-certifies? g1)
          (disj-certifies? g2))]
    [(or (delay-goal _)
         (compiled-delay-goal _)
		 (disj _ _))
	 #f]
    [(fresh _ g) (disj-certifies? g)]
    [(conj g1 g2)
     (and (disj-certifies? g1)
          (disj-certifies? g2))]
    [_ #t]))

(define/match (certify-guarded-mini-program ast profile)
  [((prog rels (run _ _ query-goal)) (compile-profile _ _ placement))
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
  [(ast _)
   (error 'certify-guarded-mini-program
          "unexpected normalized program shape: ~e"
          ast)])

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
    ;; Specific diagnostics for malformed source forms and internal-only names.
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
    ;; Internal AST forms that still serialize back to direct micro surface syntax.
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
      (match-define (cons defrels-rev run-expr) result)
      (values (reverse defrels-rev) run-expr)))

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

(define (flatten-guid-groups reversed-guid-groups [acc '()])
  (match reversed-guid-groups
    ['() acc]
    [(cons guid-group rest)
     (flatten-guid-groups rest (append guid-group acc))]))

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
    [(prog rels q)
     #:when (prog? expr)
     (define-values (trs count1 hidden1 guids1)
       (transpile-canonical/list rels count hidden-count))
     (define-values (tq count2 hidden2 guids2)
       (transpile-canonical q count1 hidden1))
     (values `(,trs ,tq (empty-stream)) count2 hidden2 (append guids1 guids2))]

    [(fresh vars goal)
     #:when (fresh? expr)
     (define-values (id count1) (next-g-id "f" count))
     (define-values (tvars count2 hidden1 guids1)
       (transpile-canonical/list vars count1 hidden-count))
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
     (values `(suspend ,tg ,(id->label id))
             count2 hidden1
             (cons id guids1))]

    [(compiled-delay-goal goal)
     #:when (compiled-delay-goal? expr)
     (define-values (id hidden1) (next-hidden-id hidden-count))
     (define-values (tg count1 hidden2 guids1)
       (transpile-canonical goal count hidden1))
     (values `(suspend ,tg ,(id->label id))
             count1 hidden2
             guids1)]

    [(relcall name terms)
     #:when (relcall? expr)
     (define-values (id count1) (next-g-id "r" count))
     (define-values (tname count2 hidden1 guids1)
       (transpile-canonical name count1 hidden-count))
     (define-values (tterms count3 hidden2 guids2)
       (transpile-canonical/list terms count2 hidden1))
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
       (transpile-canonical/list lop count1 hidden1))
     (define-values (tgoal count3 hidden3 guids3)
       (transpile-canonical goal count2 hidden2))
     (values `(,tname ,tlop ,tgoal)
             count3 hidden3
             (append guids1 guids2 guids3))]

    [(run _n qs goal)
     #:when (run? expr)
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
