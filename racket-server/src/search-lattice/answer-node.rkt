#lang racket

(require racket/hash)

(provide label->visible-id
         term->visible-json
         goal->visible-node
         state->answer-node)

(define u-rx #px"^u:([0-9]+)$")

(define (u-symbol->natural u)
  (define m (and (symbol? u) (regexp-match u-rx (symbol->string u))))
  (and m (string->number (second m))))

(define (extract-name input-str)
  (define re #rx"^[x,r]:([^«]+)")
  (define matches (regexp-match re input-str))
  (if matches (second matches) input-str))

(define (label->visible-id tag)
  (match tag
    [`(label ,s) s]
    [(? symbol? s) (symbol->string s)]
    [else (format "~a" tag)]))

(define (term->visible-json t)
  (match t
    [(? symbol? x)
     (define n (u-symbol->natural x))
     (if n n (hasheq 'var (extract-name (symbol->string x))))]
    ['empty '()]
    [`(sym ,string) (hasheq 'sym string)]
    [`(nat ,natural) (hasheq 'num natural)]
    [`(str ,string) string]
    [`(,t_1 : ,t_2)
     (cons (term->visible-json t_1)
           (list->visible-list t_2))]
    [else t]))

(define (list->visible-list t)
  (match t
    ['empty '()]
    [`(,t_1 : ,t_2)
     (cons (term->visible-json t_1)
           (list->visible-list t_2))]
    [_ (list (term->visible-json t))]))

(define (term->reify t)
  (match t
    [(? symbol? x)
     (define n (u-symbol->natural x))
     (if n n x)]
    [`(,t_1 : ,t_2)
     `(,(term->reify t_1) : ,(term->reify t_2))]
    [`(str ,s) s]
    [_ t]))

(define (sub->visible-json sub)
  (map (match-lambda
         [(list u t)
          (define key (or (u-symbol->natural u) u))
          (hasheq 'key key
                  'value (term->visible-json t))])
       sub))

(define (dis->visible-json dis)
  (map (match-lambda
         [(list t1 t2)
          (hasheq 'left (term->visible-json t1)
                  'right (term->visible-json t2))])
       dis))

(define (trail->visible-json trail)
  (map (lambda (crumb)
         (match crumb
           [`(,t_1 =? ,t_2 ,tag)
            (hasheq 'left (term->visible-json t_1)
                    'right (term->visible-json t_2)
                    'id (label->visible-id tag))]
           [_ (hasheq 'left crumb 'right crumb 'id "bad-trail")]))
       trail))

(define (state-c-bound c)
  (cond
    [(null? c) 0]
    [else
     (add1 (for/fold ([mx -1]) ([u (in-list c)])
             (max mx (or (u-symbol->natural u) -1))))]))

(define (reified? r)
  (and (symbol? r)
       (let ([s (symbol->string r)])
         (and (>= (string-length s) 2)
              (char=? (string-ref s 0) #\_)
              (char=? (string-ref s 1) #\.)))))

(define (mk->json expr)
  (match expr
    [ref #:when (reified? ref) (symbol->string ref)]
    [sym #:when (symbol? sym) (hasheq 'sym (symbol->string sym))]
    [nat #:when (natural? nat) (hasheq 'num nat)]
    [(cons t1 t2) (hasheq 'pair (list (mk->json t1) (mk->json t2)))]
    [_ expr]))

(define (underscore-symbol n)
  (string->symbol (string-append "_" (number->string n))))

(define (generate-fresh-names c)
  (map underscore-symbol (range 1 c)))

(define (generate-query-vars n)
  (for/list ([_ (in-range n)]) (gensym)))

(define (canonical-term->mk t)
  (match t
    [`(,a : ,d) `(cons ,(canonical-term->mk a) ,(canonical-term->mk d))]
    ['empty '(quote ())]
    [(? number? n) (underscore-symbol n)]
    [`(sym ,s) `(quote ,(string->symbol s))]
    [`(nat ,n) n]
    [`(str ,s) s]
    [other other]))

(define/match (make-unify-clause query-vars n pair)
  [(query-vars n (list l r))
   (define lhs
     (if (< l n)
         (list-ref query-vars l)
         (underscore-symbol l)))
   (define rhs
     (if (and (number? r) (< r n))
         (list-ref query-vars r)
         (canonical-term->mk r)))
   `(== ,lhs ,rhs)])

(define/match (make-diseq-clause query-vars n pair)
  [(query-vars n (list l r))
   (define lhs
     (if (< l n)
         (list-ref query-vars l)
         (underscore-symbol l)))
   (define rhs
     (if (and (number? r) (< r n))
         (list-ref query-vars r)
         (canonical-term->mk r)))
   `(=/= ,lhs ,rhs)])

(define (prepare-minikanren-namespace)
  (let ([ns (make-base-namespace)])
    (parameterize ([current-namespace ns])
      (namespace-require 'hosted-minikanren))
    ns))

(define (run-in-namespace ns query-vars fresh-names clauses)
  (match (eval `(run* ,query-vars
                      (fresh ,fresh-names
                             ,@clauses))
               ns)
    [(list result _ ...)
     result]
    [result
     (error 'run-in-namespace
            "expected a non-empty result list, got ~e"
            result)]))

(define (map/pair f p)
  (match p
    [`(,a . ,d) (cons (f a) (map/pair f d))]
    [else (list (f p))]))

(define (process-reify-result result)
  (cond
    [(list? result) (map mk->json result)]
    [(pair? result) (map/pair mk->json result)]
    [else (mk->json result)]))

(define (reify-state sub dis c n)
  (cond
    [(zero? n) '()]
    [else
     (let* ([fresh-names (generate-fresh-names c)]
            [query-vars (generate-query-vars n)]
            [unify-clauses (map (lambda (p) (make-unify-clause query-vars n p))
                                (sub->reify sub))]
            [diseq-clauses (map (lambda (p) (make-diseq-clause query-vars n p))
                                (dis->reify dis))]
            [clauses (append unify-clauses diseq-clauses)]
            [ns (prepare-minikanren-namespace)]
            [raw-result (run-in-namespace ns
                                          query-vars
                                          fresh-names
                                          (if (null? clauses)
                                              (list '(== 1 1))
                                              clauses))])
       (process-reify-result raw-result))]))

(define (sub->reify sub)
  (for/list ([pr (in-list sub)])
    (match-define (list u t) pr)
    (list (or (u-symbol->natural u) u)
          (term->reify t))))

(define (dis->reify dis)
  (for/list ([pr (in-list dis)])
    (match-define (list t1 t2) pr)
    (list (term->reify t1)
          (term->reify t2))))

(define (goal->visible-node g)
  (match g
    [`(succeed ,_tag)
     (hasheq 'name "Succeed"
             'renderRole "terminal")]
    [`(fail ,_tag)
     (hasheq 'name "Fail"
             'renderRole "terminal")]
    [`(,t_1 =? ,t_2 ,tag)
     (hasheq 'name "Unify"
             'renderRole "goal-leaf"
             'id (label->visible-id tag)
             'left (term->visible-json t_1)
             'right (term->visible-json t_2))]
    [`(,t_1 != ,t_2 ,tag)
     (hasheq 'name "Disequality"
             'renderRole "goal-leaf"
             'id (label->visible-id tag)
             'left (term->visible-json t_1)
             'right (term->visible-json t_2))]
    [`(,r ,t ... ,tag)
     #:when (and (symbol? r)
                 (regexp-match? #rx"^r:" (symbol->string r)))
     (hasheq 'name "Rel-Call"
             'renderRole "goal-leaf"
             'id (label->visible-id tag)
             'rel (extract-name (symbol->string r))
             'args (map term->visible-json t))]
    [`(,g_1 ∨ ,g_2 ,tag)
     (hasheq 'name "Goal-Disj"
             'renderRole "goal-branch"
             'id (label->visible-id tag)
             'focusColor "#ff8000"
             'activeChildIndex 0
             'children (list (goal->visible-node g_1)
                             (goal->visible-node g_2)))]
    [`(,g_1 ∧ ,g_2 ,tag)
     (hasheq 'name "Goal-Conj"
             'renderRole "goal-branch"
             'id (label->visible-id tag)
             'focusColor "blue"
             'activeChildIndex 0
             'children (list (goal->visible-node g_1)
                             (goal->visible-node g_2)))]
    [`(suspend ,g_1 ,tag)
     (hasheq 'name "Goal-Delay"
             'renderRole "delay"
             'id (label->visible-id tag)
             'activeChildIndex 0
             'children (list (goal->visible-node g_1)))]
    [`(∃ ,d ,g_1 ,tag)
     (hasheq 'name "Fresh"
             'renderRole "goal-fresh"
             'id (label->visible-id tag)
             'activeChildIndex 0
             'vars (map term->visible-json d)
             'children (list (goal->visible-node g_1)))]
    [_ (hasheq 'name "Goal"
               'renderRole "goal")]))

(define (state->answer-node σ num-query-variables)
  (match σ
    [`(state ,sub ,dis ,c ,trail ,tag)
     (hasheq 'name "Answer"
             'renderRole "answer-node"
             'nodeColor "green"
             'stateId (label->visible-id tag)
             'sub (sub->visible-json sub)
             'disequalities (dis->visible-json dis)
             'trail (trail->visible-json trail)
             'reified (reify-state sub
                                   dis
                                   (state-c-bound c)
                                   num-query-variables))]
    [_ (hasheq 'name "Answer"
               'renderRole "answer-node"
               'nodeColor "green")]))
