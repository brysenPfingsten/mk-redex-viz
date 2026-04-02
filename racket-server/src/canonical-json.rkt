#lang racket

(require json
         racket/hash)

(provide to-json/canonical
         num-query-vars/canonical)

(define u-rx #px"^u:([0-9]+)$")

(define (u-symbol->natural u)
  (define m (and (symbol? u) (regexp-match u-rx (symbol->string u))))
  (and m (string->number (second m))))

(define (extract-name input-str)
  (define re #rx"^[x,r]:([^«]+)")
  (define matches (regexp-match re input-str))
  (if matches (second matches) input-str))

(define (label->id tag)
  (match tag
    [`(label ,s) s]
    [(? symbol? s) (symbol->string s)]
    [else (format "~a" tag)]))

(define (term->json/canonical t)
  (match t
    [(? symbol? x)
     (define n (u-symbol->natural x))
     (if n n (hasheq 'var (extract-name (symbol->string x))))]
    ['empty '()]
    [`(sym ,string) (hasheq 'sym string)]
    [`(nat ,natural) (hasheq 'num natural)]
    [`(str ,string) string]
    [`(,t_1 : ,t_2)
     (cons (term->json/canonical t_1)
           (list->list/canonical t_2))]
    [else t]))

(define (list->list/canonical t)
  (match t
    ['empty '()]
    [`(,t_1 : ,t_2)
     (cons (term->json/canonical t_1)
           (list->list/canonical t_2))]
    [_ (list (term->json/canonical t))]))

(define (term->reify/canonical t)
  (match t
    [(? symbol? x)
     (define n (u-symbol->natural x))
     (if n n x)]
    [`(,t_1 : ,t_2)
     `(,(term->reify/canonical t_1) : ,(term->reify/canonical t_2))]
    [`(str ,s) s]
    [_ t]))

(define (sub->json/canonical sub)
  (map (match-lambda
         [(list u t)
          (define key (or (u-symbol->natural u) u))
          (hasheq 'key key
                  'value (term->json/canonical t))])
       sub))

(define (dis->json/canonical dis)
  (map (match-lambda
         [(list t1 t2)
          (hasheq 'left (term->json/canonical t1)
                  'right (term->json/canonical t2))])
       dis))

(define (trail->json/canonical trail)
  (map (lambda (crumb)
         (match crumb
           [`(,t_1 =? ,t_2 ,tag)
            (hasheq 'left (term->json/canonical t_1)
                    'right (term->json/canonical t_2)
                    'id (label->id tag))]
           [_ (hasheq 'left crumb 'right crumb 'id "bad-trail")]))
       trail))

(define (state-c-bound/canonical c)
  (cond
    [(null? c) 0]
    [else
     (add1 (for/fold ([mx -1]) ([u (in-list c)])
             (max mx (or (u-symbol->natural u) -1))))]))

;; ---------- Reification helpers for canonical renderer ----------

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
  (car (eval `(run* ,query-vars
                    (fresh ,fresh-names
                           ,@clauses))
             ns)))

(define (map/pair f p)
  (match p
    [`(,a . ,d) (cons (f a) (map/pair f d))]
    [else (list (f p))]))

(define (process-reify-result result)
  (cond
    [(list? result) (map mk->json result)]
    [(pair? result) (map/pair mk->json result)]
    [else (mk->json result)]))

(define (reify/canonical sub dis c n)
  (cond
    [(zero? n) '()]
    [else
     (let* ([fresh-names (generate-fresh-names c)]
            [query-vars (generate-query-vars n)]
            [unify-clauses (map (lambda (p) (make-unify-clause query-vars n p)) sub)]
            [diseq-clauses (map (lambda (p) (make-diseq-clause query-vars n p)) dis)]
            [clauses (append unify-clauses diseq-clauses)]
            [ns (prepare-minikanren-namespace)]
            [raw-result (run-in-namespace ns
                                          query-vars
                                          fresh-names
                                          (if (null? clauses)
                                              (list '(== 1 1))
                                              clauses))])
       (process-reify-result raw-result))]))

(define (sub->reify/canonical sub)
  (for/list ([pr (in-list sub)])
    (match-define (list u t) pr)
    (list (or (u-symbol->natural u) u)
          (term->reify/canonical t))))

(define (dis->reify/canonical dis)
  (for/list ([pr (in-list dis)])
    (match-define (list t1 t2) pr)
    (list (term->reify/canonical t1)
          (term->reify/canonical t2))))

(define (goal->json/canonical g)
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
             'id (label->id tag)
             'left (term->json/canonical t_1)
             'right (term->json/canonical t_2))]
    [`(,t_1 != ,t_2 ,tag)
     (hasheq 'name "Disequality"
             'renderRole "goal-leaf"
             'id (label->id tag)
             'left (term->json/canonical t_1)
             'right (term->json/canonical t_2))]
    [`(,r ,t ... ,tag)
     #:when (and (symbol? r)
                 (regexp-match? #rx"^r:" (symbol->string r)))
     (hasheq 'name "Rel-Call"
             'renderRole "goal-leaf"
             'id (label->id tag)
             'rel (extract-name (symbol->string r))
             'args (map term->json/canonical t))]
    [`(,g_1 ∨ ,g_2 ,tag)
     (hasheq 'name "Goal-Disj"
             'renderRole "goal-branch"
             'id (label->id tag)
             'focusColor "#ff8000"
             'activeChildIndex 0
             'children (list (goal->json/canonical g_1)
                             (goal->json/canonical g_2)))]
    [`(,g_1 ∧ ,g_2 ,tag)
     (hasheq 'name "Goal-Conj"
             'renderRole "goal-branch"
             'id (label->id tag)
             'focusColor "blue"
             'activeChildIndex 0
             'children (list (goal->json/canonical g_1)
                             (goal->json/canonical g_2)))]
    [`(suspend ,g_1 ,tag)
     (hasheq 'name "Goal-Delay"
             'renderRole "delay"
             'id (label->id tag)
             'activeChildIndex 0
             'children (list (goal->json/canonical g_1)))]
    [`(∃ ,d ,g_1 ,tag)
     (hasheq 'name "Fresh"
             'renderRole "goal-fresh"
             'id (label->id tag)
             'activeChildIndex 0
             'vars (map term->json/canonical d)
             'children (list (goal->json/canonical g_1)))]
    [_ (hasheq 'name "Goal"
               'renderRole "goal")]))

(define (state->answer-leaf-json/canonical σ num-query-variables)
  (match σ
    [`(state ,sub ,dis ,c ,trail ,tag)
     (hasheq 'name "Answer"
             'renderRole "answer-node"
             'nodeColor "green"
             'stateId (label->id tag)
             'sub (sub->json/canonical sub)
             'disequalities (dis->json/canonical dis)
             'trail (trail->json/canonical trail)
             'reified (reify/canonical (sub->reify/canonical sub)
                                       (dis->reify/canonical dis)
                                       (state-c-bound/canonical c)
                                       num-query-variables))]
    [_ (hasheq 'name "Answer"
               'renderRole "answer-node"
               'nodeColor "green")]))

(define (normalize-config/canonical cfg)
  (match cfg
    [`(,_gamma ,_f) cfg]
    [f f]))

(define (project-config-tree/canonical cfg)
  (match (normalize-config/canonical cfg)
    [`(,_gamma ,f) f]
    [f f]
    [_ '(empty-tree)]))

(define (empty-json/canonical)
  (hasheq 'name "Empty"
          'renderRole "terminal"))

(define (empty-json? node)
  (match node
    [(hash* ['name "Empty"] #:open) #t]
    [_ #f]))

(define (freshened->json/canonical c-intro child-json tag)
  (hasheq 'name "Freshened"
          'renderRole "freshened"
          'id (label->id tag)
          'vars (map term->json/canonical c-intro)
          'activeChildIndex 0
          'children (list child-json)))

(define (bounced->json/canonical child-json)
  (hasheq 'name "Bounced"
          'renderRole "bounced"
          'activeChildIndex 0
          'children (list child-json)))

(define (emit-node->json/canonical left-json right-json)
  (hasheq 'name "Emit"
          'renderRole "stream-emit"
          'resolvedChildIndices '(0)
          'resolvedColor "green"
          'activeChildIndex 1
          'children (list left-json right-json)))

(define (tree->json/canonical s num-query-variables)
  (match s
    ['(empty-tree)
     (empty-json/canonical)]
    [(or `(FreshenedTree ,c-intro ,s_1 ,tag)
         `(FreshenedShell ,c-intro ,s_1 ,tag))
     (freshened->json/canonical
      c-intro
      (tree->json/canonical s_1 num-query-variables)
      tag)]
    [`(Bounced ,s_1)
     (bounced->json/canonical
      (tree->json/canonical s_1 num-query-variables))]
    [`(,s_1 + ,s_2)
     (emit-node->json/canonical
      (tree->json/canonical s_1 num-query-variables)
      (tree->json/canonical s_2 num-query-variables))]
    [`(,g (state ,sub ,dis ,c ,trail ,tag))
     #:when (not (equal? g '⊤))
     (hash-union (goal->json/canonical g)
                 (hasheq 'stateId (label->id tag)
                         'sub (sub->json/canonical sub)
                         'disequalities (dis->json/canonical dis)
                         'trail (trail->json/canonical trail)
                         'reified (reify/canonical (sub->reify/canonical sub)
                                                   (dis->reify/canonical dis)
                                                   (state-c-bound/canonical c)
                                                   num-query-variables)))]
    [`(,s_1 <-+ ,s_2)
     (hasheq 'name "<-+"
             'renderRole "search-branch"
             'focusColor "#ff8000"
             'activeChildIndex 0
             'children (list (tree->json/canonical s_1 num-query-variables)
                             (tree->json/canonical s_2 num-query-variables)))]
    [`(,s_1 +-> ,s_2)
     (hasheq 'name "+->"
             'renderRole "search-branch"
             'focusColor "#ff8000"
             'activeChildIndex 1
             'children (list (tree->json/canonical s_1 num-query-variables)
                             (tree->json/canonical s_2 num-query-variables)))]
    [`(,s_1 × ,g ,_c)
     (hasheq 'name "Conjunction"
             'renderRole "search-conjunction"
             'focusColor "blue"
             'activeChildIndex 0
             'children (list (tree->json/canonical s_1 num-query-variables)
                             (goal->json/canonical g)))]
    [`(delay ,s_1)
     (hasheq 'name "Delay"
             'renderRole "delay"
             'activeChildIndex 0
             'children (list (tree->json/canonical s_1 num-query-variables)))]
    [`(state ,sub ,dis ,c ,trail ,tag)
     (state->answer-leaf-json/canonical
      `(state ,sub ,dis ,c ,trail ,tag)
      num-query-variables)]
    [`(⊤ ,σ)
     (state->answer-leaf-json/canonical σ num-query-variables)]
    [_ (error 'tree->json/canonical
              "unknown tree/frontier shape: ~e"
              s)]))

(define (config->tree-json/canonical cfg num-query-variables)
  (tree->json/canonical
   (project-config-tree/canonical cfg)
   num-query-variables))

(define (to-json/canonical cfg num-query-variables)
  (jsexpr->string (config->tree-json/canonical cfg num-query-variables)))

(define (goal-query-vars/canonical g)
  (match g
    [`(∃ ,d ,_ ,_) (length d)]
    [`(suspend ,g_1 ,_) (goal-query-vars/canonical g_1)]
    [`(,g_1 ∧ ,g_2 ,_) (max (goal-query-vars/canonical g_1)
                            (goal-query-vars/canonical g_2))]
    [`(,g_1 ∨ ,g_2 ,_) (max (goal-query-vars/canonical g_1)
                            (goal-query-vars/canonical g_2))]
    [_ 0]))

(define (num-query-vars/work s)
  (match s
    [(or `(FreshenedTree ,_ ,s_1 ,_)
         `(FreshenedShell ,_ ,s_1 ,_))
     (num-query-vars/work s_1)]
    [`(Bounced ,s_1)
     (num-query-vars/work s_1)]
    [`(,s_1 + ,s_2)
     (max (num-query-vars/work s_1)
          (num-query-vars/work s_2))]
    [`(,g ,_σ)
     (goal-query-vars/canonical g)]
    [`(,s_1 × ,g ,_c)
     (max (num-query-vars/work s_1)
          (goal-query-vars/canonical g))]
    [`(,s_1 <-+ ,s_2)
     (max (num-query-vars/work s_1)
          (num-query-vars/work s_2))]
    [`(,s_1 +-> ,s_2)
     (max (num-query-vars/work s_1)
          (num-query-vars/work s_2))]
    [`(delay ,s_1)
     (num-query-vars/work s_1)]
    [_ 0]))

(define (num-query-vars/canonical cfg)
  (match cfg
    [`(,_gamma ,s)
     (num-query-vars/work s)]
    [_ 0]))
