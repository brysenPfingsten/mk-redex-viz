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
  (map (lambda (p)
         (match-define (list u t) p)
         (define key (or (u-symbol->natural u) u))
         (hasheq 'key key
                 'value (term->json/canonical t)))
       sub))

(define (dis->json/canonical dis)
  (map (lambda (p)
         (match-define (list t1 t2) p)
         (hasheq 'left (term->json/canonical t1)
                 'right (term->json/canonical t2)))
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
    [(cons t1 t2) (cons (mk->json t1) (mk->json t2))]
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

(define (make-unify-clause query-vars n pair)
  (match-define (list l r) pair)
  (define lhs
    (if (< l n)
        (list-ref query-vars l)
        (underscore-symbol l)))
  (define rhs
    (if (and (number? r) (< r n))
        (list-ref query-vars r)
        (canonical-term->mk r)))
  `(== ,lhs ,rhs))

(define (make-diseq-clause query-vars n pair)
  (match-define (list l r) pair)
  (define lhs
    (if (< l n)
        (list-ref query-vars l)
        (underscore-symbol l)))
  (define rhs
    (if (and (number? r) (< r n))
        (list-ref query-vars r)
        (canonical-term->mk r)))
  `(=/= ,lhs ,rhs))

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
    (process-reify-result raw-result)))

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
     (hasheq 'name "Succeed")]
    [`(fail ,_tag)
     (hasheq 'name "Fail")]
    [`(,t_1 =? ,t_2 ,tag)
     (hasheq 'name "Unify"
             'id (label->id tag)
             'left (term->json/canonical t_1)
             'right (term->json/canonical t_2))]
    [`(,t_1 != ,t_2 ,tag)
     (hasheq 'name "Disequality"
             'id (label->id tag)
             'left (term->json/canonical t_1)
             'right (term->json/canonical t_2))]
    [`(,r ,t ... ,tag)
     #:when (and (symbol? r)
                 (regexp-match? #rx"^r:" (symbol->string r)))
     (hasheq 'name "Rel-Call"
             'id (label->id tag)
             'rel (extract-name (symbol->string r))
             'args (map term->json/canonical t))]
    [`(,g_1 ∨ ,g_2 ,tag)
     (hasheq 'name "Goal-Disj"
             'id (label->id tag)
             'children (list (goal->json/canonical g_1)
                             (goal->json/canonical g_2)))]
    [`(,g_1 ∧ ,g_2 ,tag)
     (hasheq 'name "Goal-Conj"
             'id (label->id tag)
             'children (list (goal->json/canonical g_1)
                             (goal->json/canonical g_2)))]
    [`(sdelay ,g_1 ,tag)
     (hasheq 'name "Goal-Delay"
             'id (label->id tag)
             'children (list (goal->json/canonical g_1)))]
    [`(∃ ,d ,g_1 ,tag)
     (hasheq 'name "Fresh"
             'id (label->id tag)
             'vars (map term->json/canonical d)
             'children (list (goal->json/canonical g_1)))]
    [_ (hasheq 'name "Goal")]))

(define (state->answer-json/canonical σ num-query-variables [rest #f])
  (match σ
    [`(state ,sub ,dis ,c ,trail ,tag)
     (define base
       (hasheq 'name "Answer"
               'stateId (label->id tag)
               'sub (sub->json/canonical sub)
               'disequalities (dis->json/canonical dis)
               'trail (trail->json/canonical trail)
               'reified (reify/canonical (sub->reify/canonical sub)
                                         (dis->reify/canonical dis)
                                         (state-c-bound/canonical c)
                                         num-query-variables)))
     (if rest
         (hash-set base 'children (list rest))
         base)]
    [_ (hasheq 'name "Answer")]))

(define (project-work-tree/canonical s)
  (match s
    [`(,s_1 × ,g ,c)
     `(,(project-work-tree/canonical s_1) × ,g ,c)]
    [`(,s_1 <-+ ,s_2)
     `(,(project-work-tree/canonical s_1) <-+ ,(project-work-tree/canonical s_2))]
    [`(,s_1 +-> ,s_2)
     `(,(project-work-tree/canonical s_1) +-> ,(project-work-tree/canonical s_2))]
    [`(delay ,s_1)
     `(delay ,(project-work-tree/canonical s_1))]
    [_ s]))

(define (append-stream-prefix/canonical as s)
  (match as
    ['(empty-stream) s]
    [`(⊤ ,σ) `((⊤ ,σ) + ,s)]
    [`((⊤ ,σ) + ,as_tail)
     `((⊤ ,σ) + ,(append-stream-prefix/canonical as_tail s))]
    [_ s]))

(define (project-config-tree/canonical cfg)
  (match cfg
    [`(,_gamma ,s_work ,as)
     (append-stream-prefix/canonical as (project-work-tree/canonical s_work))]
    [`(,_gamma ,s)
     (project-work-tree/canonical s)]
    [_ '(empty-tree)]))

(define (tree->json/canonical s num-query-variables)
  (match s
    ['(empty-tree)
     (hasheq 'name "Empty")]
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
    [`(proceed ((,r ,t ... ,tag-call) (state ,sub ,dis ,c ,trail ,tag-state)))
     (hasheq 'name "Proceed"
             'id (label->id tag-call)
             'stateId (label->id tag-state)
             'goal (goal->json/canonical `(,r ,@t ,tag-call))
             'sub (sub->json/canonical sub)
             'disequalities (dis->json/canonical dis)
             'trail (trail->json/canonical trail)
             'reified (reify/canonical (sub->reify/canonical sub)
                                       (dis->reify/canonical dis)
                                       (state-c-bound/canonical c)
                                       num-query-variables))]
    [`(proceed (,g (state ,sub ,dis ,c ,trail ,tag-state)))
     (hasheq 'name "Proceed"
             'id (label->id tag-state)
             'stateId (label->id tag-state)
             'goal (goal->json/canonical g)
             'sub (sub->json/canonical sub)
             'disequalities (dis->json/canonical dis)
             'trail (trail->json/canonical trail)
             'reified (reify/canonical (sub->reify/canonical sub)
                                       (dis->reify/canonical dis)
                                       (state-c-bound/canonical c)
                                       num-query-variables))]
    [`(,s_1 <-+ ,s_2)
     (hasheq 'name "<-+"
             'children (list (tree->json/canonical s_1 num-query-variables)
                             (tree->json/canonical s_2 num-query-variables)))]
    [`(,s_1 +-> ,s_2)
     (hasheq 'name "+->"
             'children (list (tree->json/canonical s_1 num-query-variables)
                             (tree->json/canonical s_2 num-query-variables)))]
    [`(,s_1 × ,g ,_c)
     (hasheq 'name "Conjunction"
             'children (list (tree->json/canonical s_1 num-query-variables)
                             (goal->json/canonical g)))]
    [`(delay ,s_1)
     (hasheq 'name "Delay"
             'children (list (tree->json/canonical s_1 num-query-variables)))]
    [`(⊤ ,σ)
     (state->answer-json/canonical σ num-query-variables)]
    [`((⊤ ,σ) + ,s_tail)
     (define tail-json (tree->json/canonical s_tail num-query-variables))
     (define tail-empty? (equal? (hash-ref tail-json 'name #f) "Empty"))
     (state->answer-json/canonical σ
                                   num-query-variables
                                   (and (not tail-empty?) tail-json))]
    [_ (hasheq 'name "Unknown")]))

(define (config->tree-json/canonical cfg num-query-variables)
  (tree->json/canonical
   (project-config-tree/canonical cfg)
   num-query-variables))

(define (to-json/canonical cfg num-query-variables)
  (jsexpr->string (config->tree-json/canonical cfg num-query-variables)))

(define (goal-query-vars/canonical g)
  (match g
    [`(∃ ,d ,_ ,_) (length d)]
    [`(sdelay ,g_1 ,_) (goal-query-vars/canonical g_1)]
    [`(,g_1 ∧ ,g_2 ,_) (max (goal-query-vars/canonical g_1)
                            (goal-query-vars/canonical g_2))]
    [`(,g_1 ∨ ,g_2 ,_) (max (goal-query-vars/canonical g_1)
                            (goal-query-vars/canonical g_2))]
    [_ 0]))

(define (num-query-vars/work s)
  (match s
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
    [`((⊤ ,_σ) + ,s_1)
     (num-query-vars/work s_1)]
    [`(delay ,s_1)
     (num-query-vars/work s_1)]
    [`(proceed (,g ,_σ))
     (goal-query-vars/canonical g)]
    [_ 0]))

(define (num-query-vars/canonical cfg)
  (match cfg
    [`(,_gamma ,s_work ,_as)
     (num-query-vars/work s_work)]
    [`(,_gamma ,s)
     (num-query-vars/work s)]
    [_ 0]))
