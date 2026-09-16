#lang racket

(require racket/hash
         (only-in "../derivations/shared/kernel.rkt" named-variable? owners-support))

;; Shared presentation of S goals, logical states, scope, and settled Frontiers.
;; These functions inspect existing data without executing a semantic operation.
(provide label->visible-id term->visible-json with-source-id
         state-fields state-node node goal->picture with-owners with-owner-annotations
         frontier-answer-nodes)

(define (label->visible-id tag)
  (match tag
    [`(label ,id) id]
    [_ (format "~a" tag)]))

(define (with-source-id picture tag)
  (define id (label->visible-id tag))
  (if (string-prefix? id "hidden:") picture (hash-set picture 'id id)))

(define (visible-name name)
  (regexp-replace #px"^[xr]:([^«]+).*" (symbol->string name) "\\1"))

(define (term->visible-json term)
  (match term
    ['empty '()]
    [(? named-variable? variable)
     (define suffix (substring (symbol->string variable) 2))
     (or (string->number suffix) (hasheq 'var (symbol->string variable)))]
    [(? symbol? variable) (hasheq 'var (visible-name variable))]
    [`(sym ,name) (hasheq 'sym name)]
    [`(nat ,number) (hasheq 'num number)]
    [`(str ,string) (hasheq 'str string)]
    [`(,left : ,right)
     (hasheq 'pair (list (term->visible-json left) (term->visible-json right)))]
    [(? boolean?) term]
    [_ (error 'term->visible-json "unknown source term: ~e" term)]))

;; Reification only walks the existing substitution. Sparse introductions
;; are names, not a numerical bound or a request to rerun a logic program.
(define (walk term substitution [seen '()])
  (match (and (named-variable? term) (assoc term substitution))
    [#f term]
    [(list variable value)
     (when (member variable seen)
       (error 'reify "cyclic substitution at ~e" variable))
     (walk value substitution (cons variable seen))]))

(define (reify-term term substitution names)
  (match (walk term substitution)
    [(? named-variable? variable)
     (match (assoc variable names)
       [(cons _ name) (values name names)]
       [#f
        (define name (format "_.~a" (length names)))
        (values name (append names (list (cons variable name))))])]
    [`(,left : ,right)
     (define-values (left* names*) (reify-term left substitution names))
     (define-values (right* names**) (reify-term right substitution names*))
     (values (hasheq 'pair (list left* right*)) names**)]
    [value (values (term->visible-json value) names)]))

(define (reify-query variables substitution)
  (define-values (reversed _names)
    (for/fold ([reversed '()] [names '()]) ([variable (in-list variables)])
      (define-values (value names*) (reify-term variable substitution names))
      (values (cons value reversed) names*)))
  (match (reverse reversed)
    [(list value) value]
    [values values]))

(define (state-fields state introductions query-variables)
  (match state
    [`(state ,substitution ,disequalities ,trail ,tag)
     (hasheq
      'stateId (label->visible-id tag)
      ;; State's semantic tag need not distinguish different substitutions.
      ;; This exact structural key is presentation identity only, not a new
      ;; field in the source state or an allocation/event observer.
      'stateKey (format "~s" (list introductions state))
      'scope (map term->visible-json introductions)
      'sub (for/list ([(variable values) (in-dict substitution)])
             (match-define (list value) values)
             (hasheq 'key (term->visible-json variable)
                     'value (term->visible-json value)))
      'disequalities
      (for/list ([constraint (in-list disequalities)])
        (match-define (list left right) constraint)
        (hasheq 'left (term->visible-json left) 'right (term->visible-json right)))
      'trail
      (for/list ([crumb (in-list trail)])
        (match-define `(,left =? ,right ,source) crumb)
        (hasheq 'left (term->visible-json left) 'right (term->visible-json right)
                'id (label->visible-id source)))
      'reified (if (andmap (lambda (variable) (member variable introductions))
                          query-variables)
                   (reify-query query-variables substitution)
                   '()))]
    [_ (error 'state-fields "expected an S logical state: ~e" state)]))

(define (state-node state introductions query-variables committed?)
  (hash-union
   (hasheq 'name (if committed? "Answer" "Candidate")
           'renderRole (if committed? "answer-node" "candidate")
           'semanticKind (if committed? "answer" "search")
           'nodeColor (if committed? "green" "#fff2cc"))
   (state-fields state introductions query-variables)))

(define (node name role children [active #f] [color #f])
  (define result (hasheq 'name name 'renderRole role 'children children))
  (define focused (if color (hash-set result 'focusColor color) result))
  (if active (hash-set focused 'activeChildIndex active) focused))

(define (goal->picture goal)
  (define picture
    (match goal
    [`(,(and name (or 'succeed 'fail)) ,tag)
     (with-source-id (hasheq 'name (if (eq? name 'succeed) "Succeed" "Fail")
                              'renderRole "goal-leaf") tag)]
    [`(,left ,(and operator (or '=? '!=)) ,right ,tag)
     (with-source-id
      (hasheq 'name (if (eq? operator '=?) "Unify" "Disequality")
              'renderRole "goal-leaf"
              'left (term->visible-json left) 'right (term->visible-json right)) tag)]
    [`(,left ,(and operator (or '∧ '∨)) ,right ,tag)
     (with-source-id (node (if (eq? operator '∧) "Goal-Conj" "Goal-Disj")
                           "goal-branch" (list (goal->picture left) (goal->picture right))) tag)]
    [`(suspend ,body ,tag)
     (with-source-id (node "Goal-Delay" "goal-delay" (list (goal->picture body))) tag)]
    [`(∃ ,variables ,body ,tag)
     (with-source-id (hash-set (node "Fresh" "goal-fresh" (list (goal->picture body)))
                               'vars (map term->visible-json variables)) tag)]
    [`(,(? symbol? relation) ,arguments ... ,tag)
     #:when (string-prefix? (symbol->string relation) "r:")
     (with-source-id (hasheq 'name "Rel-Call" 'renderRole "goal-leaf"
                             'rel (visible-name relation)
                             'args (map term->visible-json arguments)) tag)]
    [_ (error 'goal->picture "unknown source goal: ~e" goal)]))
  (hash-set picture 'semanticKind "goal"))

;; An Owner belongs to the source node that carries it. Keep the groups on
;; that picture node while threading their names into its descendants' scope.
;; Empty groups and hidden source identities still carry source information;
;; neither is an instruction to synthesize a wrapper or a selectable source id.
(define (with-owner-annotations owners introductions render)
  (match-define `(Owners ,groups ...) owners)
  (hash-set
   (render (owners-support owners introductions))
   'owners
   (for/list ([group (in-list groups)])
     (match-define `(Owner ,introduced ,tag) group)
     (hasheq 'vars (map term->visible-json introduced)
             'sourceId (label->visible-id tag)))))

;; The earlier dormant work-tree inspection keeps its historical wrapper view.
;; The current strict source uses with-owner-annotations instead.
(define (with-owners owners introductions render)
  (match owners
    ['(Owners) (render introductions)]
    [`(Owners (Owner ,introduced ,tag) ,rest ...)
     (with-source-id
      (hash-set
       (node "Freshened" "freshened"
             (list (with-owners `(Owners ,@rest) (append introductions introduced) render)) 0)
       'vars (map term->visible-json introduced)) tag)]))

(define (frontier-answer-nodes frontier query-variables)
  (define (walk-frontier frontier introductions)
    (match frontier
      [`(Emit ,owners (Answer ,private ,state) ,tail)
       (define here (owners-support owners introductions))
       (cons (with-owner-annotations private here
               (lambda (scope) (state-node state scope query-variables #t)))
             (walk-frontier tail here))]
      [`(Last ,owners (Answer ,private ,state))
       (list (with-owner-annotations private (owners-support owners introductions)
               (lambda (scope) (state-node state scope query-variables #t))))]
      [`(Forced ,owners ,tail) (walk-frontier tail (owners-support owners introductions))]
      [`(,(or 'advance 'collect) ,frontier) (walk-frontier frontier introductions)]
      [_ '()]))
  (walk-frontier frontier '()))
