#lang racket

(require redex/reduction-semantics "grammar-s.rkt" "grammar-e.rkt" "grammar-n.rkt"
         "relation-grammar.rkt"
         "kernel.rkt" "maps.rkt"
         (prefix-in s: "core/s/language.rkt")
         (prefix-in e: "core/e/language.rkt")
         (prefix-in n: "core/n/language.rkt"))

(provide wf-s? wf-e? wf-n? wf-s-rel? wf-e-rel? wf-n-rel?)

(define (term-valid? value variable? allocated? [lexical '()])
  (match value
    [(? lexical-variable? x) (and (member x lexical) #t)]
    [(? variable? variable) (allocated? variable)]
    [`(,left : ,right)
     (and (term-valid? left variable? allocated? lexical)
          (term-valid? right variable? allocated? lexical))]
    [_ #t]))

(define (goal-valid? goal variable? allocated? [lexical '()])
  (match goal
    [`(,(or 'succeed 'fail) ,_) #t]
    [`(,left ,(or '=? '!=) ,right ,_)
     (and (term-valid? left variable? allocated? lexical)
          (term-valid? right variable? allocated? lexical))]
    [`(,left ,(or '∧ '∨) ,right ,_)
     (and (goal-valid? left variable? allocated? lexical)
          (goal-valid? right variable? allocated? lexical))]
    [`(∃ ,binders ,body ,_)
     (and (= (length binders) (length (remove-duplicates binders)))
          (goal-valid? body variable? allocated? (append binders lexical)))]
    [`(suspend ,body ,_) (goal-valid? body variable? allocated? lexical)]
    [`(,(? relation-name?) ,operands ... ,_)
     (andmap (lambda (value) (term-valid? value variable? allocated? lexical)) operands)]
    [_ #f]))

(define (term-dependencies value variable?)
  (match value
    [(? variable? variable) (list variable)]
    [`(,left : ,right) (append (term-dependencies left variable?)
                              (term-dependencies right variable?))]
    [_ '()]))

(define (acyclic-from? variable substitution variable? [path '()])
  (and (not (member variable path))
       (match (assoc variable substitution)
         [#f #t]
         [(list _ value)
          (for/and ([dependency (in-list (term-dependencies value variable?))])
            (acyclic-from? dependency substitution variable? (cons variable path)))])))

;; Preserve the selected oracle's full trail contract: replay from the empty
;; substitution with this row's own walk/unification kernel and obtain the
;; stored substitution exactly. Merely scoped or extensionally equivalent
;; trails are insufficient; order and the retained alias structure matter.
(define-syntax-rule (define-trail-replay name walk unify)
  (define (name trail expected [substitution '()])
    (match trail
      ['() (equal? substitution expected)]
      [(cons `(,left =? ,right ,_) rest)
       (define next
         (term (unify (walk ,left ,substitution)
                      (walk ,right ,substitution) ,substitution)))
       (and next (name rest expected next))]
      [_ #f])))

(define-trail-replay trail-replays?/s s:walk/s s:unify/s)
(define-trail-replay trail-replays?/e e:walk/e e:unify/e)
(define-trail-replay trail-replays?/n n:walk/n n:unify/n)

(define (store-valid? sub dis trail variable? allocated? trail-replays?)
  (and (= (length sub) (length (remove-duplicates (map first sub))))
       (for/and ([entry (in-list sub)])
         (match-define (list variable value) entry)
         (and (variable? variable) (allocated? variable)
              (term-valid? value variable? allocated?)
              (acyclic-from? variable sub variable?)))
       (for/and ([constraint (in-list dis)])
         (andmap (lambda (value) (term-valid? value variable? allocated?)) constraint))
       (for/and ([goal (in-list trail)]) (goal-valid? goal variable? allocated?))
       (trail-replays? trail sub)))

(define (named-allocated support)
  (lambda (variable) (and (member variable support) #t)))

(define (state-s-valid? state support)
  (match state
    [`(state ,sub ,dis ,trail ,_)
     (store-valid? sub dis trail named-variable? (named-allocated support) trail-replays?/s)]
    [_ #f]))

(define (extend-valid-owners owners prefix)
  (define support (owners-support owners prefix))
  (and (valid-support? support) support))

(define (s-valid? computation prefix)
  (match computation
    [`(eval ,owners ,goal ,state)
     (define here (extend-valid-owners owners prefix))
     (and here (state-s-valid? state here)
          (goal-valid? goal named-variable? (named-allocated here)))]
    [`(mplus ,owners ,left ,right)
     (define here (extend-valid-owners owners prefix))
     (and here (s-valid? left here) (s-valid? right here))]
    [`(bind ,owners ,search ,goal)
     (define here (extend-valid-owners owners prefix))
     (and here (s-valid? search here)
          (goal-valid? goal named-variable? (named-allocated here)))]
    [`(,(or 'Empty 'Done) ,owners) (and (extend-valid-owners owners prefix) #t)]
    [`(,(or 'One 'Solo) ,owners ,state)
     (define here (extend-valid-owners owners prefix))
     (and here (state-s-valid? state here))]
    [`(,(or 'Yield 'Emit) ,owners (Answer ,answer-owners ,state) ,tail)
     (define here (extend-valid-owners owners prefix))
     (define answer (and here (extend-valid-owners answer-owners here)))
     (and answer (state-s-valid? state answer) (s-valid? tail here))]
    [`(,(or 'Delay 'Forced) ,owners ,body)
     (define here (extend-valid-owners owners prefix))
     (and here (s-valid? body here))]
    [`(,(or 'force 'render 'commit 'advance 'collect 'More) ,body)
     (s-valid? body prefix)]
    [_ #f]))

(define (wf-s? computation)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (redex-match? StrictS q computation) (s-valid? computation '()))))

(define (state-e-valid? state)
  (match state
    [`(state (Support ,support ...) ,sub ,dis ,trail ,_)
     (and (valid-support? support)
          (store-valid? sub dis trail named-variable? (named-allocated support) trail-replays?/e))]
    [_ #f]))

(define (e-valid? computation)
  (match computation
    [`(eval ,goal ,state)
     (and (state-e-valid? state)
          (goal-valid? goal named-variable? (named-allocated (state-support state))))]
    [`(mplus ,left ,right) (and (e-valid? left) (e-valid? right))]
    [`(bind ,search ,goal)
     (and (e-valid? search)
          (goal-valid? goal named-variable? (named-allocated (common-support search))))]
    [`(,(or 'Empty 'Done) (Support ,support ...)) (valid-support? support)]
    [`(,(or 'One 'Solo) ,state) (state-e-valid? state)]
    [`(,(or 'Yield 'Emit) ,state ,tail) (and (state-e-valid? state) (e-valid? tail))]
    [`(,(or 'Delay 'force 'render 'commit 'advance 'collect 'Forced 'More) ,body)
     (e-valid? body)]
    [_ #f]))

(define (wf-e? computation)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (redex-match? StrictE q computation) (e-valid? computation))))

(define (state-n-valid? state)
  (match state
    [`(state ,next ,sub ,dis ,trail ,_)
     (and (exact-nonnegative-integer? next)
          (store-valid? sub dis trail exact-nonnegative-integer?
                        (lambda (variable) (< variable next)) trail-replays?/n))]
    [_ #f]))

(define (world-nexts computation)
  (match computation
    [`(eval ,_ (state ,next ,_ ,_ ,_ ,_)) (list next)]
    [`(mplus ,left ,right) (append (world-nexts left) (world-nexts right))]
    [`(bind ,search ,_) (world-nexts search)]
    [`(,(or 'Empty 'Done) ,next) (list next)]
    [`(,(or 'One 'Solo) (state ,next ,_ ,_ ,_ ,_)) (list next)]
    [`(,(or 'Yield 'Emit) (state ,next ,_ ,_ ,_ ,_) ,tail) (cons next (world-nexts tail))]
    [`(,(or 'Delay 'force 'render 'commit 'advance 'collect 'Forced 'More) ,body)
     (world-nexts body)]))

(define (n-valid? computation)
  (match computation
    [`(eval ,goal ,(and state `(state ,next ,_ ,_ ,_ ,_)))
     (and (state-n-valid? state)
          (goal-valid? goal exact-nonnegative-integer? (lambda (v) (< v next))))]
    [`(mplus ,left ,right) (and (n-valid? left) (n-valid? right))]
    [`(bind ,search ,goal)
     (define next (apply min (world-nexts search)))
     (and (n-valid? search)
          (goal-valid? goal exact-nonnegative-integer? (lambda (v) (< v next))))]
    [`(,(or 'Empty 'Done) ,next) (exact-nonnegative-integer? next)]
    [`(,(or 'One 'Solo) ,state) (state-n-valid? state)]
    [`(,(or 'Yield 'Emit) ,state ,tail) (and (state-n-valid? state) (n-valid? tail))]
    [`(,(or 'Delay 'force 'render 'commit 'advance 'collect 'Forced 'More) ,body)
     (n-valid? body)]
    [_ #f]))

(define (wf-n? computation)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (redex-match? StrictN q computation) (n-valid? computation))))

;; Definition bodies may reference their lexical formals and fresh binders,
;; never allocation belonging to a particular query world. Calls are checked
;; against the complete environment, so mutual recursion is permitted.
(define (calls-valid? datum definitions)
  (match datum
    [`(program ,_ ,_) #f]
    [`(,(? relation-name? name) ,arguments ... ,_)
     (match (assoc name definitions)
       [(list _ formals _) (= (length arguments) (length formals))]
       [#f #f])]
    [(cons first rest) (and (calls-valid? first definitions) (calls-valid? rest definitions))]
    [_ #t]))

(define (definitions-valid? definitions variable?)
  (and (= (length definitions) (length (remove-duplicates (map first definitions))))
       (for/and ([definition (in-list definitions)])
         (match-define (list _ formals body) definition)
         (and (= (length formals) (length (remove-duplicates formals)))
              (goal-valid? body variable? (lambda (_) #f) formals)
              (calls-valid? body definitions)))))

(define (wf-s-rel? computation)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (redex-match? StrictSRel p computation)
         (match computation
           [`(program ,definitions ,body)
            (and (definitions-valid? definitions named-variable?)
                 (s-valid? body '()) (calls-valid? body definitions))]))))

(define (wf-e-rel? computation)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (redex-match? StrictERel p computation)
         (match computation
           [`(program ,definitions ,body)
            (and (definitions-valid? definitions named-variable?)
                 (e-valid? body) (calls-valid? body definitions))]))))

(define (wf-n-rel? computation)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (and (redex-match? StrictNRel p computation)
         (match computation
           [`(program ,definitions ,body)
            (and (definitions-valid? definitions exact-nonnegative-integer?)
                 (n-valid? body) (calls-valid? body definitions))]))))
