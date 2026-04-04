#lang racket

(require racket/list
         redex/reduction-semantics
         "../corpus.rkt"
         "../shared/kernel.rkt")

(provide premachine-lang
         parse-example
         decompose
         plug
         contract
         step
         trace
         final?
         answers
         cfg-term
         cfg-query
         cfg-root-scope
         cfg-obs)

(define-language premachine-lang
  [cfg (config (u ...) (u ...) pm (σ ...))]
  [pm (g σ)
      (ans σ)
      empty
      (scope (u ...) pm tag)
      (delay pm)
      (join pm g)
      (merge turn pm pm)]
  [turn left right]
  [σ (state sub dis trail tag)]
  [sub ((u_!_ t) ...)]
  [dis ((t t) ...)]
  [trail ((t =? t tag) ...)]
  [g (succeed tag)
     (fail tag)
     (t =? t tag)
     (t != t tag)
     (∃ (x_!_ ...) g tag)
     (g ∧ g tag)
     (g ∨ g tag)
     (suspend g tag)]
  [t x
     u
     pt
     (t : t)]
  [pt (sym string)
      (nat number)
      boolean
      (str string)
      empty]
  [x (variable-prefix x:)]
  [u (variable-prefix u:)]
  [tag (label string)]
  [frame (scope-frame (u ...) tag)
         (join-frame g)
         (merge-left-frame turn pm)
         (merge-right-frame turn pm)]
  [ctx (frame ...)])

(define (cfg-query cfg)
  (match cfg
    [`(config ,query-u* ,_root-scope ,_term ,_obs)
     query-u*]
    [_ (error 'cfg-query "unsupported config: ~e" cfg)]))

(define (cfg-root-scope cfg)
  (match cfg
    [`(config ,_query-u* ,root-scope ,_term ,_obs)
     root-scope]
    [_ (error 'cfg-root-scope "unsupported config: ~e" cfg)]))

(define (cfg-term cfg)
  (match cfg
    [`(config ,_query-u* ,_root-scope ,term ,_obs)
     term]
    [_ (error 'cfg-term "unsupported config: ~e" cfg)]))

(define (cfg-obs cfg)
  (match cfg
    [`(config ,_query-u* ,_root-scope ,_term ,obs)
     obs]
    [_ (error 'cfg-obs "unsupported config: ~e" cfg)]))

(define (make-state)
  '(state () () () (label "s")))

(define (ambient-scope ctx root-scope [acc root-scope])
  (match ctx
    ['() acc]
    [(cons `(scope-frame ,intro ,_tag) rest)
     (ambient-scope rest root-scope (scope-append intro acc))]
    [(cons _ rest)
     (ambient-scope rest root-scope acc)]))

(define (pm-final? term)
  (match term
    [`(ans ,_state)
     #t]
    ['empty
     #t]
    [`(scope ,_intro ,inner ,_tag)
     (pm-final? inner)]
    [`(delay ,inner)
     (pm-final? inner)]
    [`(join ,_left ,_g)
     #f]
    [`(merge ,_turn ,left ,right)
     (and (pm-final? left)
          (pm-final? right))]
    [`((succeed ,_tag) ,_state)
     #f]
    [`((fail ,_tag) ,_state)
     #f]
    [`((,t1 =? ,t2 ,tag) ,_state)
     #f]
    [`((,t1 != ,t2 ,tag) ,_state)
     #f]
    [`((∃ ,d ,g ,tag) ,_state)
     #f]
    [`((,g1 ∧ ,g2 ,tag) ,_state)
     #f]
    [`((,g1 ∨ ,g2 ,tag) ,_state)
     #f]
    [`((suspend ,g ,tag) ,_state)
     #f]
    [_ #f]))

(define (answer-term? term)
  (match term
    [`(ans ,_) #t]
    [_ #f]))

(define (delay-term? term)
  (match term
    [`(delay ,_) #t]
    [_ #f]))

(define (goal-state-term? term)
  (match term
    [`((succeed ,_tag) ,_state) #t]
    [`((fail ,_tag) ,_state) #t]
    [`((,t1 =? ,t2 ,tag) ,_state) #t]
    [`((,t1 != ,t2 ,tag) ,_state) #t]
    [`((∃ ,d ,g ,tag) ,_state) #t]
    [`((,g1 ∧ ,g2 ,tag) ,_state) #t]
    [`((,g1 ∨ ,g2 ,tag) ,_state) #t]
    [`((suspend ,g ,tag) ,_state) #t]
    [_ #f]))

(define (join-frontier? term)
  (match term
    [`(join (ans ,_) ,_) #t]
    [`(join empty ,_) #t]
    [`(join (delay ,_) ,_) #t]
    [`(join (scope ,_ ,_ ,_) ,_) #t]
    [`(join (merge ,_ ,_ ,_) ,_) #t]
    [_ #f]))

(define (merge-frontier? term)
  (match term
    [`(merge left (ans ,_) ,_) #t]
    [`(merge left empty ,_) #t]
    [`(merge left (delay ,_) ,_) #t]
    [`(merge right ,_ (ans ,_)) #t]
    [`(merge right ,_ empty) #t]
    [`(merge right ,_ (delay ,_)) #t]
    [_ #f]))

(define (contractible-focus? term)
  (or (equal? term 'empty)
      (answer-term? term)
      (delay-term? term)
      (goal-state-term? term)
      (join-frontier? term)
      (merge-frontier? term)))

(define (decompose term [ctx '()])
  (match term
    [_ #:when (contractible-focus? term)
     (values term ctx)]
    [`(scope ,intro ,inner ,tag)
     (decompose inner (cons `(scope-frame ,intro ,tag) ctx))]
    [`(join ,left ,g)
     (decompose left (cons `(join-frame ,g) ctx))]
    [`(merge left ,left-term ,right-term)
     (decompose left-term (cons `(merge-left-frame left ,right-term) ctx))]
    [`(merge right ,left-term ,right-term)
     (decompose right-term (cons `(merge-right-frame right ,left-term) ctx))]
    [_ (values #f #f)]))

(define (plug term ctx)
  (match ctx
    ['() term]
    [(cons `(scope-frame ,intro ,tag) rest)
     (plug `(scope ,intro ,term ,tag) rest)]
    [(cons `(join-frame ,g) rest)
     (plug `(join ,term ,g) rest)]
    [(cons `(merge-left-frame ,turn ,right-term) rest)
     (plug `(merge ,turn ,term ,right-term) rest)]
    [(cons `(merge-right-frame ,turn ,left-term) rest)
     (plug `(merge ,turn ,left-term ,term) rest)]))

(define (contract-local focus ctx root-scope)
  (match focus
    [`((succeed ,_tag) ,state)
     (list "premachine/succeed" `(ans ,state) '())]
    [`((fail ,_tag) ,_state)
     (list "premachine/fail" 'empty '())]
    [`((,t1 =? ,t2 ,tag) (state ,sub ,dis ,trail ,state-tag))
     (match (unify t1 t2 sub)
       [#f
        (list "premachine/unify-fail" 'empty '())]
       [sub^
        (if (invalid? sub^ dis)
            (list "premachine/unify-violates-disequality" 'empty '())
            (list "premachine/unify-success"
                  `(ans (state ,sub^ ,dis ,(append trail (list `(,t1 =? ,t2 ,tag))) ,state-tag))
                  '()))])]
    [`((,t1 != ,t2 ,_tag) (state ,sub ,dis ,trail ,state-tag))
     (define dis^
       (cons (list t1 t2) dis))
     (if (invalid? sub dis^)
         (list "premachine/disequality-fail" 'empty '())
         (list "premachine/disequality-success"
               `(ans (state ,sub ,dis^ ,trail ,state-tag))
               '()))]
    [`((∃ ,d ,g ,tag) (state ,sub ,dis ,trail ,state-tag))
     (define intro (fresh-u-list (ambient-scope ctx root-scope) d))
     (define subs (map list d intro))
     (list "premachine/fresh-substitute"
           `(scope ,intro
                   (,(subst-goal g subs)
                    (state ,sub ,dis ,trail ,state-tag))
                   ,tag)
           '())]
    [`((,g1 ∧ ,g2 ,_tag) ,state)
     (list "premachine/conj-push-context"
           `(join (,g1 ,state) ,g2)
           '())]
    [`((,g1 ∨ ,g2 ,_tag) ,state)
     (list "premachine/disj-build-merge"
           `(merge left (,g1 ,state) (,g2 ,state))
           '())]
    [`((suspend ,g ,_tag) ,state)
     (list "premachine/suspend-goal"
           `(delay (,g ,state))
           '())]
    [_ #f]))

(define (contract focus ctx root-scope)
  (or (contract-local focus ctx root-scope)
      (match focus
        [`(delay ,inner)
         (list "premachine/force-delay" inner '())]
        [`(join (ans ,state) ,g)
         (list "premachine/conj-bring-answer" `(,g ,state) '())]
        [`(join empty ,_g)
         (list "premachine/conj-preserve-fail" 'empty '())]
        [`(join (scope ,intro ,inner ,tag) ,g)
         (list "premachine/carry-scope-through-conj" `(scope ,intro (join ,inner ,g) ,tag) '())]
        [`(join (delay ,inner) ,g)
         (list "premachine/carry-delay-through-conj" `(delay (join ,inner ,g)) '())]
        [`(join (merge ,turn ,left ,right) ,g)
         (list "premachine/distribute-conj-over-merge" `(merge ,turn (join ,left ,g) (join ,right ,g)) '())]
        [`(merge left (ans ,state) ,right)
         (list "premachine/commit-left-answer" right (list state))]
        [`(merge left empty ,right)
         (list "premachine/drop-left-fail" right '())]
        [`(merge left (delay ,inner) ,right)
         (list "premachine/enter-right" `(delay (merge right ,inner ,right)) '())]
        [`(merge right ,left (ans ,state))
         (list "premachine/commit-right-answer" left (list state))]
        [`(merge right ,left empty)
         (list "premachine/drop-right-fail" left '())]
        [`(merge right ,left (delay ,inner))
         (list "premachine/return-left" `(delay (merge left ,left ,inner)) '())]
        [_ #f])))

(define (step-term term root-scope)
  (define-values (focus ctx) (decompose term))
  (and focus
       (match (contract focus ctx root-scope)
         [#f #f]
         [(list name next-focus emitted)
          (list name (plug next-focus ctx) emitted)])))

(define (parse-example label)
  (define-values (query-u* goal) (instantiate-program label))
  `(config ,query-u* ,query-u* (,goal ,(make-state)) ()))

(define (step cfg)
  (match cfg
    [`(config ,query-u* ,root-scope ,term ,obs)
     (match (step-term term root-scope)
       [#f '()]
       [(list name next-term emitted)
        (list (list name
                    `(config ,query-u*
                             ,root-scope
                             ,next-term
                             ,(append obs emitted))))])]
    [_ (error 'step "unsupported config: ~e" cfg)]))

(define (trace cfg [limit 128] [steps '()])
  (match (step cfg)
    ['()
     (values (reverse steps)
             cfg
             (if (final? cfg) 'value 'stuck))]
    [_ #:when (zero? limit)
       (values (reverse steps) cfg 'cap)]
    [(list (list name next))
     (trace next (sub1 limit) (cons name steps))]
    [_ (values (reverse steps) cfg 'nondeterministic)]))

(define (final? cfg)
  (match cfg
    [`(config ,_query-u* ,_root-scope ,term ,_obs)
     (pm-final? term)]
    [_ (error 'final? "unsupported config: ~e" cfg)]))

(define (answer-states term [acc '()])
  (match term
    [`(ans ,state) (cons state acc)]
    [`(scope ,_intro ,inner ,_tag) (answer-states inner acc)]
    [`(delay ,inner) (answer-states inner acc)]
    [`(join ,left ,_g) (answer-states left acc)]
    [`(merge ,_turn ,left ,right) (answer-states left (answer-states right acc))]
    [_ acc]))

(define (answers cfg)
  (match cfg
    [`(config ,query-u* ,_root-scope ,term ,obs)
     (remove-duplicates
      (for/list ([state (in-list (append obs (reverse (answer-states term))))])
        (match-define `(state ,sub ,_dis ,_trail ,_tag) state)
        (reify-query-values query-u* sub)))]
    [_ (error 'answers "unsupported config: ~e" cfg)]))
