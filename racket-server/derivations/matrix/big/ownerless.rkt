#lang racket
(require redex/reduction-semantics "feature-schema.rkt"
         (only-in "../../shared/kernel.rkt" Failure Success relation-name? instantiate-relation)
         (for-syntax racket/base racket/syntax syntax/parse))
(provide define-ownerless-big)
;; Compile-time specialization: each row retains its own grammar, state,
;; kernel and allocator. No state or control is decoded into another row.
(define-syntax (define-ownerless-big stx)
 (syntax-parse stx
  [(_ coordinate:id language:id parent:id value?:id observation?:id atomic:id allocate:id feature:id
      (~optional (~seq #:relations environment:id) #:defaults ([environment #'#f])))
   #:with (env ...) (if (syntax-e #'environment) #'(environment) #'())
   #:with (env-mode ...) (if (syntax-e #'environment) #'(I) #'())
   #:with relation-value (if (syntax-e #'environment) #'environment #''())
   #:with relation-term (if (syntax-e #'environment) #'(term environment) #''())
   #:with search-big (format-id #'coordinate "search-big/~a" #'coordinate)
   #:with merge-big (format-id #'coordinate "merge-big/~a" #'coordinate)
   #:with bind-big (format-id #'coordinate "bind-big/~a" #'coordinate)
   #:with render-big (format-id #'coordinate "render-big/~a" #'coordinate)
   #:with commit-big (format-id #'coordinate "commit-big/~a" #'coordinate)
   #:with advance-big (format-id #'coordinate "advance-big/~a" #'coordinate)
   #:with collect-big (format-id #'coordinate "collect-big/~a" #'coordinate)
   #:with observe-big (format-id #'coordinate "observe-big/~a" #'coordinate)
   #:with evaluate (format-id #'coordinate "evaluate/~a" #'coordinate)
   #:with promote-search (format-id #'coordinate "promote-search/~a" #'coordinate)
   #:with promote-merge (format-id #'coordinate "promote-merge/~a" #'coordinate)
   #:with promote-bind (format-id #'coordinate "promote-bind/~a" #'coordinate)
   #:with promote-render (format-id #'coordinate "promote-render/~a" #'coordinate)
   #:with promote-commit (format-id #'coordinate "promote-commit/~a" #'coordinate)
   #:with promote-advance (format-id #'coordinate "promote-advance/~a" #'coordinate)
   #:with promote-collect (format-id #'coordinate "promote-collect/~a" #'coordinate)
   #:with promote-observe (format-id #'coordinate "promote-observe/~a" #'coordinate)
   #:with promote (format-id #'coordinate "promote/~a" #'coordinate)
   #:with raw-derivations (format-id #'coordinate "raw-derivations/~a" #'coordinate)
   #'(feature-specialize feature
    (provide language search-big merge-big bind-big render-big commit-big advance-big collect-big observe-big
             evaluate promote raw-derivations)
    (define-extended-language language parent [label string] [trace (label (... ...))])
    (define-metafunction language traces : trace (... ...) -> trace
      [(traces (label (... ...)) (... ...)) (label (... ...) (... ...))])
    (define (atomic-result goal state)
      (match (atomic goal state)
        [(Failure) `(Empty ,(second state))]
        [(Success next) `(One ,next)]))
(define-judgment-form language
  #:mode (search-big env-mode ... I O O)
  #:contract (search-big env ... c SV trace)

  [----------------------------------------------- "value"
   (search-big env ... SV SV ())]

  [(where SV ,(atomic-result (term a) (term σ)))
   ----------------------------------------------- "eval atom"
   (search-big env ... (eval a σ) SV ("eval-atom"))]

  [(where g ,(instantiate-relation relation-term (term call)))
   (search-big env ... (eval g σ) SV trace)
   ----------------------------------------------- "relation call"
   (search-big env ... (eval call σ) SV (traces ("eval-call") trace))]

  [(where c ,(allocate (term (x (... ...))) (term g) (term σ)))
   (search-big env ... c SV trace)
   ----------------------------------------------- "eval fresh"
   (search-big env ... (eval (∃ (x (... ...)) g tag) σ) SV
               (traces ("allocate-fresh") trace))]

  [(search-big env ... (eval g_1 σ) SV_1 trace_1)
   (search-big env ... (eval g_2 σ) SV_2 trace_2)
   (merge-big env ... SV_1 SV_2 SV trace_merge)
   ----------------------------------------------- "eval disjunction"
   (search-big env ... (eval (g_1 ∨ g_2 tag) σ) SV
               (traces ("eval-disj") trace_1 trace_2 trace_merge))]

  [(search-big env ... (eval g_1 σ) SV_1 trace_1)
   (bind-big env ... SV_1 g_2 SV trace_bind)
   ----------------------------------------------- "eval conjunction"
   (search-big env ... (eval (g_1 ∧ g_2 tag) σ) SV
               (traces ("eval-conj") trace_1 trace_bind))]

  [----------------------------------------------- "eval suspension"
   (search-big env ... (eval (suspend g tag) σ) (Delay (eval g σ))
               ("eval-suspend"))]

  [(search-big env ... c_1 SV_1 trace_1)
   (search-big env ... c_2 SV_2 trace_2)
   (merge-big env ... SV_1 SV_2 SV trace_merge)
   ----------------------------------------------- "strict merge operands"
   (search-big env ... (mplus c_1 c_2) SV
               (traces trace_1 trace_2 trace_merge))]

  [(search-big env ... c SV_1 trace_1)
   (bind-big env ... SV_1 g SV trace_bind)
   ----------------------------------------------- "strict bind operand"
   (search-big env ... (bind c g) SV (traces trace_1 trace_bind))]

  [(side-condition ,(not (value? (term c))))
   (search-big env ... c SV trace)
   ----------------------------------------------- "eager Yield tail"
   (search-big env ... (Yield σ c) (Yield σ SV) trace)]

  [(search-big env ... c_1 (Delay c_2) trace_1)
   (search-big env ... c_2 SV trace_2)
   ----------------------------------------------- "force suspension"
   (search-big env ... (force c_1) SV
               (traces trace_1 ("force-delay") trace_2))])

(define-judgment-form language
  #:mode (merge-big env-mode ... I I O O)
  #:contract (merge-big env ... SV SV SV trace)

  [----------------------------------------------- "merge empty"
   (merge-big env ... (Empty supply) SV SV ("mplus-empty"))]

  [----------------------------------------------- "merge one"
   (merge-big env ... (One σ) SV (Yield σ SV) ("mplus-one"))]

  [(merge-big env ... SV_1 SV_2 SV trace)
   ----------------------------------------------- "merge eager tail"
   (merge-big env ... (Yield σ SV_1) SV_2 (Yield σ SV)
              (traces ("mplus-yield") trace))]

  [----------------------------------------------- "merge suspension"
   (merge-big env ... (Delay c) SV
              (Delay (mplus SV (force (Delay c))))
              ("mplus-delay"))])

(define-judgment-form language
  #:mode (bind-big env-mode ... I I O O)
  #:contract (bind-big env ... SV g SV trace)

  [----------------------------------------------- "bind empty"
   (bind-big env ... (Empty supply) g (Empty supply) ("bind-empty"))]

  [(search-big env ... (eval g σ) SV trace)
   ----------------------------------------------- "bind one"
   (bind-big env ... (One σ) g SV (traces ("bind-one") trace))]

  [(search-big env ... (eval g σ) SV_1 trace_1)
   (bind-big env ... SV_tail g SV_2 trace_2)
   (merge-big env ... SV_1 SV_2 SV trace_merge)
   ----------------------------------------------- "bind eager residual"
   (bind-big env ... (Yield σ SV_tail) g SV
             (traces ("bind-yield") trace_1 trace_2 trace_merge))]

  [----------------------------------------------- "bind suspension"
   (bind-big env ... (Delay c) g
             (Delay (bind c g))
             ("bind-delay"))])

(define-judgment-form language
  #:mode (render-big env-mode ... I O O)
  #:contract (render-big env ... SV O trace)

  [----------------------------------------------- "render empty"
   (render-big env ... (Empty supply) (Done supply) ("render-empty"))]

  [----------------------------------------------- "render one"
   (render-big env ... (One σ) (Solo σ) ("render-one"))]

  [(render-big env ... SV O trace)
   ----------------------------------------------- "render Yield"
   (render-big env ... (Yield σ SV) (Emit σ O)
               (traces ("render-yield") trace))]

  [(search-big env ... c SV trace_search)
   (render-big env ... SV O trace_render)
   ----------------------------------------------- "render suspension"
   (render-big env ... (Delay c) (Forced O)
               (traces ("render-delay") trace_search trace_render))])

(define-judgment-form language
  #:mode (commit-big env-mode ... I O O)
  #:contract (commit-big env ... SV F trace)
  [----------------------------------------------- "commit empty"
   (commit-big env ... (Empty supply) (Done supply) ("commit-empty"))]
  [----------------------------------------------- "commit one"
   (commit-big env ... (One σ) (Solo σ) ("commit-one"))]
  [(commit-big env ... SV F trace)
   ----------------------------------------------- "commit eager tail"
   (commit-big env ... (Yield σ SV) (Emit σ F) (traces ("commit-yield") trace))]
  [----------------------------------------------- "commit suspended tip"
   (commit-big env ... (Delay c) (More (Delay c)) ("commit-delay"))])

(define-judgment-form language
  #:mode (advance-big env-mode ... I O O)
  #:contract (advance-big env ... F F trace)
  [----------------------------------------------- "advance done"
   (advance-big env ... (Done supply) (Done supply) ("advance-done"))]
  [----------------------------------------------- "advance solo"
   (advance-big env ... (Solo σ) (Solo σ) ("advance-solo"))]
  [(advance-big env ... F_1 F_2 trace)
   ----------------------------------------------- "advance Emit"
   (advance-big env ... (Emit σ F_1) (Emit σ F_2) (traces ("advance-emit") trace))]
  [(advance-big env ... F_1 F_2 trace)
   ----------------------------------------------- "advance existing history"
   (advance-big env ... (Forced F_1) (Forced F_2) (traces ("advance-forced") trace))]
  [(search-big env ... c SV trace_search)
   (commit-big env ... SV F trace_commit)
   ----------------------------------------------- "advance one exposed suspension"
   (advance-big env ... (More (Delay c)) (Forced F)
                (traces ("advance-delay") trace_search trace_commit))])

(define-judgment-form language
  #:mode (collect-big env-mode ... I O O)
  #:contract (collect-big env ... F O trace)
  [----------------------------------------------- "collect done"
   (collect-big env ... (Done supply) (Done supply) ("collect-done"))]
  [----------------------------------------------- "collect solo"
   (collect-big env ... (Solo σ) (Solo σ) ("collect-solo"))]
  [(collect-big env ... F O trace)
   ----------------------------------------------- "collect Emit"
   (collect-big env ... (Emit σ F) (Emit σ O) (traces ("collect-emit") trace))]
  [(collect-big env ... F O trace)
   ----------------------------------------------- "collect existing history"
   (collect-big env ... (Forced F) (Forced O) (traces ("collect-forced") trace))]
  [(search-big env ... c SV trace_search)
   (commit-big env ... SV F trace_commit)
   (collect-big env ... F O trace_collect)
   ----------------------------------------------- "collect suspended frontier"
   (collect-big env ... (More (Delay c)) (Forced O)
                (traces ("collect-delay") trace_search trace_commit trace_collect))])

(define-judgment-form language
  #:mode (observe-big env-mode ... I O O)
  #:contract (observe-big env ... o F trace)

  [----------------------------------------------- "observation value"
   (observe-big env ... F F ())]

  [(search-big env ... c SV trace_search)
   (render-big env ... SV O trace_render)
   ----------------------------------------------- "strict render operand"
   (observe-big env ... (render c) O (traces trace_search trace_render))]

  [(search-big env ... c SV trace_search)
   (commit-big env ... SV F trace_commit)
   ----------------------------------------------- "strict commit operand"
   (observe-big env ... (commit c) F (traces trace_search trace_commit))]

  [(observe-big env ... o F_1 trace_operand)
   (advance-big env ... F_1 F_2 trace_advance)
   ----------------------------------------------- "strict advance operand"
   (observe-big env ... (advance o) F_2 (traces trace_operand trace_advance))]

  [(observe-big env ... o F trace_operand)
   (collect-big env ... F O trace_collect)
   ----------------------------------------------- "strict collect operand"
   (observe-big env ... (collect o) O (traces trace_operand trace_collect))]

  [(side-condition ,(not (redex-match? parent F (term o))))
   (observe-big env ... o F trace)
   ----------------------------------------------- "observation Emit tail"
   (observe-big env ... (Emit σ o) (Emit σ F) trace)]

  [(side-condition ,(not (redex-match? parent F (term o))))
   (observe-big env ... o F trace)
   ----------------------------------------------- "observation Forced tail"
   (observe-big env ... (Forced o) (Forced F) trace)])


    ;; Unbounded fixed-point promotion. No source or machine transitions.
    (define (promote-search env ... computation)
      (match computation
        [(? value? value) value]
        [`(eval ,(and goal (or `(succeed ,_) `(fail ,_) `(,_ =? ,_ ,_) `(,_ != ,_ ,_))) ,state)
         (atomic-result goal state)]
        [`(eval ,(and call `(,(? relation-name?) ,_ (... ...))) ,state)
         (promote-search env ... `(eval ,(instantiate-relation relation-value call) ,state))]
        [`(eval (∃ ,binders ,body ,_) ,state) (promote-search env ... (allocate binders body state))]
        [`(eval (,left ∨ ,right ,_) ,state)
         (promote-merge env ... (promote-search env ... `(eval ,left ,state)) (promote-search env ... `(eval ,right ,state)))]
        [`(eval (,left ∧ ,right ,_) ,state) (promote-bind env ... (promote-search env ... `(eval ,left ,state)) right)]
        [`(eval (suspend ,goal ,_) ,state) `(Delay (eval ,goal ,state))]
        [`(mplus ,left ,right) (promote-merge env ... (promote-search env ... left) (promote-search env ... right))]
        [`(bind ,search ,goal) (promote-bind env ... (promote-search env ... search) goal)]
        [`(Yield ,state ,tail) `(Yield ,state ,(promote-search env ... tail))]
        [`(force ,search)
         (match-define `(Delay ,body) (promote-search env ... search))
         (promote-search env ... body)]))
    (define (promote-merge env ... left right)
      (match left
        [`(Empty ,_) right]
        [`(One ,state) `(Yield ,state ,right)]
        [`(Yield ,state ,tail) `(Yield ,state ,(promote-merge env ... tail right))]
        [`(Delay ,body) `(Delay (mplus ,right (force (Delay ,body))))]))
    (define (promote-bind env ... search goal)
      (match search
        [`(Empty ,supply) `(Empty ,supply)]
        [`(One ,state) (promote-search env ... `(eval ,goal ,state))]
        [`(Yield ,state ,tail) (promote-merge env ... (promote-search env ... `(eval ,goal ,state)) (promote-bind env ... tail goal))]
        [`(Delay ,body) `(Delay (bind ,body ,goal))]))
    (define (promote-render env ... search)
      (match search
        [`(Empty ,supply) `(Done ,supply)]
        [`(One ,state) `(Solo ,state)]
        [`(Yield ,state ,tail) `(Emit ,state ,(promote-render env ... tail))]
        [`(Delay ,body) `(Forced ,(promote-render env ... (promote-search env ... body)))]))
    (define (promote-commit env ... search)
      (match search
        [`(Empty ,supply) `(Done ,supply)]
        [`(One ,state) `(Solo ,state)]
        [`(Yield ,state ,tail) `(Emit ,state ,(promote-commit env ... tail))]
        [`(Delay ,body) `(More (Delay ,body))]))
    (define (promote-advance env ... frontier)
      (match frontier
        [`(Done ,_) frontier]
        [`(Solo ,_) frontier]
        [`(Emit ,state ,tail) `(Emit ,state ,(promote-advance env ... tail))]
        [`(Forced ,tail) `(Forced ,(promote-advance env ... tail))]
        [`(More (Delay ,body)) `(Forced ,(promote-commit env ... (promote-search env ... body)))]))
    (define (promote-collect env ... frontier)
      (match frontier
        [`(Done ,_) frontier]
        [`(Solo ,_) frontier]
        [`(Emit ,state ,tail) `(Emit ,state ,(promote-collect env ... tail))]
        [`(Forced ,tail) `(Forced ,(promote-collect env ... tail))]
        [`(More (Delay ,body))
         `(Forced ,(promote-collect env ... (promote-commit env ... (promote-search env ... body))))]))
    (define (promote-observe env ... computation)
      (match computation
        [(? (lambda (value) (redex-match? parent F value)) value) value]
        [`(render ,search) (promote-render env ... (promote-search env ... search))]
        [`(commit ,search) (promote-commit env ... (promote-search env ... search))]
        [`(advance ,frontier) (promote-advance env ... (promote-observe env ... frontier))]
        [`(collect ,frontier) (promote-collect env ... (promote-observe env ... frontier))]
        [`(Emit ,state ,tail) `(Emit ,state ,(promote-observe env ... tail))]
        [`(Forced ,tail) `(Forced ,(promote-observe env ... tail))]))
    (define (promote env ... computation)
      (unless (redex-match? parent q computation) (raise-argument-error (quote promote) "feature computation" computation))
      (if (redex-match? parent c computation) (promote-search env ... computation) (promote-observe env ... computation)))
    (define (evaluate env ... computation)
      (define answers
        (if (redex-match? parent c computation)
            (judgment-holds (search-big ,env ... ,computation SV trace) (SV trace))
            (judgment-holds (observe-big ,env ... ,computation F trace) (F trace))))
      (match answers
        [(list (list value labels)) (list value labels)]
        [_ (error 'evaluate "nonunique or missing finite Big derivation: ~e" answers)]))
    (define (raw-derivations env ... computation)
      (if (redex-match? parent c computation)
          (build-derivations (search-big ,env ... ,computation any_value any_trace))
          (build-derivations (observe-big ,env ... ,computation any_value any_trace)))))]))
