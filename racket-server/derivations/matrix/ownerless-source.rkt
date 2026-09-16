#lang racket

(require redex/reduction-semantics
         "../shared/feature-schema.rkt"
         "../shared/ownerless-grammar.rkt"
         (only-in "../shared/kernel.rkt" Failure Success)
         (for-syntax racket/base racket/syntax syntax/parse))

(provide define-ownerless-strict-source define-ownerless-strict-control)

;; Existing feature instances compose an independently generated grammar with
;; these equations; full E/N sources import their shared named grammar instead.
(define-syntax-rule (define-ownerless-strict-source row language parent supply
                     atomic allocate initial-supply options ...)
  (begin
    (define-ownerless-strict-language language parent supply options ...)
    (define-ownerless-strict-control row language atomic allocate initial-supply options ...)))

;; Compile-time specialization of the strict control equations. E and N
;; receive different inherited grammars, atomic kernels, allocation equations,
;; state payloads, failure summaries, and runtime-variable domains. These are
;; ordinary separately named Redex relations; no runtime row dispatcher exists.
(define-syntax (define-ownerless-strict-control stx)
  (syntax-parse stx
    [(_ row:id language:id atomic:id allocate:id initial-supply
        (~optional (~seq #:feature feature:id) #:defaults ([feature #'search])))
     #:with raw (format-id #'row "~a-raw" #'row)
     #:with red (format-id #'row "strict-~a-red" #'row)
     #:with contract (format-id #'row "~a-contract" #'row)
     #:with value? (format-id #'row "~a-value?" #'row)
     #:with observation? (format-id #'row "~a-observation?" #'row)
     #:with frontier? (format-id #'row "~a-frontier?" #'row)
     #:with initial (format-id #'row "~a-initial" #'row)
     #:with query-initial (format-id #'row "~a-query-initial" #'row)
     #:with run (format-id #'row "~a-run" #'row)
     #:with trace (format-id #'row "~a-trace" #'row)
     #:with atomic-search (format-id #'row "atomic-search/~a" #'row)
     #'(begin
         (provide language raw red contract value? observation? frontier?
                  initial query-initial run trace)
         (define (atomic-search goal state)
           (match (atomic goal state)
             [(Failure) `(Empty ,(second state))]
             [(Success next) `(One ,next)]))
         (define raw
           (feature-reduction-relation
            feature language
            [--> (eval a σ) ,(atomic-search (term a) (term σ)) eval-atom]
            [--> (eval (∃ (x (... ...)) g tag) σ)
                 ,(allocate (term (x (... ...))) (term g) (term σ)) allocate-fresh]
            [--> (eval (g_1 ∧ g_2 tag) σ)
                 (bind (eval g_1 σ) g_2) eval-conj]
            [--> (eval (g_1 ∨ g_2 tag) σ)
                 (mplus (eval g_1 σ) (eval g_2 σ)) eval-disj]
            [--> (eval (suspend g tag) σ) (Delay (eval g σ)) eval-suspend]
            [--> (mplus (Empty supply) SV) SV mplus-empty]
            [--> (mplus (One σ) SV) (Yield σ SV) mplus-one]
            [--> (mplus (Yield σ SV_1) SV_2)
                 (Yield σ (mplus SV_1 SV_2)) mplus-yield]
            [--> (mplus (Delay c) SV)
                 (Delay (mplus SV (force (Delay c)))) mplus-delay]
            [--> (bind (Empty supply) g) (Empty supply) bind-empty]
            [--> (bind (One σ) g) (eval g σ) bind-one]
            [--> (bind (Yield σ SV) g)
                 (mplus (eval g σ) (bind SV g)) bind-yield]
            [--> (bind (Delay c) g)
                 (Delay (bind c g)) bind-delay]
            [--> (force (Delay c)) c force-delay]
            [--> (render (Empty supply)) (Done supply) render-empty]
            [--> (render (One σ)) (Solo σ) render-one]
            [--> (render (Yield σ SV)) (Emit σ (render SV)) render-yield]
            [--> (render (Delay c))
                 (Forced (render c)) render-delay]
            [--> (commit (Empty supply)) (Done supply) commit-empty]
            [--> (commit (One σ)) (Solo σ) commit-one]
            [--> (commit (Yield σ SV)) (Emit σ (commit SV)) commit-yield]
            [--> (commit (Delay c)) (More (Delay c)) commit-delay]
            [--> (advance (Done supply)) (Done supply) advance-done]
            [--> (advance (Solo σ)) (Solo σ) advance-solo]
            [--> (advance (Emit σ F)) (Emit σ (advance F)) advance-emit]
            [--> (advance (Forced F)) (Forced (advance F)) advance-forced]
            [--> (advance (More (Delay c)))
                 (Forced (commit c)) advance-delay]
            [--> (collect (Done supply)) (Done supply) collect-done]
            [--> (collect (Solo σ)) (Solo σ) collect-solo]
            [--> (collect (Emit σ F)) (Emit σ (collect F)) collect-emit]
            [--> (collect (Forced F)) (Forced (collect F)) collect-forced]
            [--> (collect (More (Delay c)))
                 (Forced (collect (commit c))) collect-delay]))
         (define red (context-closure raw language C))
         (define (contract computation [ignored-prefix '()])
           (match (apply-reduction-relation/tag-with-names raw computation)
             ['() #f]
             [(list result) result]
             [results (error 'contract "nonunique raw proof: ~e" results)]))
         (define (value? value) (redex-match? language SV value))
         (define (observation? value) (redex-match? language O value))
         (define (frontier? value) (redex-match? language F value))
         (define (initial goal
                          #:state [state `(state ,initial-supply () () () (label "initial"))])
           `(eval ,goal ,state))
         (define (query-initial goal
                                #:state [state `(state ,initial-supply () () () (label "initial"))])
           `(commit ,(initial goal #:state state)))
         (define (trace computation [fuel 100000] [reversed '()])
           (unless (exact-nonnegative-integer? fuel)
             (raise-argument-error 'trace "exact-nonnegative-integer?" fuel))
           (match (apply-reduction-relation/tag-with-names red computation)
             ['()
              (unless (or (value? computation) (frontier? computation))
                (error 'trace "stuck computation: ~e" computation))
              (reverse reversed)]
             [(list (and step (list _ next)))
              (when (zero? fuel) (error 'trace "fuel exhausted"))
              (trace next (sub1 fuel) (cons step reversed))]
             [results (error 'trace "nonunique source proof: ~e" results)]))
         (define (run computation [fuel 100000])
           (unless (exact-nonnegative-integer? fuel)
             (raise-argument-error 'run "exact-nonnegative-integer?" fuel))
           (cond
             [(or (value? computation) (frontier? computation)) computation]
             [else
              (when (zero? fuel) (error 'run "fuel exhausted"))
              (match (apply-reduction-relation red computation)
                [(list next) (run next (sub1 fuel))]
                [results (error 'run "stuck or nonunique source proof: ~e" results)])])))]))
