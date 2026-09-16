#lang racket

(require redex/reduction-semantics
         (for-syntax racket/base racket/list syntax/parse))

(provide feature-reduction-relation feature-labels
         (for-syntax feature-goal-productions feature-extra-productions))

(begin-for-syntax
  (define core-labels
    '(eval-atom allocate-fresh eval-conj bind-empty bind-one render-empty render-one
      commit-empty commit-one advance-done advance-solo collect-done collect-solo))
  (define delay-labels
    '(eval-suspend bind-delay force-delay render-delay
      commit-delay advance-forced advance-delay collect-forced collect-delay))
  (define disjunction-labels
    '(eval-disj mplus-empty mplus-one mplus-yield bind-yield render-yield
      commit-yield advance-emit collect-emit))
  (define (labels feature)
    (case feature
      [(core) core-labels]
      [(delay) (append core-labels delay-labels)]
      [(disjunction) (append core-labels disjunction-labels)]
      [(search) (append core-labels delay-labels disjunction-labels '(mplus-delay))]
      [else (raise-argument-error 'feature-schema "core, delay, disjunction or search" feature)]))
  (define (feature-goal-productions feature context)
    (map (lambda (datum) (datum->syntax context datum))
         (case feature
           [(core) '()]
           [(delay) '((suspend g tag))]
           [(disjunction) '((g ∨ g tag))]
           [(search) '((g ∨ g tag) (suspend g tag))]))))

(begin-for-syntax
  (define (feature-extra-productions feature category owned? context)
    (define delay? (memq feature '(delay search)))
    (define disjunction? (memq feature '(disjunction search)))
    (define delay
      (case category
        [(SV) (if owned? '((Delay owners c)) '((Delay c)))]
        [(c) '((force c))]
        [(E) '((force E))]
        [(O) (if owned? '((Forced owners O)) '((Forced O)))]
        [(F) (if owned? '((Forced owners F) (More (Delay owners c)))
                 '((Forced F) (More (Delay c))))]
        [(o) (if owned? '((Forced owners o)) '((Forced o)))]
        [(C) (if owned? '((Forced owners C)) '((Forced C)))]))
    (define disjunction
      (case category
        [(SV) (if owned? '((Yield owners A SV)) '((Yield σ SV)))]
        [(c) (if owned? '((mplus owners c c) (Yield owners A c)) '((mplus c c) (Yield σ c)))]
        [(E) (if owned? '((mplus owners E c) (mplus owners SV E) (Yield owners A E))
                 '((mplus E c) (mplus SV E) (Yield σ E)))]
        [(O) (if owned? '((Emit owners A O)) '((Emit σ O)))]
        [(F) (if owned? '((Emit owners A F)) '((Emit σ F)))]
        [(o) (if owned? '((Emit owners A o)) '((Emit σ o)))]
        [(C) (if owned? '((Emit owners A C)) '((Emit σ C)))]))
    (map (lambda (datum) (datum->syntax context datum))
         (append (if delay? delay '()) (if disjunction? disjunction '())))))

;; The macro selects literal rule clauses at expansion time. A smaller feature
;; relation does not retain disabled named clauses or invoke a runtime feature
;; predicate. The Search/rail interaction mplus-delay is separately owned.
(define-syntax (feature-reduction-relation stx)
  (syntax-parse stx
    [(_ feature:id language:id clause ...)
     (define enabled (labels (syntax-e #'feature)))
     (define selected
       (filter (lambda (clause)
                 (member (syntax-e (last (syntax->list clause))) enabled))
               (syntax->list #'(clause ...))))
     #`(reduction-relation language #:domain q #,@selected)]))

(define-syntax (feature-labels stx)
  (syntax-parse stx
    [(_ feature:id) #`'#,(map symbol->string (labels (syntax-e #'feature)))]))
