#lang racket

(require (only-in "../shared/kernel.rkt"
                  owners-support owners-append fresh-names substitute-goal)
         "../shared/kernel-equations.rkt"
         "relations.rkt")
(provide (all-defined-out))

;; This interpreter reconstructs the source's eager root attachment. A delayed
;; computation accepts its root Owners and inherited support when resumed.
;; These arguments say where its syntax is running; they are not State fields.
;; Only Delay suspends search work. Active Yield has three fields; the unary
;; More in a Frontier marks its unfinished Delay tip.
(define (failure-outcome) (lambda (failure _success) (failure)))
(define (success-outcome state) (lambda (_failure success) (success state)))
(define-kernel atomic failure-outcome success-outcome)

;; Test-only observation of actual closure captures. The default does nothing;
;; no description is stored in Search and no suspended procedure is executed.
;; A test can record these notifications externally to reify delayed syntax.
(define current-closure-observer
  (make-parameter (lambda (_family _procedure _captures) (void))))
(define (observe-closure family procedure captures)
  ((current-closure-observer) family procedure captures)
  procedure)

;; Mature root attachment still belongs to mplus's structural equations. What
;; this account avoids is attachment AFTER executing a resumption.
(define (prefix owners search)
  (match search
    [`(Empty ,local) `(Empty ,(owners-append owners local))]
    [`(One ,local ,state) `(One ,(owners-append owners local) ,state)]
    [`(Yield ,local ,answer ,rest)
     `(Yield ,(owners-append owners local) ,answer ,rest)]
    [`(Delay ,local ,resume) `(Delay ,(owners-append owners local) ,resume)]))

(define (eval/s goal state owners inherited)
  (define relations (goal-relations goal))
  (match (goal-body goal)
    [`(∃ ,binders ,body ,tag)
     (define intro (fresh-names (owners-support owners inherited) (length binders)))
     (eval/s (retain-goal relations (substitute-goal body (map list binders intro))) state
             (owners-append owners `(Owners (Owner ,intro ,tag))) inherited)]
    [`(,left ∧ ,right ,_)
     (bind/s (eval/s (retain-goal relations left) state '(Owners) (owners-support owners inherited))
             (observe-closure
              'continue-goal
              (lambda (state* owners* inherited*)
                (eval/s (retain-goal relations right) state* owners* inherited*))
              (list (retain-goal relations right)))
             owners inherited)]
    [`(,left ∨ ,right ,_)
     (merge/s (eval/s (retain-goal relations left) state '(Owners) (owners-support owners inherited))
              (eval/s (retain-goal relations right) state '(Owners) (owners-support owners inherited))
              owners inherited)]
    [`(suspend ,body ,_)
     ;; Source body: eval(Owners(), body, state). Root attachment replaces its
     ;; empty root before evaluation, giving eval(O, body, state) under P.
     `(Delay ,owners
             ,(observe-closure
               'resume-eval
               (lambda (owners* inherited*)
                 (eval/s (retain-goal relations body) state owners* inherited*))
               (list (retain-goal relations body) state)))]
    [(? relation-call? call)
     ;; Expansion is eager. Only explicit suspend produces a resumption.
     (eval/s (retain-goal relations (expand-call relations call)) state owners inherited)]
    [atom ((atomic atom state)
           (lambda () `(Empty ,owners))
           (lambda (next) `(One ,owners ,next)))]))

(define (merge/s left right owners inherited)
  (match left
    [`(Empty ,_) (prefix owners right)]
    [`(One ,local ,state) `(Yield ,owners (Answer ,local ,state) ,right)]
    [`(Yield ,local (Answer ,answer ,state) ,rest)
     `(Yield ,owners (Answer ,(owners-append local answer) ,state)
            ,(merge/s (prefix local rest) right '(Owners)
                      (owners-support owners inherited)))]
    [`(Delay ,_ ,_)
     ;; Source body: mplus(Owners(), right, force(left)). The root O scopes
     ;; both operands; force's caller support is P + names(O).
     `(Delay ,owners
             ,(observe-closure
               'resume-merge
               (lambda (owners* inherited*)
                 (merge/s right
                          (force/s left (owners-support owners* inherited*))
                          owners* inherited*))
               (list right left)))]))

(define (bind/s search continue owners inherited)
  (match search
    [`(Empty ,local) `(Empty ,(owners-append owners local))]
    [`(One ,local ,state)
     (continue state (owners-append owners local) inherited)]
    [`(Yield ,local (Answer ,answer ,state) ,rest)
     (define common (owners-append owners local))
     (define here (owners-support common inherited))
     (merge/s (continue state answer here)
              (bind/s rest continue '(Owners) here) common inherited)]
    [`(Delay ,local ,resume)
     (define common (owners-append owners local))
     ;; Source body: bind(Owners(), c, goal). Its O scopes c; c therefore
     ;; retains its empty local root and runs under P + names(O).
     `(Delay ,common
             ,(observe-closure
               'resume-bind
               (lambda (owners* inherited*)
                 (bind/s (resume '(Owners) (owners-support owners* inherited*))
                         continue owners* inherited*))
               (list resume continue)))]))

;; Internal force enters the owned source computation directly. There is no
;; post-return operation here, and consequently no KPrefix continuation site.
(define (force/s search inherited)
  (match search
    [`(Delay ,owners ,resume) (resume owners inherited)]))

;; Commitment is unchanged: it consumes mature Search, runs no goal or
;; resumption, and leaves an actual Delay behind the Frontier's unary More.
(define (commit/s search)
  (match search
    [`(Empty ,owners) `(Done ,owners)]
    [`(One ,owners ,state) `(Solo ,owners ,state)]
    [`(Yield ,owners ,answer ,rest)
     `(Emit ,owners ,answer ,(commit/s rest))]
    [`(Delay ,_ ,_) `(More ,search)]))

;; Public consumers recover support from the retained syntactic ancestry.
;; The crossed Delay's Owners stay on Forced, so its body has an empty root.
(define (advance/s frontier inherited)
  (match frontier
    [`(Done ,_) frontier]
    [`(Solo ,_ ,_) frontier]
    [`(Emit ,owners ,answer ,rest)
     `(Emit ,owners ,answer ,(advance/s rest (owners-support owners inherited)))]
    [`(Forced ,owners ,rest)
     `(Forced ,owners ,(advance/s rest (owners-support owners inherited)))]
    [`(More (Delay ,owners ,resume))
     `(Forced ,owners
              ,(commit/s (resume '(Owners) (owners-support owners inherited))))]))

(define (collect/s frontier inherited)
  (match frontier
    [`(Done ,_) frontier]
    [`(Solo ,_ ,_) frontier]
    [`(Emit ,owners ,answer ,rest)
     `(Emit ,owners ,answer ,(collect/s rest (owners-support owners inherited)))]
    [`(Forced ,owners ,rest)
     `(Forced ,owners ,(collect/s rest (owners-support owners inherited)))]
    [`(More (Delay ,owners ,resume))
     (define here (owners-support owners inherited))
     `(Forced ,owners ,(collect/s (commit/s (resume '(Owners) here)) here))]))

(define (run goal #:owners [owners '(Owners)]
             #:state [state '(state () () () (label "initial"))]
             #:relations [relations #f])
  (define frontier (commit/s (eval/s (retain-goal relations goal) state owners '())))
  (if relations `(program ,relations ,frontier) frontier))
(define (resume-once frontier)
  (match frontier
    [`(program ,relations ,body) `(program ,relations ,(advance/s body '()))]
    [_ (advance/s frontier '())]))
(define (collect-all frontier)
  (match frontier
    [`(program ,relations ,body) `(program ,relations ,(collect/s body '()))]
    [_ (collect/s frontier '())]))
