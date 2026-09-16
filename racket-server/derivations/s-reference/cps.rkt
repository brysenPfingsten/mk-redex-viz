#lang racket

(require (only-in "../shared/kernel.rkt"
                  owners-support owners-append fresh-names substitute-goal)
         (only-in "interpreter.rkt" atomic prefix observe-closure)
         "relations.rkt")
(provide eval/k merge/k bind/k force/k commit/k advance/k collect/k
         run resume-once collect-all)

;; Refunctionalized strict control: the continuation sites expose the frames
;; of eval/mplus/bind/commit and the public consumers. Resumptions receive root
;; Owners O, inherited support P, and k. No continuation attaches O on return.
(define (eval/k goal state owners inherited k)
  (define relations (goal-relations goal))
  (match (goal-body goal)
    [`(∃ ,binders ,body ,tag)
     (define intro (fresh-names (owners-support owners inherited) (length binders)))
     (eval/k (retain-goal relations (substitute-goal body (map list binders intro))) state
             (owners-append owners `(Owners (Owner ,intro ,tag))) inherited k)]
    [`(,left ∧ ,right ,_)
     (define here (owners-support owners inherited))
     (eval/k (retain-goal relations left) state '(Owners) here
             (lambda (search) ; KConj
               (bind/k search
                       (observe-closure
                        'continue-goal
                        (lambda (state* owners* inherited* k*)
                          (eval/k (retain-goal relations right) state* owners* inherited* k*))
                        (list (retain-goal relations right)))
                       owners inherited k)))]
    [`(,left ∨ ,right ,_)
     (define here (owners-support owners inherited))
     (eval/k (retain-goal relations left) state '(Owners) here
             (lambda (left*) ; KDisjLeft
               (eval/k (retain-goal relations right) state '(Owners) here
                       (lambda (right*) ; KDisjRight
                         (merge/k left* right* owners inherited k)))))]
    [`(suspend ,body ,_)
     (k `(Delay ,owners
                ,(observe-closure
                  'resume-eval
                  (lambda (owners* inherited* k*)
                    (eval/k (retain-goal relations body) state owners* inherited* k*))
                  (list (retain-goal relations body) state))))]
    [(? relation-call? call)
     (eval/k (retain-goal relations (expand-call relations call)) state owners inherited k)]
    [atom
     (define outcome (atomic atom state))
     (outcome (lambda () (k `(Empty ,owners)))
              (lambda (next) (k `(One ,owners ,next))))]))

(define (merge/k left right owners inherited k)
  (match left
    [`(Empty ,_) (k (prefix owners right))]
    [`(One ,local ,state) (k `(Yield ,owners (Answer ,local ,state) ,right))]
    [`(Yield ,local (Answer ,answer ,state) ,rest)
     (define answer* `(Answer ,(owners-append local answer) ,state))
     (merge/k (prefix local rest) right '(Owners)
              (owners-support owners inherited)
              (lambda (tail) ; KMergeYield
                (k `(Yield ,owners ,answer* ,tail))))]
    [`(Delay ,_ ,_)
     (k `(Delay ,owners
                ,(observe-closure
                  'resume-merge
                  (lambda (owners* inherited* k*)
                    (force/k left (owners-support owners* inherited*)
                             (lambda (forced) ; KMergeForced(right,O,P,k)
                               (merge/k right forced owners* inherited* k*))))
                  (list right left))))]))

(define (bind/k search continue owners inherited k)
  (match search
    [`(Empty ,local) (k `(Empty ,(owners-append owners local)))]
    [`(One ,local ,state)
     (continue state (owners-append owners local) inherited k)]
    [`(Yield ,local (Answer ,answer ,state) ,rest)
     (define common (owners-append owners local))
     (define here (owners-support common inherited))
     (continue state answer here
               (lambda (head) ; KBindHead
                 (bind/k rest continue '(Owners) here
                         (lambda (tail) ; KBindTail
                           (merge/k head tail common inherited k)))))]
    [`(Delay ,local ,resume)
     (define common (owners-append owners local))
     (k `(Delay ,common
                ,(observe-closure
                  'resume-bind
                  (lambda (owners* inherited* k*)
                    (resume '(Owners) (owners-support owners* inherited*)
                            (lambda (forced) ; KBindForced(continue,O,P,k)
                              (bind/k forced continue owners* inherited* k*))))
                  (list resume continue))))]))

(define (force/k search inherited k)
  (match search
    [`(Delay ,owners ,resume) (resume owners inherited k)]))

(define (commit/k search k)
  (match search
    [`(Empty ,owners) (k `(Done ,owners))]
    [`(One ,owners ,state) (k `(Solo ,owners ,state))]
    [`(Yield ,owners ,answer ,rest)
     (commit/k rest
               (lambda (tail) ; KCommitEmit
                 (k `(Emit ,owners ,answer ,tail))))]
    [`(Delay ,_ ,_) (k `(More ,search))]))

(define (advance/k frontier inherited k)
  (match frontier
    [`(Done ,_) (k frontier)]
    [`(Solo ,_ ,_) (k frontier)]
    [`(Emit ,owners ,answer ,rest)
     (advance/k rest (owners-support owners inherited)
                (lambda (tail) ; KAdvanceEmit
                  (k `(Emit ,owners ,answer ,tail))))]
    [`(Forced ,owners ,rest)
     (advance/k rest (owners-support owners inherited)
                (lambda (tail) ; KAdvanceHistory
                  (k `(Forced ,owners ,tail))))]
    [`(More (Delay ,owners ,resume))
     (define committed (lambda (next) (k `(Forced ,owners ,next))))
     (resume '(Owners) (owners-support owners inherited)
             (lambda (search) (commit/k search committed)))])) ; KCommit

(define (collect/k frontier inherited k)
  (match frontier
    [`(Done ,_) (k frontier)]
    [`(Solo ,_ ,_) (k frontier)]
    [`(Emit ,owners ,answer ,rest)
     (collect/k rest (owners-support owners inherited)
                (lambda (tail) ; KCollectEmit
                  (k `(Emit ,owners ,answer ,tail))))]
    [`(Forced ,owners ,rest)
     (collect/k rest (owners-support owners inherited)
                (lambda (tail) ; KCollectHistory
                  (k `(Forced ,owners ,tail))))]
    [`(More (Delay ,owners ,resume))
     (define here (owners-support owners inherited))
     (define committed
       (lambda (next) ; KCollectResume
         (collect/k next here
                    (lambda (tail) ; KCollectForced
                      (k `(Forced ,owners ,tail))))))
     (resume '(Owners) here
             (lambda (search) (commit/k search committed)))])) ; KCommit

(define (run goal #:owners [owners '(Owners)]
             #:state [state '(state () () () (label "initial"))]
             #:relations [relations #f])
  (define done
    (if relations
        (lambda (value) `(program ,relations ,value)) ; KProgram
        (lambda (value) value)))
  (eval/k (retain-goal relations goal) state owners '()
          (lambda (search) (commit/k search done))))
(define (resume-once frontier)
  (match frontier
    [`(program ,relations ,body)
     (advance/k body '() (lambda (value) `(program ,relations ,value)))]
    [_ (advance/k frontier '() (lambda (value) value))]))
(define (collect-all frontier)
  (match frontier
    [`(program ,relations ,body)
     (collect/k body '() (lambda (value) `(program ,relations ,value)))]
    [_ (collect/k frontier '() (lambda (value) value))]))
