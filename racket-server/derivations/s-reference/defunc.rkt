#lang racket

(require (only-in "../shared/kernel.rkt"
                  owners-support owners-append fresh-names substitute-goal)
         "data.rkt" "relations.rkt")
(provide (all-defined-out))

;; Literal defunctionalization of cps.rkt. Every /d call is a tail transfer;
;; derive.rkt reifies those transfers as inspected first-order machine states.
(define (eval/d goal state owners inherited k)
  (define relations (goal-relations goal))
  (match (goal-body goal)
    [`(∃ ,binders ,body ,tag)
     (define intro (fresh-names (owners-support owners inherited) (length binders)))
     (eval/d (retain-goal relations (substitute-goal body (map list binders intro))) state
             (owners-append owners `(Owners (Owner ,intro ,tag))) inherited k)]
    [`(,left ∧ ,right ,_)
     (define here (owners-support owners inherited))
     (eval/d (retain-goal relations left) state '(Owners) here
             (KConj (retain-goal relations right) owners inherited k))]
    [`(,left ∨ ,right ,_)
     (define here (owners-support owners inherited))
     (eval/d (retain-goal relations left) state '(Owners) here
             (KDisjLeft (retain-goal relations right) state owners inherited k))]
    [`(suspend ,body ,_)
     (return/d `(Delay ,owners ,(REval (retain-goal relations body) state)) k)]
    [(? relation-call? call)
     (eval/d (retain-goal relations (expand-call relations call)) state owners inherited k)]
    [atom
     (define outcome (atomic/data atom state))
     (outcome/d outcome (FEmpty owners k) (SOne owners k))]))

(define (merge/d left right owners inherited k)
  (match left
    [`(Empty ,_) (return/d (prefix owners right) k)]
    [`(One ,local ,state)
     (return/d `(Yield ,owners (Answer ,local ,state) ,right) k)]
    [`(Yield ,local (Answer ,answer ,state) ,rest)
     (define answer* `(Answer ,(owners-append local answer) ,state))
     (merge/d (prefix local rest) right '(Owners)
              (owners-support owners inherited)
              (KMergeYield owners answer* k))]
    [`(Delay ,_ ,_)
     (return/d `(Delay ,owners ,(RMerge right left)) k)]))

(define (bind/d search continue owners inherited k)
  (match search
    [`(Empty ,local) (return/d `(Empty ,(owners-append owners local)) k)]
    [`(One ,local ,state)
     (continue/d continue state (owners-append owners local) inherited k)]
    [`(Yield ,local (Answer ,answer ,state) ,rest)
     (define common (owners-append owners local))
     (define here (owners-support common inherited))
     (continue/d continue state answer here
                 (KBindHead rest continue common inherited k))]
    [`(Delay ,local ,resume)
     (define common (owners-append owners local))
     (return/d `(Delay ,common ,(RBind resume continue)) k)]))

(define (continue/d continue state owners inherited k)
  (match continue [(GRight goal) (eval/d goal state owners inherited k)]))

(define (resume/d resume owners inherited k)
  (match resume
    [(REval goal state) (eval/d goal state owners inherited k)]
    [(RMerge right left)
     (force/d left (owners-support owners inherited)
              (KMergeForced right owners inherited k))]
    [(RBind rest continue)
     (resume/d rest '(Owners) (owners-support owners inherited)
               (KBindForced continue owners inherited k))]))

(define (force/d search inherited k)
  (match search
    [`(Delay ,owners ,resume) (resume/d resume owners inherited k)]))

(define (commit/d search k)
  (match search
    [`(Empty ,owners) (return/d `(Done ,owners) k)]
    [`(One ,owners ,state) (return/d `(Solo ,owners ,state) k)]
    [`(Yield ,owners ,answer ,rest) (commit/d rest (KCommitEmit owners answer k))]
    [`(Delay ,_ ,_) (return/d `(More ,search) k)]))

(define (advance/d frontier inherited k)
  (match frontier
    [`(Done ,_) (return/d frontier k)]
    [`(Solo ,_ ,_) (return/d frontier k)]
    [`(Emit ,owners ,answer ,rest)
     (advance/d rest (owners-support owners inherited) (KAdvanceEmit owners answer k))]
    [`(Forced ,owners ,rest)
     (advance/d rest (owners-support owners inherited) (KAdvanceHistory owners k))]
    [`(More (Delay ,owners ,resume))
     (resume/d resume '(Owners) (owners-support owners inherited)
               (KCommit (KAdvanceForced owners k)))]))

(define (collect/d frontier inherited k)
  (match frontier
    [`(Done ,_) (return/d frontier k)]
    [`(Solo ,_ ,_) (return/d frontier k)]
    [`(Emit ,owners ,answer ,rest)
     (collect/d rest (owners-support owners inherited) (KCollectEmit owners answer k))]
    [`(Forced ,owners ,rest)
     (collect/d rest (owners-support owners inherited) (KCollectHistory owners k))]
    [`(More (Delay ,owners ,resume))
     (define here (owners-support owners inherited))
     (resume/d resume '(Owners) here (KCommit (KCollectResume owners here k)))]))

(define (outcome/d outcome failure success)
  (match outcome
    [(Failure) (failure/d failure)]
    [(Success state) (success/d success state)]))
(define (failure/d failure)
  (match failure [(FEmpty owners k) (return/d `(Empty ,owners) k)]))
(define (success/d success state)
  (match success [(SOne owners k) (return/d `(One ,owners ,state) k)]))

(define (return/d value k)
  (match k
    [(KDone) value]
    [(KProgram relations rest) (return/d `(program ,relations ,value) rest)]
    [(KConj right owners inherited rest)
     (bind/d value (GRight right) owners inherited rest)]
    [(KDisjLeft right state owners inherited rest)
     (define here (owners-support owners inherited))
     (eval/d right state '(Owners) here (KDisjRight value owners inherited rest))]
    [(KDisjRight left owners inherited rest)
     (merge/d left value owners inherited rest)]
    [(KMergeYield owners answer rest) (return/d `(Yield ,owners ,answer ,value) rest)]
    [(KBindHead tail continue common inherited rest)
     (define here (owners-support common inherited))
     (bind/d tail continue '(Owners) here (KBindTail value common inherited rest))]
    [(KBindTail head common inherited rest) (merge/d head value common inherited rest)]
    [(KMergeForced right owners inherited rest) (merge/d right value owners inherited rest)]
    [(KBindForced continue owners inherited rest)
     (bind/d value continue owners inherited rest)]
    [(KCommit rest) (commit/d value rest)]
    [(KCommitEmit owners answer rest) (return/d `(Emit ,owners ,answer ,value) rest)]
    [(KAdvanceEmit owners answer rest) (return/d `(Emit ,owners ,answer ,value) rest)]
    [(KAdvanceHistory owners rest) (return/d `(Forced ,owners ,value) rest)]
    [(KAdvanceForced owners rest) (return/d `(Forced ,owners ,value) rest)]
    [(KCollectEmit owners answer rest) (return/d `(Emit ,owners ,answer ,value) rest)]
    [(KCollectHistory owners rest) (return/d `(Forced ,owners ,value) rest)]
    [(KCollectResume owners here rest) (collect/d value here (KCollectForced owners rest))]
    [(KCollectForced owners rest) (return/d `(Forced ,owners ,value) rest)]))

(define (run goal #:owners [owners '(Owners)]
             #:state [state '(state () () () (label "initial"))]
             #:relations [relations #f])
  (eval/d (retain-goal relations goal) state owners '()
          (KCommit (if relations (KProgram relations (KDone)) (KDone)))))
(define (resume-once frontier)
  (match frontier
    [`(program ,relations ,body) (advance/d body '() (KProgram relations (KDone)))]
    [_ (advance/d frontier '() (KDone))]))
(define (collect-all frontier)
  (match frontier
    [`(program ,relations ,body) (collect/d body '() (KProgram relations (KDone)))]
    [_ (collect/d frontier '() (KDone))]))
