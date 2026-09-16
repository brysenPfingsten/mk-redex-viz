#lang racket
;; Generated from defunc.rkt by derive.rkt. Regenerate; do not edit.
(require (only-in
          "../shared/kernel.rkt"
          owners-support
          owners-append
          fresh-names
          substitute-goal)
         "data.rkt"
         "relations.rkt"
         "../shared/runtime.rkt")

(provide (all-defined-out))

(define signatures
  '((eval/d goal state owners inherited k)
    (merge/d left right owners inherited k)
    (bind/d search continue owners inherited k)
    (continue/d continue state owners inherited k)
    (resume/d resume owners inherited k)
    (force/d search inherited k)
    (commit/d search k)
    (advance/d frontier inherited k)
    (collect/d frontier inherited k)
    (outcome/d outcome failure success)
    (failure/d failure)
    (success/d success state)
    (return/d value k)))

(struct Call (pc operands) #:transparent)

(struct Halted (value) #:transparent)

(define (initial
         goal
         #:owners
         (owners '(Owners))
         #:state
         (state '(state () () () (label "initial")))
         #:relations
         (relations #f))
  (Call
   'eval/d
   (list
    (retain-goal relations goal)
    state
    owners
    '()
    (KCommit (if relations (KProgram relations (KDone)) (KDone))))))

(define (drive/steps current fuel)
  (match
   current
   ((Halted value) value)
   (_
    (when (zero? fuel) (exhausted 's-reference-machine current))
    (drive/steps (step current) (sub1 fuel)))))

(define (drive current #:fuel (fuel 100000)) (check-fuel fuel) (drive/steps current fuel))

(define (run
         goal
         #:owners
         (owners '(Owners))
         #:state
         (state '(state () () () (label "initial")))
         #:relations
         (relations #f)
         #:fuel
         (fuel 100000))
  (drive (initial goal #:owners owners #:state state #:relations relations) #:fuel fuel))

(define (resume-once frontier #:fuel (fuel 100000))
  (match
   frontier
   (`(program ,relations ,body)
    (drive (Call 'advance/d (list body '() (KProgram relations (KDone)))) #:fuel fuel))
   (_ (drive (Call 'advance/d (list frontier '() (KDone))) #:fuel fuel))))

(define (collect-all frontier #:fuel (fuel 100000))
  (match
   frontier
   (`(program ,relations ,body)
    (drive (Call 'collect/d (list body '() (KProgram relations (KDone)))) #:fuel fuel))
   (_ (drive (Call 'collect/d (list frontier '() (KDone))) #:fuel fuel))))

(define (step current)
  (match
   current
   ((Call 'eval/d (list goal state owners inherited k))
    (begin
      (define relations (goal-relations goal))
      (match
       (goal-body goal)
       (`(∃ ,binders ,body ,tag)
        (define intro (fresh-names (owners-support owners inherited) (length binders)))
        (Call
         'eval/d
         (list
          (retain-goal relations (substitute-goal body (map list binders intro)))
          state
          (owners-append owners `(Owners (Owner ,intro ,tag)))
          inherited
          k)))
       (`(,left ∧ ,right ,_)
        (define here (owners-support owners inherited))
        (Call
         'eval/d
         (list
          (retain-goal relations left)
          state
          '(Owners)
          here
          (KConj (retain-goal relations right) owners inherited k))))
       (`(,left ∨ ,right ,_)
        (define here (owners-support owners inherited))
        (Call
         'eval/d
         (list
          (retain-goal relations left)
          state
          '(Owners)
          here
          (KDisjLeft (retain-goal relations right) state owners inherited k))))
       (`(suspend ,body ,_)
        (Call 'return/d (list `(Delay ,owners ,(REval (retain-goal relations body) state)) k)))
       ((? relation-call? call)
        (Call
         'eval/d
         (list (retain-goal relations (expand-call relations call)) state owners inherited k)))
       (atom
        (define outcome (atomic/data atom state))
        (Call 'outcome/d (list outcome (FEmpty owners k) (SOne owners k)))))))
   ((Call 'merge/d (list left right owners inherited k))
    (begin
      (match
       left
       (`(Empty ,_) (Call 'return/d (list (prefix owners right) k)))
       (`(One ,local ,state)
        (Call 'return/d (list `(Yield ,owners (Answer ,local ,state) ,right) k)))
       (`(Yield ,local (Answer ,answer ,state) ,rest)
        (define answer* `(Answer ,(owners-append local answer) ,state))
        (Call
         'merge/d
         (list
          (prefix local rest)
          right
          '(Owners)
          (owners-support owners inherited)
          (KMergeYield owners answer* k))))
       (`(Delay ,_ ,_) (Call 'return/d (list `(Delay ,owners ,(RMerge right left)) k))))))
   ((Call 'bind/d (list search continue owners inherited k))
    (begin
      (match
       search
       (`(Empty ,local) (Call 'return/d (list `(Empty ,(owners-append owners local)) k)))
       (`(One ,local ,state)
        (Call 'continue/d (list continue state (owners-append owners local) inherited k)))
       (`(Yield ,local (Answer ,answer ,state) ,rest)
        (define common (owners-append owners local))
        (define here (owners-support common inherited))
        (Call
         'continue/d
         (list continue state answer here (KBindHead rest continue common inherited k))))
       (`(Delay ,local ,resume)
        (define common (owners-append owners local))
        (Call 'return/d (list `(Delay ,common ,(RBind resume continue)) k))))))
   ((Call 'continue/d (list continue state owners inherited k))
    (begin
      (match continue ((GRight goal) (Call 'eval/d (list goal state owners inherited k))))))
   ((Call 'resume/d (list resume owners inherited k))
    (begin
      (match
       resume
       ((REval goal state) (Call 'eval/d (list goal state owners inherited k)))
       ((RMerge right left)
        (Call
         'force/d
         (list left (owners-support owners inherited) (KMergeForced right owners inherited k))))
       ((RBind rest continue)
        (Call
         'resume/d
         (list
          rest
          '(Owners)
          (owners-support owners inherited)
          (KBindForced continue owners inherited k)))))))
   ((Call 'force/d (list search inherited k))
    (begin
      (match
       search
       (`(Delay ,owners ,resume) (Call 'resume/d (list resume owners inherited k))))))
   ((Call 'commit/d (list search k))
    (begin
      (match
       search
       (`(Empty ,owners) (Call 'return/d (list `(Done ,owners) k)))
       (`(One ,owners ,state) (Call 'return/d (list `(Solo ,owners ,state) k)))
       (`(Yield ,owners ,answer ,rest)
        (Call 'commit/d (list rest (KCommitEmit owners answer k))))
       (`(Delay ,_ ,_) (Call 'return/d (list `(More ,search) k))))))
   ((Call 'advance/d (list frontier inherited k))
    (begin
      (match
       frontier
       (`(Done ,_) (Call 'return/d (list frontier k)))
       (`(Solo ,_ ,_) (Call 'return/d (list frontier k)))
       (`(Emit ,owners ,answer ,rest)
        (Call
         'advance/d
         (list rest (owners-support owners inherited) (KAdvanceEmit owners answer k))))
       (`(Forced ,owners ,rest)
        (Call
         'advance/d
         (list rest (owners-support owners inherited) (KAdvanceHistory owners k))))
       (`(More (Delay ,owners ,resume))
        (Call
         'resume/d
         (list
          resume
          '(Owners)
          (owners-support owners inherited)
          (KCommit (KAdvanceForced owners k))))))))
   ((Call 'collect/d (list frontier inherited k))
    (begin
      (match
       frontier
       (`(Done ,_) (Call 'return/d (list frontier k)))
       (`(Solo ,_ ,_) (Call 'return/d (list frontier k)))
       (`(Emit ,owners ,answer ,rest)
        (Call
         'collect/d
         (list rest (owners-support owners inherited) (KCollectEmit owners answer k))))
       (`(Forced ,owners ,rest)
        (Call
         'collect/d
         (list rest (owners-support owners inherited) (KCollectHistory owners k))))
       (`(More (Delay ,owners ,resume))
        (define here (owners-support owners inherited))
        (Call
         'resume/d
         (list resume '(Owners) here (KCommit (KCollectResume owners here k))))))))
   ((Call 'outcome/d (list outcome failure success))
    (begin
      (match
       outcome
       ((Failure) (Call 'failure/d (list failure)))
       ((Success state) (Call 'success/d (list success state))))))
   ((Call 'failure/d (list failure))
    (begin (match failure ((FEmpty owners k) (Call 'return/d (list `(Empty ,owners) k))))))
   ((Call 'success/d (list success state))
    (begin (match success ((SOne owners k) (Call 'return/d (list `(One ,owners ,state) k))))))
   ((Call 'return/d (list value k))
    (begin
      (match
       k
       ((KDone) (Halted value))
       ((KProgram relations rest) (Call 'return/d (list `(program ,relations ,value) rest)))
       ((KConj right owners inherited rest)
        (Call 'bind/d (list value (GRight right) owners inherited rest)))
       ((KDisjLeft right state owners inherited rest)
        (define here (owners-support owners inherited))
        (Call
         'eval/d
         (list right state '(Owners) here (KDisjRight value owners inherited rest))))
       ((KDisjRight left owners inherited rest)
        (Call 'merge/d (list left value owners inherited rest)))
       ((KMergeYield owners answer rest)
        (Call 'return/d (list `(Yield ,owners ,answer ,value) rest)))
       ((KBindHead tail continue common inherited rest)
        (define here (owners-support common inherited))
        (Call
         'bind/d
         (list tail continue '(Owners) here (KBindTail value common inherited rest))))
       ((KBindTail head common inherited rest)
        (Call 'merge/d (list head value common inherited rest)))
       ((KMergeForced right owners inherited rest)
        (Call 'merge/d (list right value owners inherited rest)))
       ((KBindForced continue owners inherited rest)
        (Call 'bind/d (list value continue owners inherited rest)))
       ((KCommit rest) (Call 'commit/d (list value rest)))
       ((KCommitEmit owners answer rest)
        (Call 'return/d (list `(Emit ,owners ,answer ,value) rest)))
       ((KAdvanceEmit owners answer rest)
        (Call 'return/d (list `(Emit ,owners ,answer ,value) rest)))
       ((KAdvanceHistory owners rest) (Call 'return/d (list `(Forced ,owners ,value) rest)))
       ((KAdvanceForced owners rest) (Call 'return/d (list `(Forced ,owners ,value) rest)))
       ((KCollectEmit owners answer rest)
        (Call 'return/d (list `(Emit ,owners ,answer ,value) rest)))
       ((KCollectHistory owners rest) (Call 'return/d (list `(Forced ,owners ,value) rest)))
       ((KCollectResume owners here rest)
        (Call 'collect/d (list value here (KCollectForced owners rest))))
       ((KCollectForced owners rest) (Call 'return/d (list `(Forced ,owners ,value) rest))))))
   ((Halted _) #f)
   (_ (raise-argument-error 'step "derived S reference configuration" current))))
