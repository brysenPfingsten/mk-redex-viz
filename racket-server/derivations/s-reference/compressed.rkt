#lang racket
;; Generated from defunc.rkt via the checked atomic-handler compression in compression-derive.rkt. Regenerate; do not edit.
(require (only-in
          "../shared/kernel.rkt"
          owners-support
          owners-append
          fresh-names
          substitute-goal)
         "data.rkt"
         "relations.rkt"
         "../shared/runtime.rkt")

(require (prefix-in m: "machine.rkt"))

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
    (return/d value k)))

(struct Registers (pc r0 r1 r2 r3 r4 steps) #:mutable #:transparent)

(define (jump! bank pc r0 (r1 #f) (r2 #f) (r3 #f) (r4 #f))
  (set-Registers-r0! bank r0)
  (set-Registers-r1! bank r1)
  (set-Registers-r2! bank r2)
  (set-Registers-r3! bank r3)
  (set-Registers-r4! bank r4)
  (set-Registers-pc! bank pc))

(define (halt! bank value) (jump! bank 'halt value))

(define (operand-count pc)
  (match
   pc
   ('halt 1)
   (_
    (match
     (assq pc signatures)
     ((cons _ parameters) (length parameters))
     (#f (raise-argument-error 'operand-count "known program counter" pc))))))

(define (register-values bank)
  (list
   (Registers-r0 bank)
   (Registers-r1 bank)
   (Registers-r2 bank)
   (Registers-r3 bank)
   (Registers-r4 bank)))

(define (validate-bank! bank)
  (unless (Registers? bank) (raise-argument-error 'decode "Registers?" bank))
  (define arity (operand-count (Registers-pc bank)))
  (unless (and
           (exact-nonnegative-integer? (Registers-steps bank))
           (andmap not (drop (register-values bank) arity)))
    (raise-argument-error 'decode "canonical register configuration" bank))
  arity)

(define (decode bank)
  (define arity (validate-bank! bank))
  (match
   (Registers-pc bank)
   ('halt (m:Halted (Registers-r0 bank)))
   (pc (m:Call pc (take (register-values bank) arity)))))

(define (from-machine current)
  (match
   current
   ((m:Halted value) (Registers 'halt value #f #f #f #f 0))
   ((m:Call pc operands)
    (unless (and (not (eq? pc 'halt)) (list? operands) (= (length operands) (operand-count pc)))
      (raise-argument-error 'from-machine "Call with exact program arity" current))
    (define bank (Registers 'halt #f #f #f #f #f 0))
    (apply jump! bank pc operands)
    bank)
   (_ (raise-argument-error 'from-machine "Call or Halted" current))))

(define (initial
         goal
         #:owners
         (owners '(Owners))
         #:state
         (state '(state () () () (label "initial")))
         #:relations
         (relations #f))
  (Registers
   'eval/d
   (retain-goal relations goal)
   state
   owners
   '()
   (KCommit (if relations (KProgram relations (KDone)) (KDone)))
   0))

(define (step! bank)
  (validate-bank! bank)
  (match
   (Registers-pc bank)
   ('halt #f)
   (_ (dispatch! bank) (set-Registers-steps! bank (add1 (Registers-steps bank))) #t)))

(define (drive/steps! bank fuel)
  (match
   (Registers-pc bank)
   ('halt (Registers-r0 bank))
   (_
    (when (zero? fuel) (exhausted 's-reference-registers (decode bank)))
    (step! bank)
    (drive/steps! bank (sub1 fuel)))))

(define (drive! bank #:fuel (fuel 100000))
  (check-fuel fuel)
  (validate-bank! bank)
  (drive/steps! bank fuel))

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
  (drive! (initial goal #:owners owners #:state state #:relations relations) #:fuel fuel))

(define (resume-once frontier #:fuel (fuel 100000))
  (match
   frontier
   (`(program ,relations ,body)
    (drive! (Registers 'advance/d body '() (KProgram relations (KDone)) #f #f 0) #:fuel fuel))
   (_ (drive! (Registers 'advance/d frontier '() (KDone) #f #f 0) #:fuel fuel))))

(define (collect-all frontier #:fuel (fuel 100000))
  (match
   frontier
   (`(program ,relations ,body)
    (drive! (Registers 'collect/d body '() (KProgram relations (KDone)) #f #f 0) #:fuel fuel))
   (_ (drive! (Registers 'collect/d frontier '() (KDone) #f #f 0) #:fuel fuel))))

(define (dispatch! bank)
  (match
   (Registers-pc bank)
   ('eval/d
    (define goal (Registers-r0 bank))
    (define state (Registers-r1 bank))
    (define owners (Registers-r2 bank))
    (define inherited (Registers-r3 bank))
    (define k (Registers-r4 bank))
    (begin
      (define relations (goal-relations goal))
      (match
       (goal-body goal)
       (`(∃ ,binders ,body ,tag)
        (define intro (fresh-names (owners-support owners inherited) (length binders)))
        (jump!
         bank
         'eval/d
         (retain-goal relations (substitute-goal body (map list binders intro)))
         state
         (owners-append owners `(Owners (Owner ,intro ,tag)))
         inherited
         k))
       (`(,left ∧ ,right ,_)
        (define here (owners-support owners inherited))
        (jump!
         bank
         'eval/d
         (retain-goal relations left)
         state
         '(Owners)
         here
         (KConj (retain-goal relations right) owners inherited k)))
       (`(,left ∨ ,right ,_)
        (define here (owners-support owners inherited))
        (jump!
         bank
         'eval/d
         (retain-goal relations left)
         state
         '(Owners)
         here
         (KDisjLeft (retain-goal relations right) state owners inherited k)))
       (`(suspend ,body ,_)
        (jump! bank 'return/d `(Delay ,owners ,(REval (retain-goal relations body) state)) k))
       ((? relation-call? call)
        (jump!
         bank
         'eval/d
         (retain-goal relations (expand-call relations call))
         state
         owners
         inherited
         k))
       (atom
        (define outcome (atomic/data atom state))
        (match
         outcome
         ((Failure) (jump! bank 'return/d `(Empty ,owners) k))
         ((Success result-state) (jump! bank 'return/d `(One ,owners ,result-state) k)))))))
   ('merge/d
    (define left (Registers-r0 bank))
    (define right (Registers-r1 bank))
    (define owners (Registers-r2 bank))
    (define inherited (Registers-r3 bank))
    (define k (Registers-r4 bank))
    (begin
      (match
       left
       (`(Empty ,_) (jump! bank 'return/d (prefix owners right) k))
       (`(One ,local ,state)
        (jump! bank 'return/d `(Yield ,owners (Answer ,local ,state) ,right) k))
       (`(Yield ,local (Answer ,answer ,state) ,rest)
        (define answer* `(Answer ,(owners-append local answer) ,state))
        (jump!
         bank
         'merge/d
         (prefix local rest)
         right
         '(Owners)
         (owners-support owners inherited)
         (KMergeYield owners answer* k)))
       (`(Delay ,_ ,_) (jump! bank 'return/d `(Delay ,owners ,(RMerge right left)) k)))))
   ('bind/d
    (define search (Registers-r0 bank))
    (define continue (Registers-r1 bank))
    (define owners (Registers-r2 bank))
    (define inherited (Registers-r3 bank))
    (define k (Registers-r4 bank))
    (begin
      (match
       search
       (`(Empty ,local) (jump! bank 'return/d `(Empty ,(owners-append owners local)) k))
       (`(One ,local ,state)
        (jump! bank 'continue/d continue state (owners-append owners local) inherited k))
       (`(Yield ,local (Answer ,answer ,state) ,rest)
        (define common (owners-append owners local))
        (define here (owners-support common inherited))
        (jump!
         bank
         'continue/d
         continue
         state
         answer
         here
         (KBindHead rest continue common inherited k)))
       (`(Delay ,local ,resume)
        (define common (owners-append owners local))
        (jump! bank 'return/d `(Delay ,common ,(RBind resume continue)) k)))))
   ('continue/d
    (define continue (Registers-r0 bank))
    (define state (Registers-r1 bank))
    (define owners (Registers-r2 bank))
    (define inherited (Registers-r3 bank))
    (define k (Registers-r4 bank))
    (begin (match continue ((GRight goal) (jump! bank 'eval/d goal state owners inherited k)))))
   ('resume/d
    (define resume (Registers-r0 bank))
    (define owners (Registers-r1 bank))
    (define inherited (Registers-r2 bank))
    (define k (Registers-r3 bank))
    (begin
      (match
       resume
       ((REval goal state) (jump! bank 'eval/d goal state owners inherited k))
       ((RMerge right left)
        (jump!
         bank
         'force/d
         left
         (owners-support owners inherited)
         (KMergeForced right owners inherited k)))
       ((RBind rest continue)
        (jump!
         bank
         'resume/d
         rest
         '(Owners)
         (owners-support owners inherited)
         (KBindForced continue owners inherited k))))))
   ('force/d
    (define search (Registers-r0 bank))
    (define inherited (Registers-r1 bank))
    (define k (Registers-r2 bank))
    (begin
      (match
       search
       (`(Delay ,owners ,resume) (jump! bank 'resume/d resume owners inherited k)))))
   ('commit/d
    (define search (Registers-r0 bank))
    (define k (Registers-r1 bank))
    (begin
      (match
       search
       (`(Empty ,owners) (jump! bank 'return/d `(Done ,owners) k))
       (`(One ,owners ,state) (jump! bank 'return/d `(Solo ,owners ,state) k))
       (`(Yield ,owners ,answer ,rest)
        (jump! bank 'commit/d rest (KCommitEmit owners answer k)))
       (`(Delay ,_ ,_) (jump! bank 'return/d `(More ,search) k)))))
   ('advance/d
    (define frontier (Registers-r0 bank))
    (define inherited (Registers-r1 bank))
    (define k (Registers-r2 bank))
    (begin
      (match
       frontier
       (`(Done ,_) (jump! bank 'return/d frontier k))
       (`(Solo ,_ ,_) (jump! bank 'return/d frontier k))
       (`(Emit ,owners ,answer ,rest)
        (jump!
         bank
         'advance/d
         rest
         (owners-support owners inherited)
         (KAdvanceEmit owners answer k)))
       (`(Forced ,owners ,rest)
        (jump!
         bank
         'advance/d
         rest
         (owners-support owners inherited)
         (KAdvanceHistory owners k)))
       (`(More (Delay ,owners ,resume))
        (jump!
         bank
         'resume/d
         resume
         '(Owners)
         (owners-support owners inherited)
         (KCommit (KAdvanceForced owners k)))))))
   ('collect/d
    (define frontier (Registers-r0 bank))
    (define inherited (Registers-r1 bank))
    (define k (Registers-r2 bank))
    (begin
      (match
       frontier
       (`(Done ,_) (jump! bank 'return/d frontier k))
       (`(Solo ,_ ,_) (jump! bank 'return/d frontier k))
       (`(Emit ,owners ,answer ,rest)
        (jump!
         bank
         'collect/d
         rest
         (owners-support owners inherited)
         (KCollectEmit owners answer k)))
       (`(Forced ,owners ,rest)
        (jump!
         bank
         'collect/d
         rest
         (owners-support owners inherited)
         (KCollectHistory owners k)))
       (`(More (Delay ,owners ,resume))
        (define here (owners-support owners inherited))
        (jump!
         bank
         'resume/d
         resume
         '(Owners)
         here
         (KCommit (KCollectResume owners here k)))))))
   ('return/d
    (define value (Registers-r0 bank))
    (define k (Registers-r1 bank))
    (begin
      (match
       k
       ((KDone) (halt! bank value))
       ((KProgram relations rest) (jump! bank 'return/d `(program ,relations ,value) rest))
       ((KConj right owners inherited rest)
        (jump! bank 'bind/d value (GRight right) owners inherited rest))
       ((KDisjLeft right state owners inherited rest)
        (define here (owners-support owners inherited))
        (jump!
         bank
         'eval/d
         right
         state
         '(Owners)
         here
         (KDisjRight value owners inherited rest)))
       ((KDisjRight left owners inherited rest)
        (jump! bank 'merge/d left value owners inherited rest))
       ((KMergeYield owners answer rest)
        (jump! bank 'return/d `(Yield ,owners ,answer ,value) rest))
       ((KBindHead tail continue common inherited rest)
        (define here (owners-support common inherited))
        (jump!
         bank
         'bind/d
         tail
         continue
         '(Owners)
         here
         (KBindTail value common inherited rest)))
       ((KBindTail head common inherited rest)
        (jump! bank 'merge/d head value common inherited rest))
       ((KMergeForced right owners inherited rest)
        (jump! bank 'merge/d right value owners inherited rest))
       ((KBindForced continue owners inherited rest)
        (jump! bank 'bind/d value continue owners inherited rest))
       ((KCommit rest) (jump! bank 'commit/d value rest))
       ((KCommitEmit owners answer rest)
        (jump! bank 'return/d `(Emit ,owners ,answer ,value) rest))
       ((KAdvanceEmit owners answer rest)
        (jump! bank 'return/d `(Emit ,owners ,answer ,value) rest))
       ((KAdvanceHistory owners rest) (jump! bank 'return/d `(Forced ,owners ,value) rest))
       ((KAdvanceForced owners rest) (jump! bank 'return/d `(Forced ,owners ,value) rest))
       ((KCollectEmit owners answer rest)
        (jump! bank 'return/d `(Emit ,owners ,answer ,value) rest))
       ((KCollectHistory owners rest) (jump! bank 'return/d `(Forced ,owners ,value) rest))
       ((KCollectResume owners here rest)
        (jump! bank 'collect/d value here (KCollectForced owners rest)))
       ((KCollectForced owners rest) (jump! bank 'return/d `(Forced ,owners ,value) rest)))))
   (other (raise-argument-error 'dispatch! "active program counter" other))))
