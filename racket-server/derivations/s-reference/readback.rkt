#lang racket

(require "data.rkt" "relations.rkt"
         (only-in "../shared/kernel.rkt" owners-support valid-support?)
         (only-in "../shared/wf.rkt" wf-s? wf-s-rel?))

(provide reify-search reify-frontier reify-resumption reify-value
         reify-continuation continuation-prefix continuation-input-kind
         source-kind readback-call readback-halted valid-call? validate-program-data!)

;; Environments are checked from explicit control/continuation data. This is
;; a structural invariant check, not dynamic scope or an evaluator parameter.
(define (validate-program-data! datum [active-goal #f])
  (define boundaries '())
  (define pending '())
  (define (inventory value)
    (match value
      [(ProgramGoal relations _)
       (set! pending (cons relations pending))]
      [(KProgram relations rest)
       (set! boundaries (cons relations boundaries))
       (inventory rest)]
      [`(program ,relations ,body)
       (set! boundaries (cons relations boundaries))
       (inventory body)]
      [(cons first rest) (inventory first) (inventory rest)]
      [(? struct?)
       (for ([field (in-vector (struct->vector value))] [index (in-naturals)]
             #:unless (zero? index))
         (inventory field))]
      [_ (void)]))
  (inventory datum)
  (define expected (and (pair? boundaries) (first boundaries)))
  (unless (and (or (null? pending) expected)
               (andmap (lambda (relations) (equal? relations expected))
                       (append boundaries pending)))
    (error 'readback "pending relation environments disagree with their program boundary"))
  (define (check-goal goal)
    (unless (if expected
                (and (ProgramGoal? goal) (equal? (ProgramGoal-relations goal) expected))
                (not (ProgramGoal? goal)))
      (error 'readback "pending goal lost or changed its explicit relation environment: ~e" goal)))
  (define (check-sites value)
    (match value
      [(or (REval goal _) (GRight goal) (KConj goal _ _ _) (KDisjLeft goal _ _ _ _))
       (check-goal goal)
       (for ([field (in-vector (struct->vector value))] [index (in-naturals)]
             #:unless (zero? index))
         (check-sites field))]
      [(ProgramGoal _ _) (void)]
      [`(program ,_ ,body) (check-sites body)]
      [(cons first rest) (check-sites first) (check-sites rest)]
      [(? struct?)
       (for ([field (in-vector (struct->vector value))] [index (in-naturals)]
             #:unless (zero? index))
         (check-sites field))]
      [_ (void)]))
  (when active-goal (check-goal active-goal))
  (check-sites datum)
  expected)

;; Structural readback only: no reduction, decomposition, observer lookup, or
;; resumption execution occurs here. Resumptions receive their allocation
;; world at entry; continuation caches must equal their structural ancestry.
(define (check-prefix who captured structural)
  (unless (and (valid-support? captured) (equal? captured structural))
    (error who "captured support ~e disagrees with structural ancestry ~e"
           captured structural)))

(define (extend owners inherited)
  (define here (owners-support owners inherited))
  (unless (valid-support? here)
    (error 'readback "invalid structural allocation ancestry: ~e" here))
  here)

(define (continuation-goal continue)
  (match continue
    [(GRight goal) (goal-body goal)]
    [_ (raise-argument-error 'continuation-goal "GRight continuation" continue)]))

(define (reify-search search [inherited '()])
  (match search
    [`(Empty ,owners)
     (extend owners inherited)
     search]
    [`(One ,owners ,state)
     (extend owners inherited)
     search]
    [`(Yield ,owners (Answer ,answer-owners ,state) ,tail)
     (define here (extend owners inherited))
     (extend answer-owners here)
     `(Yield ,owners (Answer ,answer-owners ,state) ,(reify-search tail here))]
    [`(Delay ,owners ,resume)
     `(Delay ,owners ,(reify-resumption resume '(Owners) (extend owners inherited)))]
    [_ (raise-argument-error 'reify-search "defunctionalized active Search" search)]))

;; Root Owners are arguments of resumption entry, not a post-return wrapper.
;; This is the fieldwise image of that interface; lift-owners is not invoked.
(define (reify-resumption resume [owners '(Owners)] [inherited '()])
  (define here (extend owners inherited))
  (match resume
    [(REval goal state) `(eval ,owners ,(goal-body goal) ,state)]
    [(RMerge right left)
     `(mplus ,owners ,(reify-search right here)
             (force ,(reify-search left here)))]
    [(RBind inner continue)
     `(bind ,owners ,(reify-resumption inner '(Owners) here)
            ,(continuation-goal continue))]
    [_ (raise-argument-error 'reify-resumption "defunctionalized resumption" resume)]))

(define (reify-frontier frontier [inherited '()])
  (match frontier
    [`(program ,relations ,body) `(program ,relations ,(reify-frontier body inherited))]
    [`(Done ,owners)
     (extend owners inherited)
     frontier]
    [`(Solo ,owners ,_)
     (extend owners inherited)
     frontier]
    [`(Emit ,owners (Answer ,answer-owners ,state) ,tail)
     (define here (extend owners inherited))
     (extend answer-owners here)
     `(Emit ,owners (Answer ,answer-owners ,state) ,(reify-frontier tail here))]
    [`(Forced ,owners ,tail)
     `(Forced ,owners ,(reify-frontier tail (extend owners inherited)))]
    [`(More ,(and delay `(Delay ,_ ,_)))
     `(More ,(reify-search delay inherited))]
    [_ (raise-argument-error 'reify-frontier "defunctionalized settled Frontier" frontier)]))

(define (reify-value value [inherited '()])
  (match value
    [`(program ,_ ,_) (reify-frontier value inherited)]
    [`(More (Delay ,_ ,_)) (reify-frontier value inherited)]
    [`(,(or 'Empty 'One 'Yield 'Delay) ,_ ...) (reify-search value inherited)]
    [`(,(or 'Done 'Solo 'Emit 'Forced) ,_ ...) (reify-frontier value inherited)]
    [_ (raise-argument-error 'reify-value "defunctionalized Search or Frontier" value)]))

;; Read from the outermost continuation inward. Saved siblings and private
;; Answer introductions do not contribute to the active world's ancestry.
(define (continuation-prefix k [root '()])
  (match k
    [(KDone)
     (check-prefix 'KDone root root)
     root]
    [(KProgram _ rest) (continuation-prefix rest root)]
    [(or (KConj _ owners inherited rest)
         (KDisjLeft _ _ owners inherited rest)
         (KDisjRight _ owners inherited rest)
         (KBindHead _ _ owners inherited rest)
         (KBindTail _ owners inherited rest)
         (KMergeForced _ owners inherited rest)
         (KBindForced _ owners inherited rest))
     (define outside (continuation-prefix rest root))
     (check-prefix 'continuation-prefix inherited outside)
     (extend owners outside)]
    [(or (KMergeYield owners _ rest)
         (KCommitEmit owners _ rest)
         (KAdvanceEmit owners _ rest)
         (KAdvanceHistory owners rest)
         (KAdvanceForced owners rest)
         (KCollectEmit owners _ rest)
         (KCollectHistory owners rest)
         (KCollectForced owners rest))
     (extend owners (continuation-prefix rest root))]
    [(KCollectResume owners here rest)
     (define structural (extend owners (continuation-prefix rest root)))
     (check-prefix 'KCollectResume here structural)
     structural]
    [(KCommit rest) (continuation-prefix rest root)]
    [_ (raise-argument-error 'continuation-prefix "defunctionalized continuation" k)]))

(define (continuation-input-kind k)
  (match k
    [(or (KDone) (KProgram _ _) (KCommitEmit _ _ _)
         (KAdvanceEmit _ _ _) (KAdvanceHistory _ _) (KAdvanceForced _ _)
         (KCollectEmit _ _ _) (KCollectHistory _ _) (KCollectResume _ _ _)
         (KCollectForced _ _))
     'frontier]
    [(or (KCommit _)
         (KConj _ _ _ _) (KDisjLeft _ _ _ _ _) (KDisjRight _ _ _ _)
         (KMergeYield _ _ _) (KBindHead _ _ _ _ _) (KBindTail _ _ _ _)
         (KMergeForced _ _ _ _) (KBindForced _ _ _ _))
     'search]
    [_ (raise-argument-error 'continuation-input-kind "defunctionalized continuation" k)]))

(define (source-kind computation)
  (match computation
    [`(program ,_ ,body) (source-kind body)]
    [`(More (Delay ,_ ,_)) 'frontier]
    [`(,(or 'eval 'mplus 'bind 'force 'Empty 'One 'Yield 'Delay) ,_ ...) 'search]
    [`(,(or 'commit 'advance 'collect 'render 'Done 'Solo 'Emit 'Forced) ,_ ...) 'frontier]
    [_ (raise-argument-error 'source-kind "retained-scope computation" computation)]))

;; This whole-tree map is independent of the direct native-machine map.
(define (reify-continuation computation k [root '()])
  (continuation-prefix k root)
  (unless (equal? (source-kind computation) (continuation-input-kind k))
    (error 'reify-continuation "~e consumes ~e, received ~e image"
           k (continuation-input-kind k) (source-kind computation)))
  (match k
    [(KDone) computation]
    [(KProgram relations rest)
     (reify-continuation `(program ,relations ,computation) rest root)]
    [(KConj right owners _ rest)
     (reify-continuation `(bind ,owners ,computation ,(goal-body right)) rest root)]
    [(KDisjLeft right state owners _ rest)
     (reify-continuation
      `(mplus ,owners ,computation (eval (Owners) ,(goal-body right) ,state)) rest root)]
    [(KDisjRight left owners _ rest)
     (define here (extend owners (continuation-prefix rest root)))
     (reify-continuation `(mplus ,owners ,(reify-search left here) ,computation) rest root)]
    [(KMergeYield owners answer rest)
     (reify-continuation `(Yield ,owners ,answer ,computation) rest root)]
    [(KBindHead tail continue common _ rest)
     (define here (extend common (continuation-prefix rest root)))
     (reify-continuation
      `(mplus ,common ,computation
              (bind (Owners) ,(reify-search tail here) ,(continuation-goal continue)))
      rest root)]
    [(KBindTail head common _ rest)
     (define here (extend common (continuation-prefix rest root)))
     (reify-continuation `(mplus ,common ,(reify-search head here) ,computation) rest root)]
    [(KMergeForced right owners inherited rest)
     (reify-continuation
      `(mplus ,owners ,(reify-search right (extend owners inherited)) ,computation) rest root)]
    [(KBindForced continue owners _ rest)
     (reify-continuation `(bind ,owners ,computation ,(continuation-goal continue)) rest root)]
    [(KCommit rest) (reify-continuation `(commit ,computation) rest root)]
    [(or (KCommitEmit owners answer rest)
         (KAdvanceEmit owners answer rest) (KCollectEmit owners answer rest))
     (reify-continuation `(Emit ,owners ,answer ,computation) rest root)]
    [(KCollectResume owners _ rest)
     (reify-continuation `(Forced ,owners (collect ,computation)) rest root)]
    [(or (KAdvanceForced owners rest) (KAdvanceHistory owners rest)
         (KCollectHistory owners rest) (KCollectForced owners rest))
     (reify-continuation `(Forced ,owners ,computation) rest root)]))

(define (checked-readback computation)
  (unless (match computation
            [`(program ,_ ,_) (wf-s-rel? computation)]
            [_ (wf-s? computation)])
    (error 'readback "reification violates the S allocation contract: ~e" computation))
  computation)

;; Machine Call fields stay separate so readback does not import or run the
;; machine. Atomic outcome/handler states represent the already computed
;; result; dispatch does not evaluate the atomic goal a second time.
(define (readback-call pc operands)
  (validate-program-data! operands
                          (match (cons pc operands) [(list 'eval/d goal _ ...) goal] [_ #f]))
  (checked-readback
   (match (cons pc operands)
     [(list 'eval/d goal state owners inherited k)
      (check-prefix 'eval/d inherited (continuation-prefix k))
      (reify-continuation `(eval ,owners ,(goal-body goal) ,state) k)]
     [(list 'merge/d left right owners inherited k)
      (check-prefix 'merge/d inherited (continuation-prefix k))
      (define here (extend owners inherited))
      (reify-continuation
       `(mplus ,owners ,(reify-search left here) ,(reify-search right here)) k)]
     [(list 'bind/d search continue owners inherited k)
      (check-prefix 'bind/d inherited (continuation-prefix k))
      (reify-continuation
       `(bind ,owners ,(reify-search search (extend owners inherited))
              ,(continuation-goal continue)) k)]
     [(list 'continue/d continue state owners inherited k)
      (check-prefix 'continue/d inherited (continuation-prefix k))
      (reify-continuation `(eval ,owners ,(continuation-goal continue) ,state) k)]
     [(list 'resume/d resume owners inherited k)
      (check-prefix 'resume/d inherited (continuation-prefix k))
      (reify-continuation (reify-resumption resume owners inherited) k)]
     [(list 'force/d (and search `(Delay ,_ ,_)) inherited k)
      (check-prefix 'force/d inherited (continuation-prefix k))
      (reify-continuation `(force ,(reify-search search inherited)) k)]
     [(list 'commit/d search k)
      (reify-continuation `(commit ,(reify-search search (continuation-prefix k))) k)]
     [(list 'advance/d frontier inherited k)
      (check-prefix 'advance/d inherited (continuation-prefix k))
      (reify-continuation `(advance ,(reify-frontier frontier inherited)) k)]
     [(list 'collect/d frontier inherited k)
      (check-prefix 'collect/d inherited (continuation-prefix k))
      (reify-continuation `(collect ,(reify-frontier frontier inherited)) k)]
     [(list 'return/d value k)
      (reify-continuation (reify-value value (continuation-prefix k)) k)]
     [(list 'outcome/d outcome (FEmpty owners k) (SOne owners* k*))
      (unless (and (equal? owners owners*) (equal? k k*))
        (error 'readback "outcome consumers disagree on Owners or continuation"))
      (reify-continuation
       (match outcome [(Failure) `(Empty ,owners)] [(Success state) `(One ,owners ,state)]) k)]
     [(list 'failure/d (FEmpty owners k))
      (reify-continuation `(Empty ,owners) k)]
     [(list 'success/d (SOne owners k) state)
      (reify-continuation `(One ,owners ,state) k)]
     [_ (raise-arguments-error 'readback-call "unknown generated machine call"
                               "pc" pc "operands" operands)])))

(define (readback-halted value)
  (validate-program-data! value)
  (checked-readback (reify-frontier value)))

(define (valid-call? pc operands)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (readback-call pc operands)
    #t))
