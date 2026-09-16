#lang racket

(require "data.rkt" "relations.rkt"
         (prefix-in f: "machine.rkt")
         (prefix-in a: "../shared/stages/schema.rkt")
         (only-in "../shared/kernel.rkt" owners-support)
         (only-in "readback.rkt"
                  reify-search reify-frontier reify-resumption reify-value
                  continuation-prefix continuation-input-kind source-kind validate-program-data!))

(provide functional->M functional-step-label)

;; Write native M control and Frame/K fields directly. This map does not plug
;; a whole tree, decompose it, normalize a state, invoke a transition, or run a
;; resumption. Structural leaf maps only expose data saved in individual fields.
(define (goal-of continue)
  (match continue [(GRight goal) (goal-body goal)]))

(define (check-inherited who inherited k)
  (define structural (continuation-prefix k))
  (unless (equal? inherited structural)
    (error who "inherited ancestry ~e differs from continuation ancestry ~e"
           inherited structural)))

(define (frame kind before [after '()] [owners #f])
  (a:Frame kind before after owners))

;; Frames are outermost first while the continuation is assembled. Each
;; recursive step checks the result kind accepted by its enclosing frame.
;; Root ownership at resumption entry becomes an ordinary merge/bind frame.
(define (attach control frames k kind)
  (unless (equal? kind (continuation-input-kind k))
    (error 'functional->M "~e consumes ~e, received ~e"
           k (continuation-input-kind k) kind))
  (match k
    [(KDone) (a:M control (foldl a:K 'halt frames))]
    [(KProgram relations rest)
     (attach control (cons (frame 'program `(program ,relations)) frames) rest 'frontier)]
    [(KConj right owners _ rest)
     (attach control (cons (frame 'bind `(bind ,owners) (list (goal-body right)) owners) frames)
             rest 'search)]
    [(KDisjLeft right state owners _ rest)
     (attach control
             (cons (frame 'merge-left `(mplus ,owners)
                          (list `(eval (Owners) ,(goal-body right) ,state)) owners) frames)
             rest 'search)]
    [(KDisjRight left owners _ rest)
     (define here (owners-support owners (continuation-prefix rest)))
     (attach control
             (cons (frame 'merge-right `(mplus ,owners ,(reify-search left here))
                          '() owners) frames)
             rest 'search)]
    [(KMergeYield owners answer rest)
     (attach control (cons (frame 'yield `(Yield ,owners ,answer) '() owners) frames)
             rest 'search)]
    [(KBindHead tail continue common _ rest)
     (define here (owners-support common (continuation-prefix rest)))
     (attach control
             (cons (frame 'merge-left `(mplus ,common)
                          (list `(bind (Owners) ,(reify-search tail here) ,(goal-of continue)))
                          common) frames)
             rest 'search)]
    [(KBindTail head common _ rest)
     (define here (owners-support common (continuation-prefix rest)))
     (attach control
             (cons (frame 'merge-right `(mplus ,common ,(reify-search head here))
                          '() common) frames)
             rest 'search)]
    [(KMergeForced right owners inherited rest)
     (define here (owners-support owners inherited))
     (attach control
             (cons (frame 'merge-right `(mplus ,owners ,(reify-search right here))
                          '() owners) frames)
             rest 'search)]
    [(KBindForced continue owners _ rest)
     (attach control
             (cons (frame 'bind `(bind ,owners) (list (goal-of continue)) owners) frames)
             rest 'search)]
    [(KCommit rest)
     (attach control (cons (frame 'commit '(commit)) frames) rest 'frontier)]
    [(or (KCommitEmit owners answer rest)
         (KAdvanceEmit owners answer rest) (KCollectEmit owners answer rest))
     (attach control (cons (frame 'emit `(Emit ,owners ,answer) '() owners) frames)
             rest 'frontier)]
    [(KCollectResume owners _ rest)
     (attach control
             (cons (frame 'forced `(Forced ,owners) '() owners)
                   (cons (frame 'collect '(collect)) frames))
             rest 'frontier)]
    [(or (KAdvanceForced owners rest) (KAdvanceHistory owners rest)
         (KCollectHistory owners rest) (KCollectForced owners rest))
     (attach control (cons (frame 'forced `(Forced ,owners) '() owners) frames)
             rest 'frontier)]))

(define (with-continuation control k)
  (continuation-prefix k)
  (attach control '() k (source-kind control)))

;; Classify a functional transition before stepping it. A string specifies
;; exactly one source contraction. #f specifies context/closure/handler
;; administration. No label is inferred by looking for a matching successor.
(define (functional-step-label configuration)
  (match configuration
    [(f:Call 'eval/d (list goal _ _ _ _))
     (match (goal-body goal)
       [`(∃ ,_ ,_ ,_) "allocate-fresh"]
       [`(,_ ∧ ,_ ,_) "eval-conj"]
       [`(,_ ∨ ,_ ,_) "eval-disj"]
       [`(suspend ,_ ,_) "eval-suspend"]
       [(? relation-call?) "eval-call"]
       [_ "eval-atom"])]
    [(f:Call 'merge/d (list search _ _ _ _))
     (match search
       [`(Empty ,_) "mplus-empty"]
       [`(One ,_ ,_) "mplus-one"]
       [`(Yield ,_ ,_ ,_) "mplus-yield"]
       [`(Delay ,_ ,_) "mplus-delay"])]
    [(f:Call 'bind/d (list search _ _ _ _))
     (match search
       [`(Empty ,_) "bind-empty"]
       [`(One ,_ ,_) "bind-one"]
       [`(Yield ,_ ,_ ,_) "bind-yield"]
       [`(Delay ,_ ,_) "bind-delay"])]
    [(f:Call 'force/d _) "force-delay"]
    [(f:Call 'commit/d (list search _))
     (match search
       [`(Empty ,_) "commit-empty"]
       [`(One ,_ ,_) "commit-one"]
       [`(Yield ,_ ,_ ,_) "commit-yield"]
       [`(Delay ,_ ,_) "commit-delay"])]
    [(f:Call 'advance/d (list frontier _ _))
     (match frontier
       [`(Done ,_) "advance-done"]
       [`(Solo ,_ ,_) "advance-solo"]
       [`(Emit ,_ ,_ ,_) "advance-emit"]
       [`(Forced ,_ ,_) "advance-forced"]
       [`(More ,_) "advance-delay"])]
    [(f:Call 'collect/d (list frontier _ _))
     (match frontier
       [`(Done ,_) "collect-done"]
       [`(Solo ,_ ,_) "collect-solo"]
       [`(Emit ,_ ,_ ,_) "collect-emit"]
       [`(Forced ,_ ,_) "collect-forced"]
       [`(More ,_) "collect-delay"])]
    [(f:Call (or 'return/d 'continue/d 'resume/d 'outcome/d 'failure/d 'success/d) _) #f]
    [(f:Halted _) #f]
    [_ (raise-argument-error 'functional-step-label "functional machine configuration" configuration)]))

(define (functional->M configuration)
  (match configuration
    [(f:Call 'eval/d (list goal _ ...))
     (validate-program-data! configuration goal)]
    [_ (validate-program-data! configuration)])
  (match configuration
    [(f:Halted value) (a:M (reify-frontier value) 'halt)]
    [(f:Call 'eval/d (list goal state owners inherited k))
     (check-inherited 'eval/d inherited k)
     (with-continuation `(eval ,owners ,(goal-body goal) ,state) k)]
    [(f:Call 'merge/d (list left right owners inherited k))
     (check-inherited 'merge/d inherited k)
     (define here (owners-support owners inherited))
     (with-continuation `(mplus ,owners ,(reify-search left here) ,(reify-search right here)) k)]
    [(f:Call 'bind/d (list search continue owners inherited k))
     (check-inherited 'bind/d inherited k)
     (with-continuation
      `(bind ,owners ,(reify-search search (owners-support owners inherited))
             ,(goal-of continue)) k)]
    [(f:Call 'continue/d (list continue state owners inherited k))
     (check-inherited 'continue/d inherited k)
     (with-continuation `(eval ,owners ,(goal-of continue) ,state) k)]
    [(f:Call 'resume/d (list resume owners inherited k))
     (check-inherited 'resume/d inherited k)
     (with-continuation (reify-resumption resume owners inherited) k)]
    [(f:Call 'force/d (list (and search `(Delay ,_ ,_)) inherited k))
     (check-inherited 'force/d inherited k)
     (with-continuation `(force ,(reify-search search inherited)) k)]
    [(f:Call 'commit/d (list search k))
     (with-continuation `(commit ,(reify-search search (continuation-prefix k))) k)]
    [(f:Call 'advance/d (list frontier inherited k))
     (check-inherited 'advance/d inherited k)
     (with-continuation `(advance ,(reify-frontier frontier inherited)) k)]
    [(f:Call 'collect/d (list frontier inherited k))
     (check-inherited 'collect/d inherited k)
     (with-continuation `(collect ,(reify-frontier frontier inherited)) k)]
    [(f:Call 'return/d (list value k))
     (with-continuation (reify-value value (continuation-prefix k)) k)]
    [(f:Call 'outcome/d (list outcome (FEmpty owners k) (SOne owners* k*)))
     (unless (and (equal? owners owners*) (equal? k k*))
       (error 'functional->M "outcome handlers disagree"))
     (with-continuation
      (match outcome [(Failure) `(Empty ,owners)] [(Success state) `(One ,owners ,state)]) k)]
    [(f:Call 'failure/d (list (FEmpty owners k)))
     (with-continuation `(Empty ,owners) k)]
    [(f:Call 'success/d (list (SOne owners k) state))
     (with-continuation `(One ,owners ,state) k)]
    [_ (raise-argument-error 'functional->M "functional machine configuration" configuration)]))
