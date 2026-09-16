#lang racket

(require "kernel.rkt")

(provide Q-SE Q-EN Q-SN state-support common-support world-supports)

(define (state-support state)
  (match state
    [`(state (Support ,support ...) ,_ ,_ ,_ ,_)
     (unless (valid-support? support)
       (raise-argument-error 'state-support "valid E state support" state))
     support]
    [_ (raise-argument-error 'state-support "E state" state)]))

(define (state-SE state support)
  (unless (valid-support? support)
    (raise-argument-error 'Q-SE "duplicate-free world path" support))
  (match state
    [`(state ,sub ,dis ,trail ,tag) `(state (Support ,@support) ,sub ,dis ,trail ,tag)]))

;; S -> E traverses local Owner groups with the exact current-world prefix.
;; Each branch receives its own copy of that prefix. Answer-only introductions
;; never enter the residual branch, and failures retain just their Support.
(define (Q-SE computation [prefix '()])
  (match computation
    [`(program ,definitions ,body) `(program ,definitions ,(Q-SE body prefix))]
    [`(eval ,owners ,goal ,state)
     `(eval ,goal ,(state-SE state (owners-support owners prefix)))]
    [`(mplus ,owners ,left ,right)
     (define here (owners-support owners prefix))
     `(mplus ,(Q-SE left here) ,(Q-SE right here))]
    [`(bind ,owners ,search ,goal)
     `(bind ,(Q-SE search (owners-support owners prefix)) ,goal)]
    [`(Empty ,owners) `(Empty (Support ,@(owners-support owners prefix)))]
    [`(One ,owners ,state) `(One ,(state-SE state (owners-support owners prefix)))]
    [`(Yield ,owners (Answer ,answer-owners ,state) ,tail)
     (define here (owners-support owners prefix))
     `(Yield ,(state-SE state (owners-support answer-owners here)) ,(Q-SE tail here))]
    [`(Delay ,owners ,body)
     `(Delay ,(Q-SE body (owners-support owners prefix)))]
    [`(,(and constructor (or 'force 'render 'commit 'advance 'collect 'More)) ,body)
     `(,constructor ,(Q-SE body prefix))]
    [`(Done ,owners) `(Done (Support ,@(owners-support owners prefix)))]
    [`(Solo ,owners ,state)
     `(Solo ,(state-SE state (owners-support owners prefix)))]
    [`(Emit ,owners (Answer ,answer-owners ,state) ,tail)
     (define here (owners-support owners prefix))
     `(Emit ,(state-SE state (owners-support answer-owners here)) ,(Q-SE tail here))]
    [`(Forced ,owners ,tail) `(Forced ,(Q-SE tail (owners-support owners prefix)))]
    [_ (raise-argument-error 'Q-SE "strict S computation/observation" computation)]))

(define (world-supports computation)
  (match computation
    [`(program ,_ ,body) (world-supports body)]
    [`(eval ,_ ,state) (list (state-support state))]
    [`(mplus ,left ,right) (append (world-supports left) (world-supports right))]
    [`(bind ,search ,_) (world-supports search)]
    [`(,(or 'Empty 'Done) (Support ,support ...)) (list support)]
    [`(,(or 'One 'Solo) ,state) (list (state-support state))]
    [`(,(or 'Yield 'Emit) ,state ,tail) (cons (state-support state) (world-supports tail))]
    [`(,(or 'Delay 'force 'render 'commit 'advance 'collect 'Forced 'More) ,body)
     (world-supports body)]
    [_ (raise-argument-error 'world-supports "strict E computation/observation" computation)]))

(define (shared-prefix left right)
  (match* (left right)
    [((cons a d) (cons b e))
     (if (equal? a b) (cons a (shared-prefix d e)) '())]
    [(_ _) '()]))

(define (common-support computation)
  (match (world-supports computation)
    [(cons first rest) (foldl shared-prefix first rest)]
    ['() '()]))

;; E -> N addresses each world by its own ordered support. A future bind goal
;; may refer only to a prefix common to all worlds reaching it. address-goal
;; rejects a reference outside that prefix, rather than guessing an address.
(define (Q-EN computation)
  (match computation
    [`(program ,definitions ,body) `(program ,definitions ,(Q-EN body))]
    [`(eval ,goal ,state)
     (define support (state-support state))
     `(eval ,(address-goal goal support) ,(address-state state support))]
    [`(mplus ,left ,right) `(mplus ,(Q-EN left) ,(Q-EN right))]
    [`(bind ,search ,goal)
     `(bind ,(Q-EN search) ,(address-goal goal (common-support search)))]
    [`(,(and constructor (or 'Empty 'Done)) (Support ,support ...))
     (unless (valid-support? support)
       (raise-argument-error 'Q-EN "valid failure support" support))
     `(,constructor ,(length support))]
    [`(,(and constructor (or 'One 'Solo)) ,state)
     `(,constructor ,(address-state state (state-support state)))]
    [`(,(and constructor (or 'Yield 'Emit)) ,state ,tail)
     `(,constructor ,(address-state state (state-support state)) ,(Q-EN tail))]
    [`(,(and constructor (or 'Delay 'force 'render 'commit 'advance 'collect 'Forced 'More)) ,body)
     `(,constructor ,(Q-EN body))]
    [_ (raise-argument-error 'Q-EN "strict E computation/observation" computation)]))

(define (state-SN state support)
  (match-define `(state ,sub ,dis ,trail ,tag) state)
  (address-state `(state (Support ,@support) ,sub ,dis ,trail ,tag) support))

;; Independently direct S -> N. This traversal does not invoke Q-SE or Q-EN,
;; so composition tests compare distinct implementations of the vertical edge.
(define (Q-SN computation [prefix '()])
  (match computation
    [`(program ,definitions ,body) `(program ,definitions ,(Q-SN body prefix))]
    [`(eval ,owners ,goal ,state)
     (define here (owners-support owners prefix))
     `(eval ,(address-goal goal here) ,(state-SN state here))]
    [`(mplus ,owners ,left ,right)
     (define here (owners-support owners prefix))
     `(mplus ,(Q-SN left here) ,(Q-SN right here))]
    [`(bind ,owners ,search ,goal)
     (define here (owners-support owners prefix))
     `(bind ,(Q-SN search here) ,(address-goal goal here))]
    [`(,(and constructor (or 'Empty 'Done)) ,owners)
     (define here (owners-support owners prefix))
     (unless (valid-support? here)
       (raise-argument-error 'Q-SN "duplicate-free world path" here))
     `(,constructor ,(length here))]
    [`(One ,owners ,state) `(One ,(state-SN state (owners-support owners prefix)))]
    [`(Yield ,owners (Answer ,answer-owners ,state) ,tail)
     (define here (owners-support owners prefix))
     `(Yield ,(state-SN state (owners-support answer-owners here)) ,(Q-SN tail here))]
    [`(Delay ,owners ,body)
     `(Delay ,(Q-SN body (owners-support owners prefix)))]
    [`(,(and constructor (or 'force 'render 'commit 'advance 'collect 'More)) ,body)
     `(,constructor ,(Q-SN body prefix))]
    [`(Solo ,owners ,state)
     `(Solo ,(state-SN state (owners-support owners prefix)))]
    [`(Emit ,owners (Answer ,answer-owners ,state) ,tail)
     (define here (owners-support owners prefix))
     `(Emit ,(state-SN state (owners-support answer-owners here)) ,(Q-SN tail here))]
    [`(Forced ,owners ,tail) `(Forced ,(Q-SN tail (owners-support owners prefix)))]
    [_ (raise-argument-error 'Q-SN "strict S computation/observation" computation)]))
