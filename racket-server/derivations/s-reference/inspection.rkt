#lang racket

(require "relations.rkt")

(provide record-closure! reify-search reify-frontier reify-resumption)

;; Test/readback support only. Actual interpreters retain procedures, not these
;; descriptions. The observer records closure creation without calling a
;; resumption. Reading an unfinished Frontier must not run its delayed work.
(define (record-closure! descriptions family procedure captures)
  (hash-set! descriptions procedure (cons family captures)))

(define (description descriptions procedure)
  (hash-ref descriptions procedure
            (lambda () (error 'readback "closure was not observed at creation"))))

(define (reify-continuation-goal descriptions continue)
  (match (description descriptions continue)
    [(list 'continue-goal goal) (goal-body goal)]
    [other (error 'readback "expected a goal continuation, received ~e" other)]))

(define (reify-resumption descriptions resume [owners '(Owners)])
  (match (description descriptions resume)
    [(list 'resume-eval goal state) `(eval ,owners ,(goal-body goal) ,state)]
    [(list 'resume-merge right left)
     `(mplus ,owners ,(reify-search descriptions right)
             (force ,(reify-search descriptions left)))]
    [(list 'resume-bind inner continue)
     `(bind ,owners ,(reify-resumption descriptions inner)
            ,(reify-continuation-goal descriptions continue))]
    [other (error 'readback "expected an actual Delay resumption, received ~e" other)]))

(define (reify-search descriptions search)
  (match search
    [`(Empty ,_) search]
    [`(One ,_ ,_) search]
    [`(Yield ,owners ,answer ,tail)
     `(Yield ,owners ,answer ,(reify-search descriptions tail))]
    [`(Delay ,owners ,resume)
     `(Delay ,owners ,(reify-resumption descriptions resume))]
    [_ (raise-argument-error 'reify-search "active retained-scope Search" search)]))

(define (reify-frontier descriptions frontier)
  (match frontier
    [`(program ,relations ,body)
     `(program ,relations ,(reify-frontier descriptions body))]
    [`(Done ,_) frontier]
    [`(Solo ,_ ,_) frontier]
    [`(Emit ,owners ,answer ,tail)
     `(Emit ,owners ,answer ,(reify-frontier descriptions tail))]
    [`(Forced ,owners ,tail)
     `(Forced ,owners ,(reify-frontier descriptions tail))]
    [`(More ,search) `(More ,(reify-search descriptions search))]
    [_ (raise-argument-error 'reify-frontier "retained-scope Frontier" frontier)]))
