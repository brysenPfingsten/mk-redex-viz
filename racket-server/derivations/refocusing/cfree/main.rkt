#lang racket

(require "../corpus.rkt"
         "../shared/configs.rkt"
         "../bridge/main.rkt")

(provide parse-example
         step
         trace
         final?
         answers
         (struct-out cfree-config))

(define (make-state)
  '(state () () () (label "s")))

(define (parse-example label)
  (define-values (query-u* goal)
    (instantiate-program label))
  (cfree-config query-u*
                query-u*
                `(,goal ,(make-state))))

(define (step cfg)
  (match (current-step (cfree->current-c-machine cfg))
    [#f
     '()]
    [(list name next-current)
     (list (list name (erase-c next-current)))]))

(define (trace cfg [limit 128] [steps '()])
  (match (step cfg)
    ['()
     (values (reverse steps)
             cfg
             (if (final? cfg) 'value 'stuck))]
    [_ #:when (zero? limit)
       (values (reverse steps) cfg 'cap)]
    [(list (list name next))
     (trace next
            (sub1 limit)
            (cons name steps))]
    [_ (values (reverse steps) cfg 'nondeterministic)]))

(define (final? cfg)
  (not (current-step (cfree->current-c-machine cfg))))

(define (answers cfg)
  (project-observable (cfree->current-c-machine cfg)))
