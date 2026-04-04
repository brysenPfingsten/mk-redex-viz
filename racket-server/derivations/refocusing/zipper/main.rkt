#lang racket

(require (prefix-in premachine: "../premachine/main.rkt"))

(provide parse-example
         cfg->machine
         machine->cfg
         step
         trace
         final?
         answers
         machine-query
         machine-root-scope
         machine-focus
         machine-ctx
         machine-obs)

(define (machine-query machine)
  (match machine
    [`(machine ,query-u* ,_root-scope ,_focus ,_ctx ,_obs)
     query-u*]
    [_ (error 'machine-query "unsupported machine: ~e" machine)]))

(define (machine-root-scope machine)
  (match machine
    [`(machine ,_query-u* ,root-scope ,_focus ,_ctx ,_obs)
     root-scope]
    [_ (error 'machine-root-scope "unsupported machine: ~e" machine)]))

(define (machine-focus machine)
  (match machine
    [`(machine ,_query-u* ,_root-scope ,focus ,_ctx ,_obs)
     focus]
    [_ (error 'machine-focus "unsupported machine: ~e" machine)]))

(define (machine-ctx machine)
  (match machine
    [`(machine ,_query-u* ,_root-scope ,_focus ,ctx ,_obs)
     ctx]
    [_ (error 'machine-ctx "unsupported machine: ~e" machine)]))

(define (machine-obs machine)
  (match machine
    [`(machine ,_query-u* ,_root-scope ,_focus ,_ctx ,obs)
     obs]
    [_ (error 'machine-obs "unsupported machine: ~e" machine)]))

(define (cfg->machine cfg)
  (match cfg
    [`(config ,query-u* ,root-scope ,term ,obs)
     (define-values (focus ctx)
       (premachine:decompose term))
     `(machine ,query-u* ,root-scope ,focus ,ctx ,obs)]
    [_ (error 'cfg->machine "unsupported premachine config: ~e" cfg)]))

(define (machine->cfg machine)
  (match machine
    [`(machine ,query-u* ,root-scope ,focus ,ctx ,obs)
     `(config ,query-u* ,root-scope ,(premachine:plug focus ctx) ,obs)]
    [_ (error 'machine->cfg "unsupported machine: ~e" machine)]))

(define (parse-example label)
  (cfg->machine (premachine:parse-example label)))

(define (step machine)
  (match machine
    [`(machine ,query-u* ,root-scope ,focus ,ctx ,obs)
     (match (premachine:contract focus ctx root-scope)
       [#f
        '()]
       [(list name next-focus emitted)
        (define next-term
          (premachine:plug next-focus ctx))
        (define-values (next-focus* next-ctx)
          (premachine:decompose next-term))
        (list (list name
                    `(machine ,query-u*
                              ,root-scope
                              ,next-focus*
                              ,next-ctx
                              ,(append obs emitted))))])]
    [_ (error 'step "unsupported machine: ~e" machine)]))

(define (trace machine [limit 128] [steps '()])
  (match (step machine)
    ['()
     (values (reverse steps)
             machine
             (if (final? machine) 'value 'stuck))]
    [_ #:when (zero? limit)
       (values (reverse steps) machine 'cap)]
    [(list (list name next))
     (trace next
            (sub1 limit)
            (cons name steps))]
    [_ (values (reverse steps) machine 'nondeterministic)]))

(define (final? machine)
  (null? (step machine)))

(define (answers machine)
  (premachine:answers (machine->cfg machine)))
