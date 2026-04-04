#lang racket

(require "../shared/configs.rkt"
         (prefix-in premachine: "../premachine/main.rkt"))

(provide parse-example
         cfg->machine
         machine->cfg
         step
         trace
         final?
         answers
         (struct-out machine))

(define (cfg->machine cfg)
  (match cfg
    [(struct premachine:premachine-config (query-u* root-scope term obs))
     (define-values (focus ctx)
       (premachine:decompose term))
     (machine query-u* root-scope focus ctx obs)]
    [_ (error 'cfg->machine "unsupported premachine config: ~e" cfg)]))

(define (machine->cfg m)
  (match m
    [(struct machine (query-u* root-scope focus ctx obs))
     (premachine:premachine-config query-u*
                                   root-scope
                                   (premachine:plug focus ctx)
                                   obs)]
    [_ (error 'machine->cfg "unsupported machine: ~e" m)]))

(define (parse-example label)
  (cfg->machine (premachine:parse-example label)))

(define (step m)
  (match m
    [(struct machine (query-u* root-scope focus ctx obs))
     (match (premachine:contract focus ctx root-scope)
       [#f
        '()]
       [(list name next-focus emitted)
        (define next-term
          (premachine:plug next-focus ctx))
        (define-values (next-focus* next-ctx)
          (premachine:decompose next-term))
        (list (list name
                    (machine query-u*
                             root-scope
                             next-focus*
                             next-ctx
                             (append obs emitted))))])]
    [_ (error 'step "unsupported machine: ~e" m)]))

(define (trace m [limit 128] [steps '()])
  (match (step m)
    ['()
     (values (reverse steps)
             m
             (if (final? m) 'value 'stuck))]
    [_ #:when (zero? limit)
       (values (reverse steps) m 'cap)]
    [(list (list name next))
     (trace next
            (sub1 limit)
            (cons name steps))]
    [_ (values (reverse steps) m 'nondeterministic)]))

(define (final? m)
  (premachine:final? (machine->cfg m)))

(define (answers m)
  (premachine:answers (machine->cfg m)))
