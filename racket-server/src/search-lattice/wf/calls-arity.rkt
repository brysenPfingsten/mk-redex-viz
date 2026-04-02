#lang racket

(provide relcall-arity-ok/host)

(define (relation-arity rel-def)
  (match rel-def
    [`(,_ ,d ,_) (length d)]
    [_ #f]))

(define (relcall-arity-ok/host r-call args rel-env)
  (cond
    [(null? rel-env) #f]
    [else
     (match (car rel-env)
       [`(,r ,_ ,_) #:when (equal? r-call r)
        (equal? (length args) (relation-arity (car rel-env)))]
       [_ (relcall-arity-ok/host r-call args (cdr rel-env))])]))
