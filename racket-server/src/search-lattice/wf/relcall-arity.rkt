#lang racket

(provide relcall-arity-ok/host)

(define (relation-arity rel-def)
  (match rel-def
    [`(,_ ,d ,_) (length d)]
    [_ #f]))

(define (relcall-arity-ok/host r-call args rel-env)
  (match rel-env
    ['() #f]
    [(cons rel-def rest)
     (match rel-def
       [`(,r ,_ ,_) #:when (equal? r-call r)
        (equal? (length args) (relation-arity rel-def))]
       [_ (relcall-arity-ok/host r-call args rest)])]))
