#lang racket

(require "./ast.rkt"
         "./profile.rkt"
         "./mini.rkt"
         "./micro.rkt")

(provide prepare-program
         parse-prog->ast
         render-micro-source)

(define (split-program-forms exprs [defrels-rev '()] [run-expr #f])
  (match exprs
    ['()
     (values (reverse defrels-rev) run-expr)]
    [(cons expr rest)
     (match expr
       [`(defrel . ,_)
        (split-program-forms rest (cons expr defrels-rev) run-expr)]
       [(or `(run . ,_) `(run* . ,_))
        (split-program-forms rest defrels-rev expr)]
       [_ (error "Not a defrel or run form" expr)])]))

(define (prepare-program lst
                         [source-mode default-source-mode]
                         [compile-profile #f])
  (define source-mode* (normalize-source-mode source-mode))
  (define compile-profile*
    (normalize-compile-profile compile-profile source-mode*))
  (define-values (defrels run-expr)
    (split-program-forms lst))
  (case (string->symbol source-mode*)
    [(mini)
     (define-values (normalized-ast display-ast)
       (prepare-mini-program defrels run-expr compile-profile*))
     (values normalized-ast display-ast compile-profile*)]
    [(micro)
     (define-values (normalized-ast display-ast)
       (prepare-micro-program defrels run-expr))
     (values normalized-ast display-ast compile-profile*)]
    [else
     (error 'prepare-program
            "unsupported source mode ~e"
            source-mode*)]))

(define (parse-prog->ast lst
                         #:source-mode [source-mode default-source-mode]
                         #:compile-profile [compile-profile #f])
  (define-values (normalized-ast _display-ast _profile)
    (prepare-program lst source-mode compile-profile))
  normalized-ast)

(define (render-micro-source lst
                             #:source-mode [source-mode default-source-mode]
                             #:compile-profile [compile-profile #f])
  (program->micro-string
   (parse-prog->ast lst
                    #:source-mode source-mode
                    #:compile-profile compile-profile)))
