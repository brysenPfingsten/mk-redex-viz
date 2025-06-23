#lang racket

(require redex racket/port racket/sandbox)
(require "judgment-forms.rkt")
(provide check-well-formed check-syntax-capture-error)

;; Prog -> String
;; Purpose: Checks if the given program satisfies the closed-program? judgment.
;; Returns: Empty string if well-formed, else error message.
(define (check-well-formed model-prog)
  (if (judgment-holds (closed-program? ,model-prog))
      ""
      (error "Program is not well formed!")))


;; read-all: port -> ListOf sexpression
;; Purpose: To read the string program into sexpressions
(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr)
        '()  ;; Stop when EOF is reached
        (cons expr (read-all port)))))


;; String -> String or Error
;; Purpose: Uses syntax-spec to throw static errors in the given program.
(define (check-syntax-capture-error program-str)
    (parameterize ([current-namespace (make-base-namespace)])
      (expand (datum->syntax #f
                             `(module syntax-checker racket/base
                                (require hosted-minikanren)
                                ,@(read-all (open-input-string program-str)))))))
