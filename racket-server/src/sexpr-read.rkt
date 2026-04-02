#lang racket

(provide read-all-sexprs)

;; Read every s-expression from a port until EOF.
(define (read-all-sexprs port [acc '()])
  (define expr (read port))
  (if (eof-object? expr)
      (reverse acc)
      (read-all-sexprs port (cons expr acc))))
