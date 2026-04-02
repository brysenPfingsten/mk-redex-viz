#lang racket

(require redex/reduction-semantics)

(provide step-once/deterministic)

(define (step-once/deterministic rel prog)
  (define named-next*
    (apply-reduction-relation/tag-with-names rel (term ,prog)))
  (match named-next*
    ['() '()]
    [(list only-step) (list only-step)]
    [_ (error 'step-once/deterministic
              (format "nondeterministic next-step set for ~v: ~v"
                      prog
                      named-next*))]))
