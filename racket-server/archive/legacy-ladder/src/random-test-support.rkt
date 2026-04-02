#lang racket

(require racket/list)

(provide make-seeded-rng
         rng-random
         remove-at
         random-distinct/rng
         gen-primitive/rng)

(define (make-seeded-rng seed)
  (unless (exact-nonnegative-integer? seed)
    (raise-argument-error 'make-seeded-rng "exact-nonnegative-integer?" seed))
  (define rng (make-pseudo-random-generator))
  (parameterize ([current-pseudo-random-generator rng])
    (random-seed seed))
  rng)

(define (rng-random rng n)
  (unless (exact-positive-integer? n)
    (raise-argument-error 'rng-random "exact-positive-integer?" n))
  (parameterize ([current-pseudo-random-generator rng])
    (random n)))

(define (remove-at xs idx)
  (define-values (prefix suffix) (split-at xs idx))
  (match suffix
    ['() prefix]
    [(cons _ rest) (append prefix rest)]))

(define (random-distinct/rng rng xs k)
  (unless (exact-nonnegative-integer? k)
    (raise-argument-error 'random-distinct/rng "exact-nonnegative-integer?" k))
  (define need (min k (length xs)))
  (for/fold ([pool xs]
             [rev-acc '()]
             #:result (reverse rev-acc))
            ([_ (in-range need)])
    (define idx (rng-random rng (length pool)))
    (values (remove-at pool idx)
            (cons (list-ref pool idx) rev-acc))))

(define (gen-primitive/rng rng)
  (case (rng-random rng 5)
    [(0) `(sym ,(format "sym-~a" (rng-random rng 100)))]
    [(1) `(nat ,(rng-random rng 20))]
    [(2) (zero? (rng-random rng 2))]
    [(3) `(str ,(format "str-~a" (rng-random rng 100)))]
    [else 'empty]))
