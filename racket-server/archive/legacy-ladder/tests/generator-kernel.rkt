#lang racket

(require racket/list
         (prefix-in rt: "../src/random-test-support.rkt"))

(provide require-positive
         require-nonnegative
         make-u-pool
         make-x-pool
         make-r-pool
         pick-one/rng
         make-label/rng
         extend-c/rng
         gen-term/rng
         fresh-x-list/rng)

(define (require-positive who n err-sym)
  (unless (positive? n)
    (error err-sym (format "~a must be >= 1, got ~a" who n))))

(define (require-nonnegative who n err-sym)
  (unless (>= n 0)
    (error err-sym (format "~a must be >= 0, got ~a" who n))))

(define (make-u-pool size)
  (for/list ([i (in-range 0 size)])
    (string->symbol (format "u:~a" i))))

(define (make-x-pool size)
  (for/list ([i (in-range 0 size)])
    (string->symbol (format "x:~a" i))))

(define (make-r-pool size)
  (for/list ([i (in-range 0 size)])
    (string->symbol (format "r:~a" i))))

(define (pick-one/rng rng xs)
  (list-ref xs (rt:rng-random rng (length xs))))

(define (make-label/rng rng prefix)
  `(label ,(format "~a-~a" prefix (rt:rng-random rng 1000000))))

(define (extend-c/rng rng c u-pool c-max max-extra)
  (define unused
    (filter (lambda (u) (not (member u c))) u-pool))
  (define room (- c-max (length c)))
  (define extra-limit (min max-extra room (length unused)))
  (define extra-count (rt:rng-random rng (add1 extra-limit)))
  (append c (rt:random-distinct/rng rng unused extra-count)))

(define (gen-term/rng rng x-env c depth)
  (define options
    (append '(primitive)
            (if (null? c) '() '(logic-var))
            (if (null? x-env) '() '(lex-var))
            (if (zero? depth) '() '(pair))))
  (case (pick-one/rng rng options)
    [(primitive) (rt:gen-primitive/rng rng)]
    [(logic-var) (pick-one/rng rng c)]
    [(lex-var) (pick-one/rng rng x-env)]
    [(pair)
     `(,(gen-term/rng rng x-env c (sub1 depth))
       :
       ,(gen-term/rng rng x-env c (sub1 depth)))]))

(define (fresh-x-list/rng rng x-env x-pool)
  (define available (filter (lambda (x) (not (member x x-env))) x-pool))
  (rt:random-distinct/rng rng
                          available
                          (rt:rng-random rng (add1 (min 2 (length available))))))
