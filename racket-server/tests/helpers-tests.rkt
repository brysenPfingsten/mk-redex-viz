#lang racket

(require rackunit
         rackunit/text-ui
         racket/match
         "./helpers.rkt")

(provide HELPERS-TESTS)

(define (primitive-term? v)
  (or (boolean? v)
      (equal? v 'empty)
      (match v
        [`(sym ,s) (string? s)]
        [`(nat ,n) (and (integer? n) (>= n 0))]
        [`(str ,s) (string? s)]
        [_ #f])))

(define-test-suite HELPERS-TESTS
  (test-case "make-seeded-rng + rng-random deterministic replay"
    (define r1 (make-seeded-rng 424242))
    (define r2 (make-seeded-rng 424242))
    (check-equal? (for/list ([_ (in-range 40)]) (rng-random r1 1000))
                  (for/list ([_ (in-range 40)]) (rng-random r2 1000))))

  (test-case "remove-at removes index"
    (check-equal? (remove-at '(a b c d) 0) '(b c d))
    (check-equal? (remove-at '(a b c d) 2) '(a b d))
    (check-equal? (remove-at '(a b c d) 3) '(a b c)))

  (test-case "random-distinct/rng returns subset with no duplicates"
    (define rng (make-seeded-rng 999))
    (define xs '(a b c d e))
    (define out (random-distinct/rng rng xs 4))
    (check-true (<= (length out) 4))
    (check-equal? (length out) (length (remove-duplicates out)))
    (check-true (andmap (lambda (x) (not (false? (member x xs)))) out)))

  (test-case "random-distinct/rng saturates when k exceeds length"
    (define rng (make-seeded-rng 999))
    (define xs '(a b c d e))
    (define out (random-distinct/rng rng xs 99))
    (check-equal? (length out) (length xs))
    (check-equal? (length out) (length (remove-duplicates out)))
    (check-true (andmap (lambda (x) (not (false? (member x xs)))) out)))

  (test-case "gen-primitive/rng stays in expected primitive grammar fragment"
    (define rng (make-seeded-rng 2026))
    (for ([_ (in-range 200)])
      (check-true (primitive-term? (gen-primitive/rng rng)))))

  (test-case "helpers argument checks fail fast on bad inputs"
    (check-exn exn:fail:contract? (lambda () (make-seeded-rng -1)))
    (check-exn exn:fail:contract? (lambda () (rng-random (make-seeded-rng 1) 0)))
    (check-exn exn:fail:contract? (lambda () (random-distinct/rng (make-seeded-rng 1) '(a b) -1)))))

(module+ test
  (run-tests HELPERS-TESTS))
