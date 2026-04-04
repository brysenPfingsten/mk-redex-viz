#lang racket

(provide x-symbol?
         u-symbol?
         scope-append
         fresh-u-symbol
         fresh-u-list
         walk
         occurs?
         unify
         invalid?
         subst-term
         subst-goal
         reify-query-values)

(define (x-symbol? s)
  (and (symbol? s)
       (regexp-match? #rx"^x:" (symbol->string s))))

(define (u-symbol? s)
  (and (symbol? s)
       (regexp-match? #rx"^u:" (symbol->string s))))

(define (scope-append intro outer)
  (append intro outer))

(define (fresh-u-symbol used [n 0])
  (define candidate
    (string->symbol (format "u:~a" n)))
  (if (member candidate used)
      (fresh-u-symbol used (add1 n))
      candidate))

(define (fresh-u-list used names)
  (define-values (rev _used)
    (for/fold ([rev '()]
               [used* used])
              ([_name (in-list names)])
      (define u
        (fresh-u-symbol used*))
      (values (cons u rev)
              (cons u used*))))
  (reverse rev))

(define (walk t sub)
  (match (assoc t sub)
    [(list _ value)
     (walk value sub)]
    [_ t]))

(define (occurs? u t sub)
  (match (walk t sub)
    [`(,t1 : ,t2)
     (or (occurs? u t1 sub)
         (occurs? u t2 sub))]
    [u^
     (equal? u u^)]))

(define (extend u t sub)
  (and (not (occurs? u t sub))
       (cons (list u t) sub)))

(define (unify t1 t2 sub)
  (define t1^ (walk t1 sub))
  (define t2^ (walk t2 sub))
  (match* (t1^ t2^)
    [((? u-symbol? u1) (? u-symbol? u2))
     (if (equal? u1 u2)
         sub
         (extend u1 u2 sub))]
    [((? u-symbol? u) t)
     (extend u t sub)]
    [(t (? u-symbol? u))
     (extend u t sub)]
    [(`(,a1 : ,d1) `(,a2 : ,d2))
     (match (unify a1 a2 sub)
       [#f #f]
       [sub^ (unify d1 d2 sub^)])]
    [(t t)
     sub]
    [(_ _)
     #f]))

(define (forced-equal? t1 t2 sub)
  (define t1^
    (walk t1 sub))
  (define t2^
    (walk t2 sub))
  (match* (t1^ t2^)
    [((? u-symbol? u1) (? u-symbol? u2))
     (equal? u1 u2)]
    [((? u-symbol? _) _)
     #f]
    [(_ (? u-symbol? _))
     #f]
    [(`(,a1 : ,d1) `(,a2 : ,d2))
     (and (forced-equal? a1 a2 sub)
          (forced-equal? d1 d2 sub))]
    [(t t)
     #t]
    [(_ _)
     #f]))

(define (invalid? sub dis)
  (match dis
    ['() #f]
    [(cons (list t1 t2) rest)
     (or (forced-equal? t1 t2 sub)
         (invalid? sub rest))]))

(define (subst-term t subs)
  (match t
    [(? x-symbol? x)
     (match (assoc x subs)
       [(list _ value) value]
       [_ x])]
    [`(,t1 : ,t2)
     `(,(subst-term t1 subs) : ,(subst-term t2 subs))]
    [_ t]))

(define (drop-subst-for d subs)
  (for/list ([pr (in-list subs)]
             #:do [(match-define (list x t) pr)]
             #:unless (member x d))
    (list x t)))

(define (subst-goal g subs)
  (match g
    [`(succeed ,tag)
     `(succeed ,tag)]
    [`(fail ,tag)
     `(fail ,tag)]
    [`(,t1 =? ,t2 ,tag)
     `(,(subst-term t1 subs) =? ,(subst-term t2 subs) ,tag)]
    [`(,t1 != ,t2 ,tag)
     `(,(subst-term t1 subs) != ,(subst-term t2 subs) ,tag)]
    [`(,g1 ∧ ,g2 ,tag)
     `(,(subst-goal g1 subs) ∧ ,(subst-goal g2 subs) ,tag)]
    [`(,g1 ∨ ,g2 ,tag)
     `(,(subst-goal g1 subs) ∨ ,(subst-goal g2 subs) ,tag)]
    [`(suspend ,g* ,tag)
     `(suspend ,(subst-goal g* subs) ,tag)]
    [`(∃ ,d ,g* ,tag)
     `(∃ ,d ,(subst-goal g* (drop-subst-for d subs)) ,tag)]
    [_
     (error 'subst-goal
            "unsupported goal form: ~e"
            g)]))

(define (reify-query-values query-u* sub)
  (map (lambda (u) (walk u sub))
       query-u*))
