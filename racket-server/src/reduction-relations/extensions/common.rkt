#lang racket

(require racket/match
         redex/reduction-semantics
         "../../core-definitions.rkt")

(provide instantiate-call-host
         subst-goal-host
         bridge-source-delay/lazy-host
         bridge-source-delay/eager-host)

;; Relation call helper shared by eager/lazy variants.
(define (x-symbol? s)
  (and (symbol? s)
       (regexp-match? #rx"^x:" (symbol->string s))))

(define (r-symbol? s)
  (and (symbol? s)
       (regexp-match? #rx"^r:" (symbol->string s))))

(define (subst-t-host t subs)
  (match t
    [(? x-symbol? x)
     (define a (assoc x subs))
     (if a (second a) x)]
    [`(,t1 : ,t2)
     `(,(subst-t-host t1 subs) : ,(subst-t-host t2 subs))]
    [_ t]))

(define (drop-subst-for-host d subs)
  (for/list ([(x t*) (in-dict subs)]
             #:unless (member x d))
    (list x (car t*))))

(define (subst-goal-host g subs)
  (match g
    [`(succeed ,tag)
     `(succeed ,tag)]
    [`(fail ,tag)
     `(fail ,tag)]
    [`(,t1 =? ,t2 ,tag)
     `(,(subst-t-host t1 subs) =? ,(subst-t-host t2 subs) ,tag)]
    [`(,t1 != ,t2 ,tag)
     `(,(subst-t-host t1 subs) != ,(subst-t-host t2 subs) ,tag)]
    [`(,g1 ∧ ,g2 ,tag)
     `(,(subst-goal-host g1 subs) ∧ ,(subst-goal-host g2 subs) ,tag)]
    [`(,g1 ∨ ,g2 ,tag)
     `(,(subst-goal-host g1 subs) ∨ ,(subst-goal-host g2 subs) ,tag)]
    [`(sdelay ,g* ,tag)
     `(sdelay ,(subst-goal-host g* subs) ,tag)]
    [`(∃ ,d ,g* ,tag)
     `(∃ ,d ,(subst-goal-host g* (drop-subst-for-host d subs)) ,tag)]
    [`(,r ,args ... ,tag)
     #:when (r-symbol? r)
     `(,r ,@(map (lambda (a) (subst-t-host a subs)) args) ,tag)]
    [_ (error 'subst-goal-host
              "unsupported goal form in call instantiation: ~e"
              g)]))

(define (instantiate-call-host gamma r ts)
  (define maybe-rel
    (for/first ([clause (in-list gamma)]
                #:when (equal? (first clause) r))
      clause))
  (unless maybe-rel
    (error 'instantiate-call-host "unknown relation ~a in Γ" r))
  (match-define (list _r d g) maybe-rel)
  (unless (= (length d) (length ts))
    (error 'instantiate-call-host
           "arity mismatch for ~a: expected ~a, got ~a"
           r
           (length d)
           (length ts)))
  (subst-goal-host g (map list d ts)))

(define (bridge-source-delay/lazy-host _gamma goal sigma)
  (match goal
    [`(sdelay (,r ,ts ... ,tag-call) ,_tag-delay)
     #:when (r-symbol? r)
     `(delay (proceed ((,r ,@ts ,tag-call) ,sigma)))]
    [`(sdelay ,inner ,_tag-delay)
     `(delay (,inner ,sigma))]
    [_ #f]))

(define (bridge-source-delay/eager-host gamma goal sigma)
  (match goal
    [`(sdelay (,r ,ts ... ,_tag-call) ,_tag-delay)
     #:when (r-symbol? r)
     (define g-new (instantiate-call-host gamma r ts))
     `(delay (proceed (,g-new ,sigma)))]
    [`(sdelay ,inner ,_tag-delay)
     `(delay (,inner ,sigma))]
    [_ #f]))
