#lang racket

(provide instantiate-call-host
         subst-goal-host
         tree-prefix->shell/host
         empty-freshened-head?
         bubble-left-answer-host
         promote-left-answer-host
         bubble-left-fail-host
         skip-left-fail-host)

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
    (match-define (list t) t*)
    (list x t)))

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
    [`(suspend ,g* ,tag)
     `(suspend ,(subst-goal-host g* subs) ,tag)]
    [`(∃ ,d ,g* ,tag)
     `(∃ ,d ,(subst-goal-host g* (drop-subst-for-host d subs)) ,tag)]
    [`(,r ,args ... ,tag)
     #:when (r-symbol? r)
     `(,r ,@(map (lambda (a) (subst-t-host a subs)) args) ,tag)]
    [_ (error 'subst-goal-host
              "unsupported goal form in substitution: ~e"
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

(define (tree-prefix->shell/host t)
  (match t
    [`(FreshenedTree ,intro ,inner ,tag)
     `(FreshenedShell ,intro ,(tree-prefix->shell/host inner) ,tag)]
    [_ t]))

(define (empty-freshened-head? h)
  (match h
    ['(empty-tree) #t]
    [(or `(FreshenedTree ,_ ,inner ,_)
         `(FreshenedShell ,_ ,inner ,_))
     (empty-freshened-head? inner)]
    [_ #f]))

(define (answer-head?/host t)
  (match t
    [`(⊤ ,_) #t]
    [(or `(FreshenedTree ,_ ,inner ,_)
         `(FreshenedShell ,_ ,inner ,_))
     (answer-head?/host inner)]
    [_ #f]))

(define (lift-bounced-rewrite cfg rewrite-inner rebuild-inner)
  (match cfg
    [`(Bounced (,prefix + ,rest))
     #:when (answer-head?/host prefix)
     (match (rewrite-inner rest)
       [#f #f]
       [rest^ `(Bounced (,prefix + ,rest^))])]
    [`(Bounced ,inner)
     (rebuild-inner (rewrite-inner inner))]
    [_ #f]))

(define (rebuild-bounced-answer result)
  (match result
    [#f #f]
    [`(,left + ,rest)
     #:when (answer-head?/host left)
     `(,left + (Bounced ,rest))]
    [inner^ `(Bounced ,inner^)]))

(define (rebuild-bounced-inner result)
  (match result
    [#f #f]
    [inner^ `(Bounced ,inner^)]))

(define (bubble-left-answer-host cfg)
  (match cfg
    [`((,left <-+ ,mid) <-+ ,right)
     #:when (answer-head?/host left)
     `(,left + (,mid <-+ ,right))]
    [_ (lift-bounced-rewrite cfg
                             bubble-left-answer-host
                             rebuild-bounced-answer)]))

(define (promote-left-answer-host cfg)
  (match cfg
    [`(,left <-+ ,right)
     #:when (answer-head?/host left)
     `(,left + ,right)]
    [_ (lift-bounced-rewrite cfg
                             promote-left-answer-host
                             rebuild-bounced-answer)]))

(define (bubble-left-fail-host cfg)
  (match cfg
    [`(((empty-tree) <-+ ,mid) <-+ ,right)
     `(,mid <-+ ,right)]
    [_ (lift-bounced-rewrite cfg
                             bubble-left-fail-host
                             rebuild-bounced-inner)]))

(define (skip-left-fail-host cfg)
  (match cfg
    [`((empty-tree) <-+ ,right)
     right]
    [_ (lift-bounced-rewrite cfg
                             skip-left-fail-host
                             rebuild-bounced-inner)]))
