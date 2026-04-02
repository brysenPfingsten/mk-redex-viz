#lang racket

(require rackunit
         redex/reduction-semantics
         "../languages/core-lang.rkt")

(provide lvar-member?
         lvars-subset?
         lvars-same-members?
         lvars-fresh-extension?
         scope-pop/host
         wf-term?
         wf-sub?
         wf-dis?
         fresh-lv
         fresh-lvars
         wf-trail-unify*s-to-sub
         wf-sub/wf+equiv-trail?
         wf-state/at-scope?
         wf-state?
         symbols-in/set
         substitution-acyclic?
         acyclic-sub?)

(check-redundancy #t)

(define (symbols-in/set t [acc (set)])
  (match t
    ['() acc]
    [(? symbol?) (set-add acc t)]
    [(cons a d) (symbols-in/set a (symbols-in/set d acc))]
    [_ acc]))

(define (substitution-acyclic? pairs)
  (define dom (map first pairs))
  (define adj
    (for/hash ([(u t*) (in-dict pairs)])
      (match-define (list t) t*)
      (values u
              (for/set ([v (in-set (symbols-in/set t))]
                        #:when (member v dom))
                v))))
  (define visiting (make-hash))
  (define visited (make-hash))
  (define (visit u)
    (cond
      [(hash-ref visited u #f) #t]
      [(hash-ref visiting u #f) #f]
      [else
       (hash-set! visiting u #t)
       (define ok
         (for/and ([v (in-set (hash-ref adj u (set)))])
           (visit v)))
       (hash-remove! visiting u)
       (when ok (hash-set! visited u #t))
       ok]))
  (for/and ([u dom]) (visit u)))

(define-metafunction core-lang
  acyclic-sub? : sub -> boolean
  [(acyclic-sub? sub)
   ,(substitution-acyclic? (term sub))])

(define-judgment-form
  core-lang
  #:contract (lvar-member? u c)
  #:mode (lvar-member? I I)
  [-------- "lvar member"
   (lvar-member? u (u_1 ... u u_2 ...))])

(define-judgment-form
  core-lang
  #:contract (lvars-subset? (u ...) (u ...))
  #:mode (lvars-subset? I I)
  [------------------- "empty ⊆ anything"
   (lvars-subset? () c)]
  [(lvar-member? u c_2)
   (lvars-subset? (u_rest ...) c_2)
   ------------------- "cons ⊆"
   (lvars-subset? (u u_rest ...) c_2)])

(define (lvars-same-members?/host c_1 c_2)
  (and (for/and ([u (in-list c_1)])
         (and (member u c_2) #t))
       (for/and ([u (in-list c_2)])
         (and (member u c_1) #t))))

(define-judgment-form
  core-lang
  #:contract (lvars-same-members? (u ...) (u ...))
  #:mode (lvars-same-members? I I)
  [(where #t ,(lvars-same-members?/host (term c_1) (term c_2)))
   ------------------- "same lvars, order irrelevant"
   (lvars-same-members? c_1 c_2)])

(define (lvars-fresh-extension?/host c-intro c-outer)
  (and (= (length c-intro)
          (length (remove-duplicates c-intro)))
       (for/and ([u (in-list c-intro)])
         (not (member u c-outer)))))

(define (scope-pop/host intro current)
  (define n (length intro))
  (cond
    [(< (length current) n) #f]
    [(equal? intro (take current n)) (drop current n)]
    [else #f]))

(define-judgment-form
  core-lang
  #:contract (lvars-fresh-extension? c c)
  #:mode (lvars-fresh-extension? I I)
  [(where #t ,(lvars-fresh-extension?/host (term c_1) (term c_2)))
   ------------------- "fresh lvar extension"
   (lvars-fresh-extension? c_1 c_2)])

(define-judgment-form
  core-lang
  #:contract (wf-term? t (x ...) c)
  #:mode (wf-term? I I I)
  [(lvar-member? u c)
   -------------- "lv in extant lvs"
   (wf-term? u (x ...) c)]
  [-------------- "primitive terms are wf and valid"
   (wf-term? pt (x ...) c)]
  [(wf-term? t_2 (x ...) c)
   (wf-term? t_1 (x ...) c)
   -------------- "pairs wf when constituents wf"
   (wf-term? (t_1 : t_2) (x ...) c)]
  [-------------- "lexical var is in lv bindings"
   (wf-term? x_2 (x_1 ... x_2 x_3 ...) c)])

(define-judgment-form
  core-lang
  #:contract (wf-sub? sub c)
  #:mode (wf-sub? I I)
  [(wf-term? t () c) ...
   (lvar-member? u c) ...
   (where #t (acyclic-sub? ([u t] ...)))
   ------------------ "sub closed under c w/no lexical vars"
   (wf-sub? ([u t] ...) c)])

(define-judgment-form
  core-lang
  #:contract (wf-dis? dis c)
  #:mode (wf-dis? I I)
  [------------------ "empty disequality store is wf"
   (wf-dis? () c)]
  [(wf-term? t_1 () c)
   (wf-term? t_2 () c)
   (wf-dis? ((t_3 t_4) ...) c)
   ------------------ "disequality pair wf"
   (wf-dis? ((t_1 t_2) (t_3 t_4) ...) c)])

(define-metafunction core-lang
  fresh-lv : (u ...) -> u
  [(fresh-lv (u ...)) ,(variable-not-in (cons 'u: (term (u ...))) 'u:)])

(define-metafunction core-lang
  fresh-lvars : (x ...) c -> c
  [(fresh-lvars (x ...) c)
   ,(let-values ([(fv* _used)
                  (for/fold ([fv* '()]
                             [used (term c)])
                            ([_ (in-list (term (x ...)))])
                    (define fv (variable-not-in (cons 'u: used) 'u:))
                    (values (cons fv fv*) (cons fv used)))])
      fv*)])

(define-judgment-form
  core-lang
  #:contract (wf-trail-unify*s-to-sub (eq ...) c sub sub)
  #:mode (wf-trail-unify*s-to-sub I I I I)
  [------------------- "trail is empty, acc is our sub"
   (wf-trail-unify*s-to-sub () c sub sub)]
  [(where sub_acc2 (unify (walk t_1 sub_acc) (walk t_2 sub_acc) sub_acc))
   (wf-term? t_1 () c)
   (wf-term? t_2 () c)
   (wf-trail-unify*s-to-sub (eq ...) c sub_acc2 sub)
   ------------------- "this pair is well formed and unify"
   (wf-trail-unify*s-to-sub ((t_1 =? t_2 tag) eq ...) c sub_acc sub)])

(define-judgment-form
  core-lang
  #:contract (wf-sub/wf+equiv-trail? sub c trail)
  #:mode (wf-sub/wf+equiv-trail? I I I)
  [(wf-sub? sub c)
   (wf-trail-unify*s-to-sub (eq ...) c () sub)
   ------------------- "goal w/ sub wf"
   (wf-sub/wf+equiv-trail? sub c (eq ...))])

(define-judgment-form
  core-lang
  #:contract (wf-state/at-scope? σ c)
  #:mode (wf-state/at-scope? I I)
  [(lvars-same-members? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ----------------------- "state wf at exact ambient scope"
   (wf-state/at-scope? (state sub dis c_i trail tag) c)])

(define-judgment-form
  core-lang
  #:contract (wf-state? σ)
  #:mode (wf-state? I)
  [(wf-state/at-scope? (state sub dis c trail tag) c)
   (where #f (invalid? sub dis))
   ----------------------- "state wf"
   (wf-state? (state sub dis c trail tag))])

(module+ test
  (check-true (judgment-holds (lvar-member? u:0 (u:0))))
  (check-true (judgment-holds (wf-term? (sym "a") () ())))
  (check-true (judgment-holds (wf-sub? ((u:0 (sym "x"))) (u:0)))))
