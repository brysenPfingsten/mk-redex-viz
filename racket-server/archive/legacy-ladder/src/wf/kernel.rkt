#lang racket

(require rackunit
         redex/reduction-semantics
         "../languages/l0.rkt")

(check-redundancy #t)

(provide lvar-member?
         lvars-subset?
         wf-term?
         wf-sub?
         wf-dis?
         fresh-lv
         fresh-lvars
         wf-trail-unify*s-to-sub
         wf-sub/wf+equiv-trail?
         wf-state?
         symbols-in/set
         substitution-acyclic?
         acyclic-sub?)

;; Collect symbols appearing in a term-shaped datum into a set.
(define (symbols-in/set t [acc (set)])
  (match t
    ['() acc]
    [(? symbol?) (set-add acc t)]
    [(cons a d) (symbols-in/set a (symbols-in/set d acc))]
    [_ acc]))

;; Graph-based acyclicity check for substitutions represented as `((u t) ...)`.
(define (substitution-acyclic? pairs)
  (define dom (map first pairs))
  (define adj
    (for/hash ([(u t*) (in-dict pairs)])
      (values u
              (for/set ([v (in-set (symbols-in/set (car t*)))]
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

(define-metafunction L0
  acyclic-sub? : sub -> boolean
  [(acyclic-sub? sub)
   ,(substitution-acyclic? (term sub))])

(define-judgment-form
  L0
  #:contract (lvar-member? u c)
  #:mode (lvar-member? I I)
  [-------- "lvar member"
   (lvar-member? u (u_1 ... u u_2 ...))])

(define-judgment-form
  L0
  #:contract (lvars-subset? (u ...) (u ...))
  #:mode (lvars-subset? I I)
  [------------------- "empty ⊆ anything"
   (lvars-subset? () c)]
  [(lvar-member? u c_2)
   (lvars-subset? (u_rest ...) c_2)
   ------------------- "cons ⊆"
   (lvars-subset? (u u_rest ...) c_2)])

(define-judgment-form
  L0
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
  L0
  #:contract (wf-sub? sub c)
  #:mode (wf-sub? I I)
  [(wf-term? t () c) ...
   (lvar-member? u c) ...
   (where #t (acyclic-sub? ([u t] ...)))
   ------------------ "sub closed under c w/no lexical vars"
   (wf-sub? ([u t] ...) c)])

(define-judgment-form
  L0
  #:contract (wf-dis? dis c)
  #:mode (wf-dis? I I)
  [------------------ "empty disequality store is wf"
   (wf-dis? () c)]
  [(wf-term? t_1 () c)
   (wf-term? t_2 () c)
   (wf-dis? ((t_3 t_4) ...) c)
   ------------------ "disequality pair wf"
   (wf-dis? ((t_1 t_2) (t_3 t_4) ...) c)])

(define-metafunction L0
  fresh-lv : (u ...) -> u
  [(fresh-lv (u ...)) ,(variable-not-in (cons 'u: (term (u ...))) 'u:)])

;; redex's variables-not-in uses the vars list themselves as prefixes,
;; which does not match our use case.
(define-metafunction L0
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
  L0
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
  L0
  #:contract (wf-sub/wf+equiv-trail? sub c trail)
  #:mode (wf-sub/wf+equiv-trail? I I I)
  [(wf-sub? sub c)
   (wf-trail-unify*s-to-sub (eq ...) c () sub)
   ------------------- "goal w/ sub wf"
   (wf-sub/wf+equiv-trail? sub c (eq ...))])

(define-judgment-form
  L0
  #:contract (wf-state? σ)
  #:mode (wf-state? I)
  [(wf-sub/wf+equiv-trail? sub c trail)
   (wf-dis? dis c)
   (where #f (invalid? sub dis))
   ----------------------- "state wf"
   (wf-state? (state sub dis c trail tag))])

(module+ test
  (check-true (judgment-holds (lvar-member? u:0 (u:0))))
  (check-true (judgment-holds (lvar-member? u:0 (u:1 u:0))))
  (check-false (judgment-holds (lvar-member? u:7 (u:0))))
  (check-true (judgment-holds (lvar-member? u:1 (u:2 u:1 u:0))))

  (check-true (judgment-holds (wf-term? (sym "a") () ())))
  (check-true (judgment-holds (wf-term? u:0 () (u:0))))
  (check-true (judgment-holds (wf-term? u:1 () (u:0 u:1))))
  (check-false (judgment-holds (wf-term? u:3 () (u:0 u:1))))
  (check-true (judgment-holds (wf-term? x:0 (x:0) (u:5))))
  (check-false (judgment-holds (wf-term? x:0 () (u:5))))

  (check-true (substitution-acyclic? (term ((u:0 u:1) (u:1 (sym "z"))))))
  (check-false (substitution-acyclic? (term ((u:0 u:1) (u:1 u:0)))))
  (check-true (judgment-holds (wf-sub? ((u:0 (sym "x"))) (u:0))))
  (check-false (judgment-holds (wf-sub? ((u:1 (sym "x"))) (u:0))))
  (check-true (judgment-holds (wf-sub? ((u:0 (sym "x")) (u:2 (sym "y")))
                                       (u:0 u:2))))
  (check-true (judgment-holds (wf-sub? ((u:0 u:1) (u:1 (sym "z")))
                                       (u:0 u:1))))
  (check-false (judgment-holds (wf-sub? ((u:0 u:1) (u:1 u:0))
                                        (u:0 u:1))))

  (check-equal?
   (term (fresh-lvars (x:0 x:1 x:2) (u:1 u:7 u:3)))
   '(u:5 u:4 u:2))

  (check-false
   (judgment-holds
    (wf-trail-unify*s-to-sub () (u:2 u:1 u:0) ((u:0 u:2) (u:1 u:0)) ((u:1 u:0)))))
  (check-false
   (judgment-holds
    (wf-trail-unify*s-to-sub () (u:2 u:1 u:0) ((u:1 u:0)) ((u:0 u:2) (u:1 u:0)))))

  (check-true
   (judgment-holds
    (wf-trail-unify*s-to-sub
     ((u:0 =? (sym "a") (label "t1"))
      ((u:1 : u:0) =? ((sym "b") : (sym "a")) (label "t2")))
     (u:0 u:1)
     ()
     ((u:1 (sym "b")) (u:0 (sym "a"))))))

  (check-true
   (judgment-holds
    (wf-sub/wf+equiv-trail?
     ((u:1 (sym "b")) (u:0 (sym "a")))
     (u:0 u:1)
     ((u:0 =? (sym "a") (label "t1"))
      ((u:1 : u:0) =? ((sym "b") : (sym "a")) (label "t2"))))))

  (check-false
   (judgment-holds
    (wf-state? (state ((u:1 (sym "b")) (u:0 (sym "a")))
                      ()
                      (u:0 u:1)
                      ((u:0 =? (sym "a") (label "t1")))
                      (label "σ"))))))
