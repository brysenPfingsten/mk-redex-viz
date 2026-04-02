#lang racket
(require redex/reduction-semantics)
;; Jason Hemann and Brysen Pfingsten
;; Initial redex lang setup from Ryan Jung
;; Unify &c metafunctions from Phil Nguyen

(check-redundancy #t)

#;(current-traced-metafunctions 'all)

(provide L0
         unify
         walk
         extend
         occurs?
         invalid?
         fresh-substitution
         subst-goal
         append-answer)

(module+ test
  (require rackunit)
  (default-language L0)

)

(define-language L0
  ;--------------------Top Level-------------------------
  [config (Γ s as)] ; Program: active work + produced answer stream

  [Γ ((r_!_ d g) ...)]  ; Relation Environment w/ distinct relation names
  [d (x_!_ ...)]        ; Distinct variable declarations

  ;-------------------Search Trees------------------------
  [s (empty-tree)               ; Empty Tree / Failure
     (g σ)                      ; Goal-State
     (s × g c)                  ; Conjunction, w/vars used so far.
     (⊤ σ)                      ; Immediate single answer

  ]
  ;----------------------Goals----------------------------
  [eq (t =? t tag)] ; Syntactic equality w/ tag
  [neq (t != t tag)] ; Syntactic disequality w/ tag

  [g eq
     neq
     (succeed tag)
     (fail tag)
     (∃ d g tag)     ; Variable introduction w/ tag
     (g ∧ g tag)     ; Conjunction w/ tag

  ]
  ;----------------------Terms---------------------------
  [t x             ; Parameters
     u             ; logic vars
	 pt
     (t : t)       ; Non-empty list
   ]

  ;; primitive terms are either
  [pt (sym string)  ; Symbol constants
      (nat number)  ; Numeric constants
      boolean
      (str string)  ; String contants
      empty         ; Empty list
	  ]


  ;----------------------Other---------------------------
  [r (variable-prefix r:)]  ; to account for arbitrary relation names
  [x (variable-prefix x:)]  ; to account for arbitrary parameter names
  [u (variable-prefix u:)] ; Logic variables, tagged, not raw naturals
  [tag (label string)]

  [σ (state sub dis c trail tag)] ; State
  [sub ((u_!_ t) ...)]        ; Substitution, make the vars definitionally distinct
  [dis ((t t) ...)]
  [maybe-sub sub #f]
  [trail (eq ...)]
  [as (empty-stream)
      (⊤ σ)
      ((⊤ σ) + as)]
  [end-config (Γ (empty-tree) as)]
  [c (u_!_ ...)]
  ;; Kconj: descend only through the active left side of conjunction.
  [Kconj ::= hole
             (Kconj × g c)]
  ;---------------------Binding Forms--------------------
  #:binding-forms
  (∃ (x ...) g #:refers-to (shadow x ...))
  (s #:refers-to (shadow r ...) ((r (x ...) g #:refers-to (shadow x ...)) ...) #:refers-to (shadow r ...))
)

(module+ test
  ;; matches to create a new variable not in a term
  (redex-define L0 (name new-var u) (variable-not-in (term (u: u:1 u:2 u:3)) 'u:))
  (check-true (redex-match? L0 u (term new-var)))

  ;; terms and primitives
  (check-equal? (term u:2) 'u:2)
  (check-true   (redex-match? L0 u (term u:2)))

  (check-true   (redex-match? L0 pt (term empty)))
  (check-false  (redex-match? L0 pt (term (sym 5)))) ; bad payload

  (check-true   (redex-match? L0 t (term (sym "a"))))
  (check-true   (redex-match? L0 t (term u:2)))
  (check-true   (redex-match? L0 t (term (u:0 : u:1))))
  (check-true   (redex-match? L0 t (term (u:0 : (sym "x")))))

  ;; one binding: list of pairs ((u t) ...)
  (check-true  (redex-match? L0 sub (term ((u:0 (sym "x"))))))
  (check-false (redex-match? L0 sub (term (u:1 (sym "x"))))) ; missing parens
  (check-false (redex-match? L0 sub (term ((u:0 (sym "x")) (u:0 (sym "y")))))) ; non-distinct

  (check-true (redex-match? L0 tag (term (label "t"))))

  (check-true (redex-match? L0 g (term (u:0 =? (sym "a") (label "t")))))
  (check-true (redex-match? L0 s (term (⊤ (state () () () () (label "Om"))))))
  (check-true (redex-match? L0 s (term ((u:0 =? (sym "a") (label "t")) (state ((u:0 (sym "a"))) () (u:0) () (label "σ"))))))

  (check-true (redex-match? L0 config (term (() (empty-tree) (empty-stream)))))

)

(define-metafunction L0
  append-answer : as σ -> as
  [(append-answer (empty-stream) σ_new)
   (⊤ σ_new)]
  [(append-answer (⊤ σ_old) σ_new)
   ((⊤ σ_old) + (⊤ σ_new))]
  [(append-answer ((⊤ σ_old) + as_tail) σ_new)
   ((⊤ σ_old) + (append-answer as_tail σ_new))])


(define-metafunction L0
  walk : t sub -> t
  [(walk u (name sub (_ ... [u t] _ ...))) (walk t sub)]
  [(walk t _) t])

(define-metafunction L0
  invalid? : sub dis -> boolean
  [(invalid? sub ()) #f]
  [(invalid? sub ((t_1 t_2) (t_3 t_4) ...))
   #t
   (where sub (unify (walk t_1 sub) (walk t_2 sub) sub))]
  [(invalid? sub ((t_1 t_2) (t_3 t_4) ...))
   (invalid? sub ((t_3 t_4) ...))])

;; Pick the least-indexed u:n not already present in `used`.
(define (fresh-u-symbol used [n 0])
  (define u
    (string->symbol (format "u:~a" n)))
  (cond
    [(member u used) (fresh-u-symbol used (add1 n))]
    [else u]))

;; Build ((x u) ...) where each u is fresh w.r.t. c and previously chosen u's.
(define-metafunction L0
  fresh-substitution : c d -> ((x u) ...)
  [(fresh-substitution c (x ...))
   ,(let ([xs (term (x ...))]
          [used0 (term c)])
      (define-values (rev-pairs _used)
        (for/fold ([rev-pairs '()]
                   [used used0])
                  ([x (in-list xs)])
          (define u (fresh-u-symbol used))
          (values (cons (list x u) rev-pairs)
                  (cons u used))))
      (reverse rev-pairs))])

;; Remove substitutions for variables newly bound by a declaration list.
(define-metafunction L0
  drop-subst-for : d ((x t) ...) -> ((x t) ...)
  [(drop-subst-for (x_b ...) ((x_1 t_1) ...))
   ,(let* ([bound (term (x_b ...))]
           [subs (term ((x_1 t_1) ...))])
      (for/list ([(x t*) (in-dict subs)]
                 #:unless (member x bound))
        (list x (car t*))))])

;; Capture-avoiding substitution over terms.
(define-metafunction L0
  subst-t : t ((x t) ...) -> t
  [(subst-t x ((x_1 t_1) ... (x t_0) (x_2 t_2) ...))
   t_0]
  [(subst-t x ((x_1 t_1) ...))
   x]
  [(subst-t u ((x_1 t_1) ...)) u]
  [(subst-t pt ((x_1 t_1) ...)) pt]
  [(subst-t (t_1 : t_2) ((x_1 t_1_sub) ...))
   ((subst-t t_1 ((x_1 t_1_sub) ...))
    :
    (subst-t t_2 ((x_1 t_1_sub) ...)))])

;; Capture-avoiding substitution over goals.
(define-metafunction L0
  subst-goal : g ((x t) ...) -> g
  [(subst-goal (succeed tag) ((x_1 t_1) ...))
   (succeed tag)]
  [(subst-goal (fail tag) ((x_1 t_1) ...))
   (fail tag)]
  [(subst-goal (t_1 =? t_2 tag) ((x_1 t_1_sub) ...))
   ((subst-t t_1 ((x_1 t_1_sub) ...))
    =?
    (subst-t t_2 ((x_1 t_1_sub) ...)
    )
    tag)]
  [(subst-goal (t_1 != t_2 tag) ((x_1 t_1_sub) ...))
   ((subst-t t_1 ((x_1 t_1_sub) ...))
    !=
    (subst-t t_2 ((x_1 t_1_sub) ...))
    tag)]
  [(subst-goal (g_1 ∧ g_2 tag) ((x_1 t_1_sub) ...))
   ((subst-goal g_1 ((x_1 t_1_sub) ...))
    ∧
    (subst-goal g_2 ((x_1 t_1_sub) ...))
    tag)]
  [(subst-goal (∃ d g tag) ((x_1 t_1_sub) ...))
   (∃ d
      (subst-goal g (drop-subst-for d ((x_1 t_1_sub) ...)))
      tag)])

(module+ test

  (check-equal? (term (walk u:0 ((u:0 (sym "a"))))) (term (sym "a")))

  ;; triangular: 0 ↦ 1, 1 ↦ "z"
  (check-equal? (term (walk u:1 ((u:0 (sym "z")) (u:1 u:0))))
				(term (sym "z")))

  ;; no change on constants or pairs
  (check-equal? (term (walk (sym "x") ((u:0 (sym "a"))))) (term (sym "x")))

  (check-equal? (term (walk (u:2 : (sym "q")) ((u:2 (sym "p")))))
                (term (u:2 : (sym "q"))))

  (check-equal?
   (term (fresh-substitution () (x:0)))
   (term ((x:0 u:0))))

  (define fs-pairs
    (term (fresh-substitution (u:0 u:1) (x:0 x:1 x:2))))
  (check-equal? (length fs-pairs) 3)
  (check-equal? (map first fs-pairs) '(x:0 x:1 x:2))
  (check-true (andmap (lambda (pr) (redex-match? L0 u (second pr))) fs-pairs))
  (check-false (ormap (lambda (pr) (member (second pr) '(u:0 u:1))) fs-pairs))

  (check-equal?
   (term (subst-goal (x:0 =? (x:1 : u:3) (label "t"))
                     ((x:0 u:0) (x:1 (sym "a")))))
   (term (u:0 =? ((sym "a") : u:3) (label "t"))))

  ;; Bound variables are not substituted under ∃.
  (check-equal?
   (term (subst-goal (∃ (x:0) (x:0 =? x:1 (label "t1")) (label "f"))
                     ((x:0 u:0) (x:1 u:1))))
   (term (∃ (x:0) (x:0 =? u:1 (label "t1")) (label "f"))))

)

(define-relation L0
  occurs? ⊆ u × t × sub
  [(occurs? u (t : _) sub) (occurs? u (walk t sub) sub)]
  [(occurs? u (_ : t) sub) (occurs? u (walk t sub) sub)]
  [(occurs? u_1 u_1 sub)])

(module+ test
  ;; direct self
  (check-true  (judgment-holds (occurs? u:0 u:0 ())))
  (check-false (judgment-holds (occurs? u:0 u:1 ())))

  ;; list spine
  (check-true  (judgment-holds (occurs? u:0 (u:0 : (sym "x")) ())))
  (check-true  (judgment-holds (occurs? u:0 ((sym "x") : u:0) ())))
  (check-true  (judgment-holds (occurs? u:0 ((sym "x") : (u:1 : empty)) ((u:1 u:0)))))
  (check-false (judgment-holds (occurs? u:0 ((sym "x") : (sym "y")) ())))

)


(define-metafunction L0
  extend : u t sub -> maybe-sub
  [(extend u t sub) #f
   (side-condition (judgment-holds (occurs? u t sub)))]
  [(extend u t sub) ([u t] ,@(term sub))
   (side-condition (not (judgment-holds (occurs? u t sub))))])

(module+ test
  ;; ok when variable doesn't occur
  (check-equal? (term (extend u:0 (sym "cat") ()))
                (term ((u:0 (sym "cat")))))
  ;; ok when variable doesn't occur
  (check-equal? (term (extend u:0 (sym "cat") ((u:1 u:2))))
                (term ((u:0 (sym "cat")) (u:1 u:2))))
  ;; fails on direct self-occurs
  (check-equal? (term (extend u:0 (u:0 : (sym "x")) ()))
                (term #f))
  ;; fails when u occurs on the right
  (check-equal? (term (extend u:0 ((sym "x") : (u:0 : empty)) ()))
                (term #f))
  ;; fails when u occurs viz sub
  (check-equal? (term (extend u:0 ((sym "x") : (u:1 : empty)) ((u:1 u:0))))
                (term #f))
)


(define-metafunction L0
  unify : t t sub -> maybe-sub
  [(unify u_1 u_1 sub) sub]
  [(unify u t sub) (extend u t sub)]
  [(unify t u sub) (extend u t sub)]
  [(unify (t_1a : t_1b) (t_2a : t_2b) sub)
   (unify (walk t_1b sub_1) (walk t_2b sub_1) sub_1)
   (where sub_1 (unify (walk t_1a sub) (walk t_2a sub) sub))]
  [(unify t_1 t_1 sub) sub]
  [(unify _ _ _) #f])

(module+ test

  (check-equal? (term (unify u:0 u:0 ())) (term ()))
  (check-equal? (term (unify u:0 (sym "cat") ())) (term ((u:0 (sym "cat")))))
  (check-equal? (term (unify (sym "dog") u:1 ())) (term ((u:1 (sym "dog")))))
  ;; variable/variable orientation (extend the second in (unify t u ...))
  (check-equal? (term (unify u:0 u:1 ()))
                (term ((u:0 u:1))))

  ;; pair/pair same-shape success
  (check-equal?
   (term (unify ((sym "a") : u:0) ((sym "a") : (sym "b")) ()))
   (term ((u:0 (sym "b")))))

  (check-equal?
   (term (unify ((sym "a") : u:1) (u:1 : u:0) ()))
   (term ((u:0 (sym "a")) (u:1 (sym "a")))))

  (check-equal?
   (term (unify (u:0 : u:1) (u:1 : (sym "a")) ()))
   (term ((u:1 (sym "a")) (u:0 u:1))))

  ;; mismatched head fails
  (check-equal?
   (term (unify ((sym "a") : empty) ((sym "b") : empty) ()))
   (term #f))

)
