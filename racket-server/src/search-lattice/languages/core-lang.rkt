#lang racket

(require redex/reduction-semantics)

(provide core-lang
         c-append
         unify
         walk
         extend
         occurs?
         invalid?
         fresh-substitution)

(check-redundancy #t)

(define-language core-lang
  [cfg w
       (head + cfg)]
  [d (x_!_ ...)]

  [f w
     (head + f)]

  [obs (empty-tree)
       head
       (head + obs)]

  [head cell
        Bounced
        (Freshened c tag obs)]
  [cell (⊤ σ)]

  [w (empty-tree)
     (g σ)
     (f × g c)
     (⊤ σ)
     (Freshened c tag cfg)]

  [eq (t =? t tag)]
  [neq (t != t tag)]

  [g eq
     neq
     (succeed tag)
     (fail tag)
     (∃ d g tag)
     (g ∧ g tag)]

  [t x
     u
     pt
     (t : t)]

  [pt (sym string)
      (nat number)
      boolean
      (str string)
      empty]

  [x (variable-prefix x:)]
  [u (variable-prefix u:)]
  [tag (label string)]

  [σ (state sub dis c trail tag)]
  [sub ((u_!_ t) ...)]
  [dis ((t t) ...)]
  [maybe-sub sub #f]
  [trail (eq ...)]
  [c (u_!_ ...)]

  ;; Base active-work context.
  [K ::= hole
         (K × g c)]
  [Q ::= hole
         (head + Q)
         (Freshened c tag Q)]
  [P ::= hole
         (head + P)
         (Freshened c tag P)]

  #:binding-forms
  (∃ (x ...) g #:refers-to (shadow x ...)))

(define-metafunction core-lang
  walk : t sub -> t
  [(walk u (name sub (_ ... [u t] _ ...))) (walk t sub)]
  [(walk t _) t])

(define-metafunction core-lang
  invalid? : sub dis -> boolean
  [(invalid? sub ()) #f]
  [(invalid? sub ((t_1 t_2) (t_3 t_4) ...))
   #t
   (where sub (unify (walk t_1 sub) (walk t_2 sub) sub))]
  [(invalid? sub ((t_1 t_2) (t_3 t_4) ...))
   (invalid? sub ((t_3 t_4) ...))])

(define (fresh-u-symbol used [n 0])
  (define u
    (string->symbol (format "u:~a" n)))
  (cond
    [(member u used) (fresh-u-symbol used (add1 n))]
    [else u]))

(define-metafunction core-lang
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

(define-metafunction core-lang
  c-append : c c -> c
  [(c-append (u_1 ...) (u_2 ...))
   (u_1 ... u_2 ...)])

(define-relation core-lang
  occurs? ⊆ u × t × sub
  [(occurs? u (t : _) sub) (occurs? u (walk t sub) sub)]
  [(occurs? u (_ : t) sub) (occurs? u (walk t sub) sub)]
  [(occurs? u_1 u_1 sub)])

(define-metafunction core-lang
  extend : u t sub -> maybe-sub
  [(extend u t sub) #f
   (side-condition (judgment-holds (occurs? u t sub)))]
  [(extend u t sub) ([u t] ,@(term sub))
   (side-condition (not (judgment-holds (occurs? u t sub))))])

(define-metafunction core-lang
  unify : t t sub -> maybe-sub
  [(unify u_1 u_1 sub) sub]
  [(unify u t sub) (extend u t sub)]
  [(unify t u sub) (extend u t sub)]
  [(unify (t_1a : t_1b) (t_2a : t_2b) sub)
   (unify (walk t_1b sub_1) (walk t_2b sub_1) sub_1)
   (where sub_1 (unify (walk t_1a sub) (walk t_2a sub) sub))]
  [(unify t_1 t_1 sub) sub]
  [(unify _ _ _) #f])
