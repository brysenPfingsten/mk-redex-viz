#lang racket

(require redex/reduction-semantics)

(provide core-lang
         c-append
         shellify-tree-prefix
         unify
         walk
         extend
         occurs?
         invalid?
         fresh-substitution)

(check-redundancy #t)

(define-language core-lang
  [cfg search
       (FreshenedShell c cfg tag)]

  [search cell
          (empty-tree)
          runnable-root
          (FreshenedTree c search tag)]

  [runnable-search runnable-root
                   (FreshenedTree c runnable-search tag)]

  [runnable-root (g σ)
                 (search × g c)]

  [d (x_!_ ...)]

  [cell (⊤ σ)]

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
  [trail (eq ...)] ;; what about neq?
  [c (u_!_ ...)]
  [c+ (u u_!_ ...)]
  [summary (wf-summary number number number number)]

  ;; Outer committed shell wrappers. L0 owns the shell/tail split, even though
  ;; shell growth first becomes interesting once later layers add more shell
  ;; constructors.
  [QShell ::= hole
              (FreshenedShell c QShell tag)]

  ;; Pure introduction-provenance chain for scoped phase-boundary focus.
  ;; L0 uses it for conjunction handoff; later layers reuse the same helper for
  ;; delay / answer / fail heads without introducing per-node scoped families.
  ;; First divergent layer: L0/core.
  ;; Allowed extension direction: reuse as pure FreshenedTree* only.
  [QFresh ::= hole
              (FreshenedTree c QFresh tag)]
  ;; One-or-more FreshenedTree frames. Used when a shellification step should
  ;; only fire if it actually has a tree prefix to convert.
  [QFresh+ ::= (FreshenedTree c QFresh tag)]
  ;; One-or-more pending conjunction layers, each optionally wrapped in
  ;; FreshenedTree* before the next outer layer.
  [KConj ::= (KLocal × g c)
             (FreshenedTree c KConj tag)]
  ;; Frozen local-work path used by inherited lower-layer rules.
  ;; First divergent layer: L0/core.
  ;; Allowed extension direction: later policy helpers may branch from it, but
  ;; core itself stays frozen at pure FreshenedTree* bottoms plus conjunction
  ;; layers built around them.
  [KLocal ::= QFresh
              KConj]

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

(define-metafunction core-lang
  shellify-tree-prefix : any -> any
  [(shellify-tree-prefix (FreshenedTree c any_1 tag))
   (FreshenedShell c (shellify-tree-prefix any_1) tag)]
  [(shellify-tree-prefix any_1)
   any_1])

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
