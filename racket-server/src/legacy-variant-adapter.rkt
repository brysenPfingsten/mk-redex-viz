#lang racket

(require racket/match
         racket/list
         redex/reduction-semantics
         "definitions.rkt"
         "extensions/l4-railroad-syntax.rkt")

(provide legacy-program->l4-config
         l4-config->legacy-program
         l4-config?
         canonical-target-id
         canonical-parser-profile
         canonical-config?
         legacy-program->canonical-config
         canonical-config->legacy-program
         legacy-tag->label)

;; Canonical backend target for parser/transpiler output.
;; All current surface profiles normalize into this shape.
(define canonical-target-id "L4/config")
(define canonical-parser-profile "surface->l4")

(define u-rx #px"^u:([0-9]+)$")
(define r-rx #px"^r:")

(define (u-symbol n)
  (string->symbol (format "u:~a" n)))

(define (u-symbol? x)
  (and (symbol? x)
       (regexp-match? u-rx (symbol->string x))))

(define (u->natural u)
  (define m (and (symbol? u) (regexp-match u-rx (symbol->string u))))
  (if m
      (string->number (second m))
      0))

(define (legacy-tag->label o)
  (match o
    [`(label ,_) o]
    [`(sym ,s) `(label ,s)]
    [`(nat ,n) `(label ,(number->string n))]
    [(? boolean? b) `(label ,(if b "true" "false"))]
    [(? string? s) `(label ,s)]
    [(? symbol? s) `(label ,(symbol->string s))]
    [_ `(label ,(format "~a" o))]))

(define (label->legacy-tag tag)
  (match tag
    [`(label ,s) s]
    [_ (format "~a" tag)]))

(define (legacy-term->core t)
  (match t
    ['empty 'empty]
    [`(,a : ,d) `(,(legacy-term->core a) : ,(legacy-term->core d))]
    [`(sym ,s) `(sym ,s)]
    [`(nat ,n) `(nat ,n)]
    [(? boolean? b) b]
    [(? number? n) (u-symbol n)]
    [(? string? s) `(str ,s)]
    [(? symbol? s) s]
    [_ (error 'legacy-term->core "unhandled legacy term ~a" t)]))

(define (core-term->legacy t)
  (match t
    ['empty 'empty]
    [`(,a : ,d) `(,(core-term->legacy a) : ,(core-term->legacy d))]
    [`(sym ,s) `(sym ,s)]
    [`(nat ,n) `(nat ,n)]
    [`(str ,s) s]
    [(? boolean? b) b]
    [(? symbol? s) (if (u-symbol? s) (u->natural s) s)]
    [_ (error 'core-term->legacy "unhandled core term ~a" t)]))

(define (legacy-c->core c)
  (cond
    [(number? c) (for/list ([i (in-range c)]) (u-symbol i))]
    [(list? c) (map (lambda (u) (if (number? u) (u-symbol u) u)) c)]
    [else '()]))

(define (core-c->legacy c)
  (if (null? c)
      0
      (for/fold ([mx 0]) ([u (in-list c)])
        (max mx (add1 (u->natural u))))))

(define (legacy-goal->core g)
  (match g
    ['⊤ `(succeed (label "legacy-top"))]
    [`(,t1 =? ,t2 ,o)
     `(,(legacy-term->core t1) =? ,(legacy-term->core t2) ,(legacy-tag->label o))]
    [`(,g1 ∨ ,g2 ,o)
     `(,(legacy-goal->core g1) ∨ ,(legacy-goal->core g2) ,(legacy-tag->label o))]
    [`(,g1 ∧ ,g2 ,o)
     `(,(legacy-goal->core g1) ∧ ,(legacy-goal->core g2) ,(legacy-tag->label o))]
    [`(∃ ,d ,g1 ,o)
     `(∃ ,d ,(legacy-goal->core g1) ,(legacy-tag->label o))]
    [(list* r rest)
     #:when (and (symbol? r)
                 (regexp-match? r-rx (symbol->string r))
                 (pair? rest))
     (define o (last rest))
     (define ts (drop-right rest 1))
     `(,r ,@(map legacy-term->core ts) ,(legacy-tag->label o))]
    [_ (error 'legacy-goal->core "unhandled legacy goal ~a" g)]))

(define (core-goal->legacy g)
  (match g
    [`(succeed ,_tag) '⊤]
    [`(,t1 =? ,t2 ,tag)
     `(,(core-term->legacy t1) =? ,(core-term->legacy t2) ,(label->legacy-tag tag))]
    [`(,g1 ∨ ,g2 ,tag)
     `(,(core-goal->legacy g1) ∨ ,(core-goal->legacy g2) ,(label->legacy-tag tag))]
    [`(,g1 ∧ ,g2 ,tag)
     `(,(core-goal->legacy g1) ∧ ,(core-goal->legacy g2) ,(label->legacy-tag tag))]
    [`(∃ ,d ,g1 ,tag)
     `(∃ ,d ,(core-goal->legacy g1) ,(label->legacy-tag tag))]
    [(list* r rest)
     #:when (and (symbol? r)
                 (regexp-match? r-rx (symbol->string r))
                 (pair? rest))
     (define tag (last rest))
     (define ts (drop-right rest 1))
     `(,r ,@(map core-term->legacy ts) ,(label->legacy-tag tag))]
    [_ (error 'core-goal->legacy "unhandled core goal ~a" g)]))

(define (legacy-state->core st)
  (match st
    [`(state ,sub ,c ,trail ,o)
     `(state ,(for/list ([pr (in-list sub)])
                (match pr
                  [`(,u ,t) (list (if (number? u) (u-symbol u) u) (legacy-term->core t))]
                  [_ (error 'legacy-state->core "bad substitution pair ~a" pr)]))
             ,(legacy-c->core c)
             ,(for/list ([eq (in-list trail)])
                (match eq
                  [`(,t1 =? ,t2 ,o1)
                   `(,(legacy-term->core t1) =? ,(legacy-term->core t2) ,(legacy-tag->label o1))]
                  [_ (error 'legacy-state->core "bad trail eq ~a" eq)]))
             ,(legacy-tag->label o))]
    [_ (error 'legacy-state->core "unhandled legacy state ~a" st)]))

(define (core-state->legacy st)
  (match st
    [`(state ,sub ,c ,trail ,tag)
     `(state ,(for/list ([pr (in-list sub)])
                (match pr
                  [`(,u ,t) (list (if (u-symbol? u) (u->natural u) u) (core-term->legacy t))]
                  [_ (error 'core-state->legacy "bad substitution pair ~a" pr)]))
             ,(core-c->legacy c)
             ,(for/list ([eq (in-list trail)])
                (match eq
                  [`(,t1 =? ,t2 ,tag1)
                   `(,(core-term->legacy t1) =? ,(core-term->legacy t2) ,(label->legacy-tag tag1))]
                  [_ (error 'core-state->legacy "bad trail eq ~a" eq)]))
             ,(label->legacy-tag tag))]
    [_ (error 'core-state->legacy "unhandled core state ~a" st)]))

(define (first-c-in-core-tree s)
  (match s
    [`(,g (state ,_sub ,c ,_trail ,_tag)) c]
    [`(⊤ (state ,_sub ,c ,_trail ,_tag)) c]
    [`(,s1 × ,_g ,_c) (first-c-in-core-tree s1)]
    [`(,s1 <-+ ,s2) (or (first-c-in-core-tree s1) (first-c-in-core-tree s2))]
    [`(,s1 +-> ,s2) (or (first-c-in-core-tree s1) (first-c-in-core-tree s2))]
    [`(delay ,s1) (first-c-in-core-tree s1)]
    [`(proceed (,g ,_σ)) (if (equal? g 'empty-tree) '() '())]
    [`(proceed ((,r ,_t ... ,_tag) (state ,_sub ,c ,_trail ,_tag2))) c]
    [_ #f]))

(define (legacy-tree->core s)
  (match s
    ['() '(empty-tree)]
    ['(empty-tree) '(empty-tree)]
    [`(⊤ ,σ) `(⊤ ,(legacy-state->core σ))]
    [`(∂ ,s1 ,_maybe-state) (legacy-tree->core s1)]
    [`(,s1 <-+ ,s2) `(,(legacy-tree->core s1) <-+ ,(legacy-tree->core s2))]
    [`(,s1 +-> ,s2) `(,(legacy-tree->core s1) +-> ,(legacy-tree->core s2))]
    [`((⊤ ,σ) + ,s1) `((⊤ ,(legacy-state->core σ)) <-+ ,(legacy-tree->core s1))]
    [`(,s1 × ,g)
     (define s1* (legacy-tree->core s1))
     (define captured-c (or (first-c-in-core-tree s1*) '()))
     `(,s1* × ,(legacy-goal->core g) ,captured-c)]
    [`(proceed ((,r ,ts ... ,o) ,σ))
     `(proceed ((,r ,@(map legacy-term->core ts) ,(legacy-tag->label o))
                ,(legacy-state->core σ)))]
    [`(delay ,s1) `(delay ,(legacy-tree->core s1))]
    [`(,g ,σ) `(,(legacy-goal->core g) ,(legacy-state->core σ))]
    [_ (error 'legacy-tree->core "unhandled legacy tree ~a" s)]))

(define (core-tree->legacy s)
  (match s
    ['(empty-tree) '()]
    [`(⊤ ,σ) `(⊤ ,(core-state->legacy σ))]
    [`(,s1 <-+ ,s2) `(,(core-tree->legacy s1) <-+ ,(core-tree->legacy s2))]
    [`(,s1 +-> ,s2) `(,(core-tree->legacy s1) +-> ,(core-tree->legacy s2))]
    [`(,s1 × ,g ,_c) `(,(core-tree->legacy s1) × ,(core-goal->legacy g))]
    [`(proceed ((,r ,ts ... ,tag) ,σ))
     `(proceed ((,r ,@(map core-term->legacy ts) ,(label->legacy-tag tag))
                ,(core-state->legacy σ)))]
    [`(proceed (,g ,σ))
     `(,(core-goal->legacy g) ,(core-state->legacy σ))]
    [`(delay ,s1) `(delay ,(core-tree->legacy s1))]
    [`(,g ,σ) `(,(core-goal->legacy g) ,(core-state->legacy σ))]
    [_ (error 'core-tree->legacy "unhandled core tree ~a" s)]))

(define (legacy-env->core gamma)
  (for/list ([defn (in-list gamma)])
    (match defn
      [`(,r ,d ,g) `(,r ,d ,(legacy-goal->core g))]
      [_ (error 'legacy-env->core "bad relation def ~a" defn)])))

(define (core-env->legacy gamma)
  (for/list ([defn (in-list gamma)])
    (match defn
      [`(,r ,d ,g) `(,r ,d ,(core-goal->legacy g))]
      [_ (error 'core-env->legacy "bad relation def ~a" defn)])))

(define (legacy-e->ans+tree e)
  (match e
    ['() (values '() '(empty-tree))]
    [`((⊤ ,σ) + ,e2)
     (define-values (ans tail-tree) (legacy-e->ans+tree e2))
     (values (cons (legacy-state->core σ) ans) tail-tree)]
    [_ (values '() (legacy-tree->core e))]))

(define (ans+tree->legacy-e ans* s)
  (for/fold ([e (core-tree->legacy s)])
            ([σ (in-list (reverse ans*))])
    `((⊤ ,(core-state->legacy σ)) + ,e)))

(define (legacy-program->l4-config prog)
  (match prog
    [`(,e ,gamma)
     (define-values (ans* s) (legacy-e->ans+tree e))
     `(,(legacy-env->core gamma) ,ans* ,s)]
    [_ (if (redex-match? L4 config prog)
           prog
           (error 'legacy-program->l4-config "expected legacy program p, got ~a" prog))]))

(define (l4-config->legacy-program cfg)
  (match cfg
    [`(,gamma ,ans* ,s)
     `(,(ans+tree->legacy-e ans* s) ,(core-env->legacy gamma))]
    [_ (if (redex-match? L p cfg)
           cfg
           (error 'l4-config->legacy-program "expected L4 config, got ~a" cfg))]))

(define (l4-config? cfg)
  (redex-match? L4 config cfg))

(define (canonical-config? cfg)
  (l4-config? cfg))

(define (legacy-program->canonical-config prog)
  (legacy-program->l4-config prog))

(define (canonical-config->legacy-program cfg)
  (l4-config->legacy-program cfg))
