#lang racket

(require redex/reduction-semantics
         (only-in "core/s/language.rkt" core-s-oracle-lang)
         (only-in "kernel.rkt" owners-support))
(provide StrictS context-support/s)

;; Shared strict S grammar and structural ancestry traversal. This module
;; contains no source reduction or machine transition definitions.
;; Owner fields retain local provenance at strict computation/value positions.
;; Yield/Emit Owners are shared by head and residual; Answer Owners apply only
;; to the head. The logical state still has exactly the original four fields.
(define-extended-language StrictS core-s-oracle-lang
  [g .... (g ∨ g tag) (suspend g tag)]
  [a eq (t != t tag) (succeed tag) (fail tag)]
  [SV (Empty owners) (One owners σ)
      (Yield owners A SV) (Delay owners c)]
  [c SV (eval owners g σ) (mplus owners c c) (bind owners c g)
     (Yield owners A c) (force c)]
  [E hole (mplus owners E c) (mplus owners SV E)
     (bind owners E g) (Yield owners A E) (force E)]
  [O (Done owners) (Solo owners σ) (Emit owners A O) (Forced owners O)]
  [F (Done owners) (Solo owners σ) (Emit owners A F) (Forced owners F)
     (More (Delay owners c))]
  [o F (render c) (commit c) (advance o) (collect o)
     (Emit owners A o) (Forced owners o)]
  [q c o]
  [C E (render E) (commit E) (advance C) (collect C)
     (Emit owners A C) (Forced owners C)])

(define (context-support/s context [support '()])
  (match context
    [(? (lambda (value) (equal? value (term hole)))) support]
    [`(mplus ,owners ,left ,right)
     (define prefix (owners-support owners support))
     (if (context-hole? left)
         (context-support/s left prefix)
         (context-support/s right prefix))]
    [`(bind ,owners ,inner ,_) (context-support/s inner (owners-support owners support))]
    [`(,(or 'Yield 'Emit) ,owners ,_ ,inner)
     (context-support/s inner (owners-support owners support))]
    [`(Forced ,owners ,inner)
     (context-support/s inner (owners-support owners support))]
    [`(,(or 'force 'render 'commit 'advance 'collect) ,inner)
     (context-support/s inner support)]
    [`(program ,_ ,inner) (context-support/s inner support)]
    [_ (raise-argument-error 'context-support/s "S evaluation context" context)]))

(define (context-hole? datum)
  (match datum
    [(? (lambda (value) (equal? value (term hole)))) #t]
    [(cons first rest) (or (context-hole? first) (context-hole? rest))]
    [_ #f]))
