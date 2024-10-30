#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)


(define-language L
  [p ::= (prog Γ e)]   ; Programs, Relation Environments, and Relations
  [Γ ((r_!_ x g) ...)] ; Ensure that 'ri's are distinct
  ;------------------------------------
  ; Expressions
  [e ::=
     ()
     s
     ((⊤ σ) ∨ e)]

  ; Search Trees
  [s ()
     (⊥ #f)
     (g σ)
     (s + s)
     (s × g)
     (delay s)]

  ; Goals
  [g ⊤           ; Trivial success
     ⊥           ; Trivial failure
     (t =? t)    ; Syntactic equality
     (g ∨ g)     ; Disjunction
     (g ∧ g)     ; Conjuction
     (r t)       ; Relation call
     (∃ x g)]    ; Variable introduction

  ;Terms
  [t c
     o ;; for "other", change to make c constant and n natural
     x
     (t : t)]

  ;Other
  [r (variable-prefix r:)] ; to account for arbitrary relation names
  [x (variable-prefix x:)] ; to account for arbitrary parameter names
  [c natural]
  [o ;; symbol ; Why isn't this working
     boolean
     string]
  [σ (state sub c)]
  [sub ((natural t) ...)]
  [maybe-sub sub #f]

  ;-------------------------------------
  ; Values
  [v ()           ; Empty Node
     (⊤ σ)        ; Singleton Node
     ((⊤ σ) + v)] ; Answer Disjunct (yuck the letter v and logical or look the same

  ;-------------------------------------
  ; Evaluation Contexts
  [EΓ (prog Γ hole)]

  ; Answer Stream
  [Ev hole
      ((⊤ σ) + Ev)]

  ; Search Tree
  [Es hole
      (Es + s)
      (Es × g)]

  ; Goal
  [Eg hole
      (Eg ∧ g)
      (Eg ∨ g)]
  #:binding-forms
  (∃ x g #:refers-to x)
  (prog ((r x g #:refers-to x) ...) #:refers-to (shadow r ...) e #:refers-to (shadow r ...)))

(default-language L)