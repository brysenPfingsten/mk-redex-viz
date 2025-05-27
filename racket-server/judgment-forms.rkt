#lang racket
(require redex)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)
(require redex-etc)

(provide closed-goal? closed-tree? closed-term?
         closed-sub? closed-program?)
(require "definitions.rkt")

(define-judgment-form
  L
  #:contract (closed-term? t (x ...) c)
  #:mode (closed-term? I I I)

  [
   ----------------- "empty is closed"
   (closed-term? empty (x ...) c)]

  [#;(side-condition ,(< (term c_1) (term c_2)))
   -------------- "logic var is closed"
   (closed-term? c_1 (x ...) c_2)]

  [
   -------------- "primitive is closed"
   (closed-term? o (x ...) c)]

  [(closed-term? t_2 (x ...) c)
   (closed-term? t_1 (x ...) c)
   -------------- "list is closed"
   (closed-term? (t_1 : t_2) (x ...) c)]

  [
   -------------- "lexical var is closed"
   (closed-term? x_2 (x_1 ... x_2 x_3 ...) c)])

(define-judgment-form
  L
  #:contract (closed-trail? trail c)
  #:mode (closed-trail? I I)

  [
   ------------------ "empty trail is closed"
   (closed-trail? () c)]

  [(closed-term? t_1 () c)
   (closed-term? t_2 () c)
   (closed-trail? ((t_3 =? t_4 o) ...) c) 
   ------------------ "trail is closed"
  (closed-trail? ((t_1 =? t_2 _) (t_3 =? t_4 o) ...) c)])

  
(define-judgment-form
  L
  #:contract (closed-sub? sub c)
  #:mode (closed-sub? I I)
  [
   ------------------ "empty sub is closed"
   (closed-sub? () c)]

  [(closed-term? t_1 () c_3)
   (closed-term? c_1 () c_3)
   (closed-sub? ((c_2 t_2)) c_3) ...
   ------------------"sub is closed"
   (closed-sub? ((c_1 t_1) (c_2 t_2) ...) c_3)])

(define-judgment-form
  L
  #:contract (closed-goal? g (r ...) (x ...) c)
  #:mode (closed-goal? I I I I)

  [
   ------------------ "trivial success closed"
   (closed-goal? ⊤ (r ...) (x ...) c)]

  [(closed-goal? g (r ...) (x_1 ... x_2 ...) ,(+ (length (term (x_1 ...))) (term c)))
   ------------------- "fresh-closed"
   (closed-goal? (∃ (x_1 ...) g _) (r ...) (x_2 ...) c)]
  
  [(closed-goal? g_1 (r ...) (x ...) c)
   (closed-goal? g_2 (r ...) (x ...) c)
   ---------- "conj-closed"
   (closed-goal? (g_1 ∧ g_2 _) (r ...) (x ...) c)]
  
  [(closed-goal? g_1 (r ...) (x ...) c)
   (closed-goal? g_2 (r ...) (x ...) c)
   ---------- "disj-closed"
   (closed-goal? (g_1 ∨ g_2 _) (r ...) (x ...) c)]

  [(closed-term? t_1 (x ...) c)
   (closed-term? t_2 (x ...) c)
   ---------- "==-closed"
   (closed-goal? (t_1 =? t_2 _) (r ...) (x ...) c)]
  
  [(closed-term? t (x ...) c) ...
   ---------- "relcall-closed"
   (closed-goal? (r_1 t ... _) (r_2 ... r_1 r_3 ...) (x ...) c)]
  )

(define-judgment-form
  L
  #:contract (closed-tree? s (r ...))
  #:mode (closed-tree? I I)

  [
   -------------------"empty tree is closed"
   (closed-tree? () (r ...))]

  [
   ------------------"trivial success is closed"
   (closed-tree? ⊤ (r ...))]

  [(closed-goal? g (r ...) () c)
   (side-condition ,(andmap (λ (pair) (< (first pair) (term c))) (term sub)))
   (side-condition ,(<= (length (term sub)) (term c)))
   (closed-sub? sub c)
   (closed-trail? trail c)
   -------------------"goal w/ sub closed"
   (closed-tree? (g (state sub c trail)) (r ...))]

  [(closed-tree? s_1 (r ...))
   (closed-tree? s_2 (r ...))
   -------------------"left disj closed"
   (closed-tree? (s_1 <-+ s_2) (r ...))]

  [(closed-tree? s_1 (r ...))
   (closed-tree? s_2 (r ...))
   -------------------"right disj closed"
   (closed-tree? (s_1 +-> s_2) (r ...))]

  [(closed-tree? s (r ...))
   (closed-goal? g (r ...) () 0)
   -------------------"conj closed"
   (closed-tree? (s × g) (r ...))]

  [(closed-tree? s (r ...))
   -------------------"delay closed"
   (closed-tree? (delay s) (r ...))]

  [(closed-tree? s (r ...))
   -------------------"proceed closed"
   (closed-tree? (proceed s) (r ...))])


(define-judgment-form
  L
  #:contract (closed-program? p)
  #:mode (closed-program? I)
  [(closed-goal? g (r ...) (x ...) 0) ...
   (closed-tree? s (r ...))
   ----------------------- "program-closed"
   (closed-program? (prog ((r (x ...) g) ...) s))]
  )
