#lang racket
(require redex)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(provide closed-goal? closed-tree? closed-term?
         closed-sub? closed-program?
		 bump)

(require "definitions.rkt")

;; c is a natural; (x ...) is any list of binders/vars.
;; Returns c + (length (x ...)).
(define-metafunction L
  bump : c (x ...) -> c
  [(bump c (x ...))
   ,(+ (term c) (length (term (x ...))))])


(define-judgment-form
  L
  #:contract (same-length? (t ...) (x ...))
  #:mode (same-length? I I)

  [
   ------------"empty list same length"
   (same-length? () ())]

  [(same-length? (t ...) (x ...))
   ------------"cons list same length"
   (same-length? (t_1 t ...) (x_1 x ...))]

  )


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
  ;; Well I suppose what I want to say is that the accumulator contains the same x ... as *some* set of variables in the (x ... ...) list
  #:contract (closed-goal? g ((r (x ...)) ...) (x ... y ...) c)
  #:mode (closed-goal? I I I I)

  [
   ------------------ "trivial success closed"
   (closed-goal? ⊤ headers (x ...) c)]

  [(where c′ (bump c (x_1 ...)))
   (closed-goal? g headers (x_1 ... x_2 ...) c′)
   ------------------- "fresh-closed"
   (closed-goal? (∃ (x_1 ...) g _) headers (x_2 ...) c)]
  
  [(closed-goal? g_1 headers (x ...) c)
   (closed-goal? g_2 headers (x ...) c)
   ---------- "conj-closed"
   (closed-goal? (g_1 ∧ g_2 _) headers (x ...) c)]
  
  [(closed-goal? g_1 headers (x ...) c)
   (closed-goal? g_2 headers (x ...) c)
   ---------- "disj-closed"
   (closed-goal? (g_1 ∨ g_2 _) headers (x ...) c)]

  [(closed-term? t_1 (x ...) c)
   (closed-term? t_2 (x ...) c)
   ---------- "==-closed"
   (closed-goal? (t_1 =? t_2 _) headers (x ...) c)]
  
  [(same-length? (t ...) (x_i ...))
   (closed-term? t (x_k ...) c) ...
   ---------- "relcall-closed"
   (closed-goal? (r_i t ... _) ((r_1 (x_1 ...)) ... (r_i (x_i ...)) (r_j (x_j ...)) ...) (x_k ...) c)]
  )

(define-judgment-form
  L
  ;; How do I distinguish between an ((r (x ...)) (r2 (x_1 ...))) if the lengths x x_1 have to be the same
  #:contract (closed-tree? s ((r (x ...)) ...))
  #:mode (closed-tree? I I)

  [
   -------------------"empty tree is closed"
   (closed-tree? () ((r (x ...)) ...))]

  [
   ------------------"trivial success is closed"
   (closed-tree? ⊤ ((r (x ...)) ...))]

  [(closed-goal? g ((r (x ...)) ...) () c)
   (side-condition ,(andmap (λ (pair) (< (first pair) (term c))) (term sub)))
   (side-condition ,(<= (length (term sub)) (term c)))
   (closed-sub? sub c)
   (closed-trail? trail c)
   -------------------"goal w/ sub closed"
   (closed-tree? (g (state sub c trail _)) ((r (x ...)) ...))]

  [(closed-tree? s ((r (x ...)) ...))
   -------------------"partial tree closed"
   (closed-tree? (∂ s _) ((r (x ...)) ...))] ;; TODO: closed-state-judgement?
  
  [(closed-tree? s_1 ((r (x ...)) ...))
   (closed-tree? s_2 ((r (x ...)) ...))
   -------------------"left disj closed"
   (closed-tree? (s_1 <-+ s_2) ((r (x ...)) ...))]

  [(closed-tree? s_1 ((r (x ...)) ...))
   (closed-tree? s_2 ((r (x ...)) ...))
   -------------------"right disj closed"
   (closed-tree? (s_1 +-> s_2) ((r (x ...)) ...))]

  [(closed-sub? sub c)
   (closed-trail? trail c)
   (closed-tree? s ((r (x ...)) ...))
   -------------------"answer stream closed"
   (closed-tree? ((⊤ (state sub c trail _)) + s) ((r (x ...)) ...))]

  [(closed-tree? s ((r (x ...)) ...))
   (closed-goal? g ((r (x ...)) ...) () 0)
   -------------------"conj closed"
   (closed-tree? (s × g) ((r (x ...)) ...))]

  [(closed-tree? s ((r (x ...)) ...))
   -------------------"delay closed"
   (closed-tree? (delay s) ((r (x ...)) ...))]

  [(closed-tree? s ((r (x ...)) ...))
   -------------------"proceed closed"
   (closed-tree? (proceed s) ((r (x ...)) ...))])


(define-judgment-form
  L
  #:contract (closed-program? p)
  #:mode (closed-program? I)
  [(closed-tree? s ((r (x ...) g) ...))
   (closed-goal? g ((r (x ...) g) ...) (x ...) 0) ...
   ----------------------- "program-closed"
   (closed-program? (prog ((r (x ...) g) ...) s))]
  )
