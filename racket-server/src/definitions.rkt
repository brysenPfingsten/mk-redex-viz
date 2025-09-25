#lang racket
(require redex/reduction-semantics
         redex/pict)

(check-redundancy #t)

#;(current-traced-metafunctions 'all)

(provide L unify walk extend fresh-sub occurs?)

;; Jason Hemann
;; Initial redex lang setup from Ryan Jung
;; Unify &c metafunctions from Phil Nguyen

;; Consider, if we separate answer streams from search tree
;; disjuncts, then we would need some rule to "move into the
;; answer stream."

;; Right now we pun between a succeed node in the language and a
;; successful result, with that substitution. Not a sin.

(define-language L
  ;--------------------Top Level-------------------------
  [p (e Γ)]             ; Program
  [Γ ((r_!_ d g) ...)]  ; Relation Environment w/ distinct relation names
  [d (x_!_ ...)]        ; Distinct variable declarations
  
  ;--------------------Expressions-------------------------
  [e ()               ; Empty Tree / Failure
     (⊤ σ)            ; Singleton Answer
     s                ; Search Tree
     ((⊤ σ) + e)]     ; Answer Stream

  ;-------------------Search Trees------------------------
  [s ()                         ; Empty Tree / Failure
     (g σ)                      ; Goal-State
     (∂ s maybe-state)          ; Partially Evaluated Tree
     (s +-> s)                  ; Right Disjunciton
     (s <-+ s)                  ; Left Disjunction
     ((⊤ σ) + s)                ; Answer Stream
     (s × g)                    ; Conjunction
     (proceed ((r t ... o) σ))  ; Proceed
     (delay s)]                 ; Delay

  ;----------------------Goals----------------------------
  [g ⊤
     (t =? t o)    ; Syntactic equality w/ tag
     (r t ... o)   ; Relation call w/ tag
     (g ∨ g o)     ; Disjunction w/ tag
     (g ∧ g o)     ; Conjunction w/ tag
     (∃ d g o)]    ; Variable introduction w/ tag

  ;----------------------Terms---------------------------
  [t c              ; Logic variables
     (sym string)   ; String
     (nat natural)  ; Naturals
     boolean
     string
     x        ; Parameters
     empty    ; Empty list
     (t : t)] ; Non-empty list


  ;----------------------Other---------------------------
  [r (variable-prefix r:)]  ; to account for arbitrary relation names
  [x (variable-prefix x:)]  ; to account for arbitrary parameter names
  [c natural]               ; Logic variables
  [o (sym string)           ; Tagged string  = symbol
     (nat natural)          ; Tagged natural = natural (not logic variable)
     boolean
     string]
  [σ (state sub c trail o)] ; State
  [sub ((natural t) ...)]   ; Substitution
  [maybe-sub sub #f]
  [maybe-state σ #f]
  [trail ((t =? t o) ...)]


  ;----------------------Values--------------------------
  [v ()           ; Empty Node
     (⊤ σ)        ; Singleton Node
     ((⊤ σ) + v)] ; Answer Disjunct

  [prog-val (prog Γ v)]


  ;-----------------Evaluation Contexts------------------

  ; Answer Stream
  [Ev hole
      ((⊤ σ) + Ev)]

  ; Search Tree
  [Es hole
      (Es <-+ s)
      (s +-> Es)
      (Es × g)]

  ;; Prog to first tree w/o sub-tree
  [Ex (in-hole Ev (in-hole Es hole))]

  ;---------------------Binding Forms--------------------
  #:binding-forms
  (∃ (x ...) g #:refers-to (shadow x ...))
  (e #:refers-to (shadow r ...) ((r (x ...) g #:refers-to (shadow x ...)) ...) #:refers-to (shadow r ...))
)

(default-language L)

(define-metafunction L
  unify : t t sub -> maybe-sub
  [(unify natural_1 natural_1 sub) sub]
  [(unify natural t sub) (extend natural t sub)]
  [(unify t natural sub) (extend natural t sub)]
  [(unify (t_1a : t_1b) (t_2a : t_2b) sub)
   (unify (walk t_1b sub_1) (walk t_2b sub_1) sub_1)
   (where sub_1 (unify (walk t_1a sub) (walk t_2a sub) sub))]
  [(unify t_1 t_1 sub) sub]
  [(unify _ _ _) #f])

(define-metafunction L
  walk : t sub -> t
  [(walk natural (name sub (_ ... [natural t] _ ...))) (walk t sub)]
  [(walk t _) t])
  
(define-metafunction L
  extend : natural t sub -> maybe-sub
  [(extend natural t sub) ([natural t] ,@(term sub))
                          (side-condition (not (term (occurs? natural t sub))))]
  [(extend _ _ _) #f])

(define-metafunction L
  fresh-sub : c any ... -> any
  [(fresh-sub c) ()]
  [(fresh-sub c x_1 x_2 ...)
   ,(cons (term (x_1 c)) (term (fresh-sub ,(add1 (term c)) x_2 ...)))])

(define-relation L
  occurs? ⊆ natural × t × sub
  [(occurs? natural (t : _) sub) (occurs? natural t sub)]
  [(occurs? natural (_ : t) sub) (occurs? natural t sub)]
  [(occurs? natural_1 natural_1 sub)])
