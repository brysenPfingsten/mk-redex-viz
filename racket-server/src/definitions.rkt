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

(define-language Core
  ;--------------------Top Level-------------------------
  [config (Γ Ans s)]    ; Program

  [Γ ((r_!_ d g) ...)]  ; Relation Environment w/ distinct relation names
  [d (x_!_ ...)]        ; Distinct variable declarations
  [Ans (σ ...)]

  ;-------------------Search Trees------------------------
  [s (empty-tree)               ; Empty Tree / Failure
     (g σ)                      ; Goal-State
     (s × g)                    ; Conjunction

     ;; ((⊤ σ) + s)                ; Answer Stream

     ;; (s +-> s)                  ; Right Disjunciton
     ;; (s <-+ s)                  ; Left Disjunction
     ;; (proceed ((r t ... o) σ))  ; Proceed
     ;; (delay s)

  ]                 ; Delay

  ;----------------------Goals----------------------------
  [eq (t =? t tag)] ; Syntactic equality w/ tag

  [g eq
     (succeed)
     (∃ d g tag)     ; Variable introduction w/ tag
     (g ∧ g tag)     ; Conjunction w/ tag
     ;(r t ... tag)   ; Relation call w/ tag
     ;(g ∨ g tag)     ; Disjunction w/ tag

  ]
  ;----------------------Terms---------------------------
  [t x             ; Parameters
     u             ; logic vars
     (sym string)  ; String contants
     (nat number)  ; Numeric constants
     boolean
     string
     empty         ; Empty list
     (t : t)       ; Non-empty list
   ]


  ;----------------------Other---------------------------
  [r (variable-prefix r:)]  ; to account for arbitrary relation names
  [x (variable-prefix x:)]  ; to account for arbitrary parameter names
  [u (variable-prefix u:)]  ; Logic variables – tagged, not raw naturals
  [tag (label string)]

  [σ (state sub c trail tag)] ; State
  [sub ((u t) ...)]   ; Substitution
  [maybe-sub sub #f]
  [trail (eq ...)]
  [end-config (Γ Ans (empty-tree))]
  [c natural]
  ;-----------------Evaluation Contexts------------------

  ; Search Tree
  [Es hole
      (Es × g)
      ;; (Es <-+ s)
      ;; (s +-> Es)
  ]

  ;---------------------Binding Forms--------------------
  #:binding-forms
  (∃ (x ...) g #:refers-to (shadow x ...))
  (e #:refers-to (shadow r ...) ((r (x ...) g #:refers-to (shadow x ...)) ...) #:refers-to (shadow r ...))
)

(default-language Core)

(define-metafunction Core
  unify : t t sub -> maybe-sub
  [(unify u_1 u_1 sub) sub]
  [(unify u t sub) (extend u t sub)]
  [(unify t u sub) (extend u t sub)]
  [(unify (t_1a : t_1b) (t_2a : t_2b) sub)
   (unify (walk t_1b sub_1) (walk t_2b sub_1) sub_1)
   (where sub_1 (unify (walk t_1a sub) (walk t_2a sub) sub))]
  [(unify t_1 t_1 sub) sub]
  [(unify _ _ _) #f])

(define-metafunction Core
  walk : t sub -> t
  [(walk u (name sub (_ ... [u t] _ ...))) (walk t sub)]
  [(walk t _) t])

(define-metafunction Core
  extend : u t sub -> maybe-sub
  [(extend u t sub) ([u t] ,@(term sub))
                          (side-condition (not (term (occurs? u t sub))))]
  [(extend _ _ _) #f])

  ;; produce the mapping between lexical vars an the numbers for logic vars
(define-metafunction Core
  fresh-sub : c x ... -> ((x c) ...)
  [(fresh-sub c) ()]
  [(fresh-sub c x_1 x_2 ...)
   ,(cons (term (x_1 c)) (term (fresh-sub ,(add1 (term c)) x_2 ...)))])

(define-relation Core
  occurs? ⊆ u × t × sub
  [(occurs? u (t : _) sub) (occurs? u t sub)]
  [(occurs? u (_ : t) sub) (occurs? u t sub)]
  [(occurs? u_1 u_1 sub)])
