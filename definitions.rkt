#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)
(require redex-etc)
#;(current-traced-metafunctions 'all)

(provide L unify walk extend fresh-sub occurs? in-hole-to-goal)

;; Jason Hemann
;; Initial redex lang setup from Ryan Jung
;; Unify &c metafunctions from Phil Nguyen

;; Consider, if we separate answer streams from search tree
;; disjuncts, then we would need some rule to "move into the
;; answer stream."

;; Right now we pun between a succeed node in the language and a
;; successful result, with that substitution. Not a sin.

;; I could also think about this as though the query is instead the
;; one and only call to an initial, implicitly define defrel called
;; "main".


;                                                                                        
;                                                                                        
;                                                                                        
;                                                                                        
;      ;;;;;;   ;;;;;;;        ;      ;;;       ;;;  ;;;       ;;;      ;      ;;;;;;;   
;    ;;     ;    ;     ;       ;       ;;       ;;    ;;       ;;       ;       ;     ;  
;    ;      ;    ;     ;      ; ;      ; ;     ; ;    ; ;     ; ;      ; ;      ;     ;  
;   ;            ;     ;      ; ;      ; ;     ; ;    ; ;     ; ;      ; ;      ;     ;  
;   ;            ;    ;      ;   ;     ; ;     ; ;    ; ;     ; ;     ;   ;     ;    ;   
;   ;            ;;;;;       ;   ;     ;  ;   ;  ;    ;  ;   ;  ;     ;   ;     ;;;;;    
;   ;     ;;;;   ;   ;       ;   ;     ;  ;   ;  ;    ;  ;   ;  ;     ;   ;     ;   ;    
;   ;       ;    ;    ;     ;;;;;;;    ;   ; ;   ;    ;   ; ;   ;    ;;;;;;;    ;    ;   
;    ;      ;    ;    ;     ;     ;    ;   ; ;   ;    ;   ; ;   ;    ;     ;    ;    ;   
;    ;;     ;    ;     ;    ;     ;    ;    ;    ;    ;    ;    ;    ;     ;    ;     ;  
;      ;;;;;    ;;;    ;;;;;;;   ;;;; ;;;   ;   ;;;  ;;;   ;   ;;; ;;;;   ;;;; ;;;    ;;;
;                                                                                        
;                                                                                        
;                                                                                        


(define-language L
  [p (prog Γ e)]   ; Programs, Relation Environments, and Relations
  [Γ ((r_!_ d g) ...)] ; Ensure that 'ri's are distinct
  [d (x_!_ ...)] ; Distinct variable declarations
  ;------------------------------------
  ; Expressions
  [e ()
     s
     ((⊤ σ) ∨ e)]

  ; Search Trees
  [s ()
     #; (⊥ #f) ; What is this?
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
     (r t ...)       ; Relation call
     (∃ d g)]    ; Variable introduction

  ;Terms
  [t c
     o  ;; for "other", change to make c constant and n natural
     x
     empty
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

  [prog-val (prog Γ v)]

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
  (∃ x ... g #:refers-to (shadow x ...))
  (prog ((r x ... g #:refers-to (shadow x ...)) ...) #:refers-to (shadow r ...) e #:refers-to (shadow r ...)))

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

(define-metafunction L
  in-hole-to-goal : g -> any
  [(in-hole-to-goal g) (in-hole EΓ (in-hole Ev (in-hole Es g)))])

(define-relation L
  occurs? ⊆ natural × t × sub
  [(occurs? natural (t : _) sub) (occurs? natural t sub)]
  [(occurs? natural (_ : t) sub) (occurs? natural t sub)]
  [(occurs? natural_1 natural_1 sub)])





