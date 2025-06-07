#lang racket
(require minikanren)

(defrel (booleano b)
  (conde
   [(== b #f)]
   [(== b #t)]))

(defrel (listo l)
  (conde
   [(== l 'empty)]
   [(fresh (t_1 t_2)
      (termo t_1)
      (listo t_2)
      (== l `(,t_1 : ,t_2)))]))

(defrel (termo t)
  (conde
   [(numbero t)]
   [(booleano t)]
   [(stringo t)]
   [(listo t)]))

(defrel (goalo g)
  (conde
   [(== g '⊤)]
   [(fresh (t_1 t_2)
      (termo t_1)
      (termo t_2)
      (== g `(,t_1 =? ,t_2)))]))


;  [g ⊤
;     done
;     (t =? t)    ; Syntactic equality
;     (t =? t o)  ; Unfication with a tag
;     (r t ...)   ; Relation call
;     (g ∨ g)     ; Disjunction
;     (g ∧ g)     ; Conjuction
;     (∃ d g)]    ; Variable introduction

(defrel (search-treeo s)
  ...)

;; Search Trees
;[s ()
;   (g σ)
;   (s +-> s)
;   (s <-+ s)
;   ((⊤ σ) + s)
;   (s × g)
;   (delay s)]

(defrel (expressiono e)
  ...)

;[e ()
;   (⊤ σ)
;   s
;   (done-delay e)
;   ((⊤ σ) + e)]




; [p (prog Γ e)]   ; Programs, Relation Environments, and Relations
;  [Γ ((r_!_ d g) ...)] ; Ensure that 'ri's are distinct
;  [d (x_!_ ...)] ; Distinct variable declarations


;[r (variable-prefix r:)] ; to account for arbitrary relation names
;[x (variable-prefix x:)] ; to account for arbitrary parameter names
;[c natural]
;[o ;; symbol ; Why isn't this working
; boolean
; string]
;[σ (state sub c trail)]
;[sub ((natural t) ...)]
;[maybe-sub sub #f]
;[trail ((t =? t o) ...)]


   
