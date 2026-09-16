#lang racket

(require redex/reduction-semantics
         (only-in "../shared/grammar-s.rkt" StrictS context-support/s)
         (only-in "../shared/relation-grammar.rkt" StrictSRel)
         "../shared/feature-schema.rkt"
         "../shared/kernel.rkt")

(provide StrictS strict-s-red s-control-raw make-s-raw
         s-contract s-value? s-observation? s-frontier? s-initial s-query-initial s-run s-trace
         lift-owners/s context-support/s define-s-control)

;; Retain a removed Delay's introductions on the active root before its body
;; runs. Only force is transparent; common Owners are never copied to siblings
;; or moved through the Search/Frontier commitment boundary.
(define (lift-owners/s owners computation)
  (unless (redex-match? StrictSRel c computation)
    (raise-argument-error 'lift-owners/s "active S computation" computation))
  (match computation
    [`(force ,inner) `(force ,(lift-owners/s owners inner))]
    [`(,constructor ,local ,rest ...)
     `(,constructor ,(owners-append owners local) ,@rest)]))

(define (atomic-search/s owners goal state)
  (match (atomic/s goal state)
    [(Failure) `(Empty ,owners)]
    [(Success next) `(One ,owners ,next)]))

(define-syntax-rule (define-s-control name language feature)
 (define name
  (feature-reduction-relation
   feature language
   [--> (eval owners a σ)
        ,(atomic-search/s (term owners) (term a) (term σ)) eval-atom]
   [--> (eval owners (g_1 ∧ g_2 tag) σ)
        (bind owners (eval (Owners) g_1 σ) g_2) eval-conj]
   [--> (eval owners (g_1 ∨ g_2 tag) σ)
        (mplus owners (eval (Owners) g_1 σ) (eval (Owners) g_2 σ)) eval-disj]
   [--> (eval owners (suspend g tag) σ)
        (Delay owners (eval (Owners) g σ)) eval-suspend]
   [--> (mplus owners (Empty owners_1) SV)
        ,(lift-owners/s (term owners) (term SV)) mplus-empty]
   [--> (mplus owners (One owners_1 σ) SV)
        (Yield owners (Answer owners_1 σ) SV) mplus-one]
   [--> (mplus owners (Yield owners_1 (Answer owners_2 σ) SV_1) SV_2)
        (Yield owners
              (Answer ,(owners-append (term owners_1) (term owners_2)) σ)
              (mplus (Owners) ,(lift-owners/s (term owners_1) (term SV_1)) SV_2))
        mplus-yield]
   [--> (mplus owners (Delay owners_1 c) SV)
        (Delay owners (mplus (Owners) SV (force (Delay owners_1 c)))) mplus-delay]
   [--> (bind owners (Empty owners_1) g)
        (Empty ,(owners-append (term owners) (term owners_1))) bind-empty]
   [--> (bind owners (One owners_1 σ) g)
        (eval ,(owners-append (term owners) (term owners_1)) g σ) bind-one]
   [--> (bind owners (Yield owners_1 (Answer owners_2 σ) SV) g)
        (mplus ,(owners-append (term owners) (term owners_1))
               (eval owners_2 g σ)
               (bind (Owners) SV g))
        bind-yield]
   [--> (bind owners (Delay owners_1 c) g)
        (Delay ,(owners-append (term owners) (term owners_1))
               (bind (Owners) c g)) bind-delay]
   [--> (force (Delay owners c)) ,(lift-owners/s (term owners) (term c)) force-delay]

   [--> (render (Empty owners)) (Done owners) render-empty]
   [--> (commit (Empty owners)) (Done owners) commit-empty]

   [--> (render (One owners σ)) (Solo owners σ) render-one]
   [--> (commit (One owners σ)) (Solo owners σ) commit-one]

   [--> (render (Yield owners A SV)) (Emit owners A (render SV)) render-yield]
   [--> (commit (Yield owners A SV)) (Emit owners A (commit SV)) commit-yield]

   [--> (render (Delay owners c)) (Forced owners (render c)) render-delay]
   [--> (commit (Delay owners c)) (More (Delay owners c)) commit-delay]

   [--> (advance (Done owners)) (Done owners) advance-done]
   [--> (collect (Done owners)) (Done owners) collect-done]

   [--> (advance (Solo owners σ)) (Solo owners σ) advance-solo]
   [--> (collect (Solo owners σ)) (Solo owners σ) collect-solo]

   [--> (advance (Emit owners A F)) (Emit owners A (advance F)) advance-emit]
   [--> (collect (Emit owners A F)) (Emit owners A (collect F)) collect-emit]

   [--> (advance (Forced owners F)) (Forced owners (advance F)) advance-forced]
   [--> (collect (Forced owners F)) (Forced owners (collect F)) collect-forced]

   [--> (advance (More (Delay owners c))) (Forced owners (commit c)) advance-delay]
   [--> (collect (More (Delay owners c))) (Forced owners (collect (commit c))) collect-delay])))

(define-s-control s-control-raw StrictS search)

;; Allocation is context-sensitive only through the active world's Owner
;; prefix. No sibling, answer, or entire-frontier occurrence scan is involved.
(define s-allocation-red
  (reduction-relation
   StrictS #:domain q
   [--> (in-hole C (eval owners (∃ (x ...) g tag) σ))
        (in-hole C ,(allocate/s (term owners) (term (x ...)) (term g)
                               (term tag) (term σ) (context-support/s (term C))))
        allocate-fresh]))

(define strict-s-red
  (union-reduction-relations
   (context-closure s-control-raw StrictS C)
   s-allocation-red))

(define (make-s-raw [prefix '()])
  (extend-reduction-relation
   s-control-raw StrictS
   [--> (eval owners (∃ (x ...) g tag) σ)
        ,(allocate/s (term owners) (term (x ...)) (term g)
                     (term tag) (term σ) prefix)
        allocate-fresh]))

(define (s-contract computation [prefix '()])
  (match (apply-reduction-relation/tag-with-names (make-s-raw prefix) computation)
    ['() #f]
    [(list result) result]
    [results (error 's-contract "nonunique raw proof: ~e" results)]))

(define (s-value? value) (redex-match? StrictS SV value))
(define (s-observation? value) (redex-match? StrictS O value))
(define (s-frontier? value) (redex-match? StrictS F value))
(define (s-initial goal #:owners [owners '(Owners)]
                   #:state [state '(state () () () (label "initial"))])
  `(eval ,owners ,goal ,state))

(define (s-query-initial goal #:owners [owners '(Owners)]
                         #:state [state '(state () () () (label "initial"))])
  `(commit ,(s-initial goal #:owners owners #:state state)))

(define (s-trace computation [fuel 100000] [reversed '()])
  (unless (exact-nonnegative-integer? fuel)
    (raise-argument-error 's-trace "exact-nonnegative-integer?" fuel))
  (match (apply-reduction-relation/tag-with-names strict-s-red computation)
    ['()
     (unless (or (s-value? computation) (s-frontier? computation))
       (error 's-trace "stuck computation: ~e" computation))
     (reverse reversed)]
    [(list (and step (list _ next)))
     (when (zero? fuel) (error 's-trace "fuel exhausted"))
     (s-trace next (sub1 fuel) (cons step reversed))]
    [results (error 's-trace "nonunique source proof: ~e" results)]))

(define (s-run computation [fuel 100000])
  (unless (exact-nonnegative-integer? fuel)
    (raise-argument-error 's-run "exact-nonnegative-integer?" fuel))
  (cond
    [(or (s-value? computation) (s-frontier? computation)) computation]
    [else
     (when (zero? fuel) (error 's-run "fuel exhausted"))
     (match (apply-reduction-relation strict-s-red computation)
       [(list next) (s-run next (sub1 fuel))]
       [results (error 's-run "stuck or nonunique source proof: ~e" results)])]))
