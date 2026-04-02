#lang racket

(require rackunit
         redex/reduction-semantics
         "core-definitions.rkt"
         "wf-kernel.rkt")

(check-redundancy #t)

(provide (all-from-out "wf-kernel.rkt")
         wf-goal?
         wf-tree?
         wf-answer-stream?
         wf-rel-env?
         wf-config?
         core-goal-shape?
         core-tree-shape?
         core-answer-stream-shape?
         core-shape?)

(define-judgment-form
  Core
  #:contract (wf-goal? g ((r d g_env) ...) (x_1 ...) c)
  #:mode (wf-goal? I I I I)

  [------------------ "trivial success wf"
   (wf-goal? (succeed tag) ((r d g_env) ...) (x_1 ...) c)]

  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal? g ((r d g_env) ...) (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf"
   (wf-goal? (∃ (x_1 ...) g tag) ((r d g_env) ...) (x_2 ...) c)]

  [(wf-goal? g_1 ((r d g_env) ...) (x_1 ...) c)
   (wf-goal? g_2 ((r d g_env) ...) (x_1 ...) c)
   ---------- "conj-wf"
   (wf-goal? (g_1 ∧ g_2 tag) ((r d g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "==-wf"
   (wf-goal? (t_1 =? t_2 tag) ((r d g_env) ...) (x_1 ...) c)])

(define-judgment-form
  Core
  #:contract (core-goal-shape? g)
  #:mode (core-goal-shape? I)

  [------------------- "core-succeed-shape"
   (core-goal-shape? (succeed tag))]

  [------------------- "core-eq-shape"
   (core-goal-shape? (t_1 =? t_2 tag))]

  [(core-goal-shape? g_1)
   (core-goal-shape? g_2)
   ------------------- "core-conj-shape"
   (core-goal-shape? (g_1 ∧ g_2 tag))]

  [(core-goal-shape? g)
   ------------------- "core-exists-shape"
   (core-goal-shape? (∃ d g tag))])

(define-judgment-form
  Core
  #:contract (core-tree-shape? s)
  #:mode (core-tree-shape? I)

  [------------------- "core-empty-tree-shape"
   (core-tree-shape? (empty-tree))]

  [------------------- "core-answer-shape"
   (core-tree-shape? (⊤ σ))]

  [(core-goal-shape? g)
   ------------------- "core-goal-state-shape"
   (core-tree-shape? (g σ))]

  [(core-tree-shape? s)
   (core-goal-shape? g)
   ------------------- "core-conj-tree-shape"
   (core-tree-shape? (s × g c))]

  [(core-tree-shape? s_tail)
   ------------------- "core-emit-shape"
   (core-tree-shape? (emit σ s_tail))])

(define-judgment-form
  Core
  #:contract (core-answer-stream-shape? as)
  #:mode (core-answer-stream-shape? I)

  [------------------- "core-empty-answer-stream-shape"
   (core-answer-stream-shape? (empty-stream))]

  [------------------- "core-single-answer-stream-shape"
   (core-answer-stream-shape? (⊤ σ))]

  [(core-answer-stream-shape? as_tail)
   ------------------- "core-answer-stream-tail-shape"
   (core-answer-stream-shape? ((⊤ σ) + as_tail))])

(define-judgment-form
  Core
  #:contract (core-shape? config)
  #:mode (core-shape? I)
  [(core-goal-shape? g) ...
   (core-tree-shape? s)
   (core-answer-stream-shape? as)
   ------------------- "core-config-shape"
   (core-shape? (((r d g) ...) s as))])

(define-judgment-form
  Core
  #:contract (wf-tree? s ((r d g_env) ...) c)
  #:mode (wf-tree? I I I)

  [------------------- "empty tree is wf"
   (wf-tree? (empty-tree) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   ------------------- "single answer/state wf"
   (wf-tree? (⊤ (state sub c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   ------------------- "goal/state wf"
   (wf-tree? (g (state sub c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-tree? s ((r d g_env) ...) c_i)
   (wf-goal? g ((r d g_env) ...) () c_i)
   ------------------- "conj wf"
   (wf-tree? (s × g c_i) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-tree? s_tail ((r d g_env) ...) c)
   ------------------- "emit wf"
   (wf-tree? (emit (state sub c_i trail tag) s_tail) ((r d g_env) ...) c)])

(define-judgment-form
  Core
  #:contract (wf-answer-stream? as c)
  #:mode (wf-answer-stream? I I)

  [------------------- "empty answer stream wf"
   (wf-answer-stream? (empty-stream) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   ------------------- "single answer stream wf"
   (wf-answer-stream? (⊤ (state sub c_i trail tag)) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-answer-stream? as_tail c)
   ------------------- "answer stream wf"
   (wf-answer-stream? ((⊤ (state sub c_i trail tag)) + as_tail) c)])

(define-judgment-form
  Core
  #:contract (wf-rel-env? Γ)
  #:mode (wf-rel-env? I)
  [(wf-goal? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf"
   (wf-rel-env? ((r d g) ...))])

(define-judgment-form
  Core
  #:contract (wf-config? config)
  #:mode (wf-config? I)
  [(wf-rel-env? ((r d g) ...))
   (wf-tree? s ((r d g) ...) ())
   (wf-answer-stream? as ())
   ----------------------- "program-wf"
   (wf-config? (((r d g) ...) s as))])

(module+ test
  (check-true (judgment-holds (wf-goal? (succeed (label "fish")) () () ())))

  (check-true
   (judgment-holds
    (wf-goal? (u:0 =? (sym "a") (label "t"))
              ()
              ()
              (u:0))))

  (check-true
   (judgment-holds
    (wf-goal? ((u:0 =? (sym "a") (label "t1"))
               ∧
               (u:1 =? (sym "b") (label "t2"))
               (label "∧"))
              ()
              ()
              (u:0 u:1))))

  (check-true
   (judgment-holds
    (wf-goal? (u:0 =? (sym "a") (label "t"))
              ()
              (x:0 x:1)
              (u:2 u:1 u:0))))

  (check-true
   (judgment-holds
    (wf-goal? (∃ (x:0 x:1)
                 (u:0 =? (sym "a") (label "t"))
                 (label "fresh"))
              ()
              ()
              (u:0))))

  (check-true (judgment-holds (wf-tree? (empty-tree) () ())))

  (check-true
   (judgment-holds
    (wf-tree? ((u:0 =? (sym "a") (label "t"))
               (state ((u:0 (sym "a")))
                      (u:0)
                      ((u:0 =? (sym "a") (label "t1")))
                      (label "σ")))
              ()
              (u:0))))

  (check-true
   (judgment-holds
    (wf-tree? (((u:0 =? (sym "a") (label "t"))
                (state ((u:0 (sym "a")))
                       (u:0)
                       ((u:0 =? (sym "a") (label "t1")))
                       (label "σ")))
               ×
               (succeed (label "fish"))
               ())
              ()
              ())))

  (check-true (judgment-holds (wf-config? (() (empty-tree) (empty-stream)))))

  (check-true
   (judgment-holds
    (wf-config? (() (empty-tree)
                 (⊤ (state ((u:0 (sym "a")))
                           (u:0)
                           (((sym "a") =? u:0 (label "g1")))
                           (label "σ")))))))

  (check-true
   (judgment-holds
    (wf-rel-env?
     ((r:ok (x:0) (x:0 =? x:0 (label "eq")))))))

  (check-false
   (judgment-holds
    (wf-rel-env?
     ((r:bad () (x:0 =? x:0 (label "eq")))))))

  (check-true (judgment-holds (core-shape? (() (empty-tree) (empty-stream)))))

  (check-true
   (judgment-holds
    (core-shape?
     (()
      (((succeed (label "ok")) ∧ (succeed (label "ok2")) (label "c"))
       (state () () () (label "s")))
      (empty-stream)))))

  (check-true
   (judgment-holds
    (core-shape?
     (() (empty-tree)
         ((⊤ (state () () () (label "a")))
          +
          ((⊤ (state () () () (label "b")))
           +
           (empty-stream)))))))

  (define (core-tree-shape-holds? st)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (judgment-holds (core-tree-shape? ,st))))

  (define (core-config-shape-holds? cfg)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (judgment-holds (core-shape? ,cfg))))

  (check-false (core-tree-shape-holds? '(delay (empty-tree))))

  (check-false
   (core-config-shape-holds?
    '(() (proceed ((r:foo (sym "x") (label "t"))
                   (state () () () (label "s"))))
         (empty-stream)))))
