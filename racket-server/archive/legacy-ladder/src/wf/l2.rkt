#lang racket

(require rackunit
         redex/reduction-semantics
         "./l0.rkt"
         "../languages/l2-disjunction-left.rkt")

(check-redundancy #t)

(provide wf-goal/L2?
         wf-tree/L2?
         wf-answer-stream/L2?
         wf-rel-env/L2?
         wf-config/L2?)

(define-judgment-form
  L2
  #:contract (wf-goal/L2? g ((r d_env g_env) ...) (x_1 ...) c)
  #:mode (wf-goal/L2? I I I I)

  [------------------ "trivial success wf/L2"
   (wf-goal/L2? (succeed tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [------------------ "trivial fail wf/L2"
   (wf-goal/L2? (fail tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/L2? g ((r d_env g_env) ...) (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/L2"
   (wf-goal/L2? (∃ (x_1 ...) g tag) ((r d_env g_env) ...) (x_2 ...) c)]

  [(wf-goal/L2? g_1 ((r d_env g_env) ...) (x_1 ...) c)
   (wf-goal/L2? g_2 ((r d_env g_env) ...) (x_1 ...) c)
   ------------------- "conj-wf/L2"
   (wf-goal/L2? (g_1 ∧ g_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-goal/L2? g_1 ((r d_env g_env) ...) (x_1 ...) c)
   (wf-goal/L2? g_2 ((r d_env g_env) ...) (x_1 ...) c)
   ------------------- "disj-wf/L2"
   (wf-goal/L2? (g_1 ∨ g_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/L2"
   (wf-goal/L2? (t_1 =? t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/L2"
   (wf-goal/L2? (t_1 != t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)])

(define-judgment-form
  L2
  #:contract (wf-tree/L2? s ((r d g_env) ...) c)
  #:mode (wf-tree/L2? I I I)

  [------------------- "empty tree is wf/L2"
   (wf-tree/L2? (empty-tree) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/L2"
   (wf-tree/L2? (⊤ (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L2? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/L2"
   (wf-tree/L2? (g (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-tree/L2? s ((r d g_env) ...) c_i)
   (wf-goal/L2? g ((r d g_env) ...) () c_i)
   ------------------- "conj wf/L2"
   (wf-tree/L2? (s × g c_i) ((r d g_env) ...) c)]

  [(wf-tree/L2? s_1 ((r d g_env) ...) c)
   (wf-tree/L2? s_2 ((r d g_env) ...) c)
   ------------------- "left disj wf/L2"
   (wf-tree/L2? (s_1 <-+ s_2) ((r d g_env) ...) c)])

(define-judgment-form
  L2
  #:contract (wf-answer-stream/L2? as c)
  #:mode (wf-answer-stream/L2? I I)

  [------------------- "empty answer stream wf/L2"
   (wf-answer-stream/L2? (empty-stream) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/L2"
   (wf-answer-stream/L2? (⊤ (state sub dis c_i trail tag)) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/L2? as_tail c)
   ------------------- "answer stream wf/L2"
   (wf-answer-stream/L2? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  L2
  #:contract (wf-rel-env/L2? Γ)
  #:mode (wf-rel-env/L2? I)
  [(wf-goal/L2? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/L2"
   (wf-rel-env/L2? ((r d g) ...))])

(define-judgment-form
  L2
  #:contract (wf-config/L2? config)
  #:mode (wf-config/L2? I)
  [(wf-rel-env/L2? ((r d g) ...))
   (wf-tree/L2? s ((r d g) ...) ())
   (wf-answer-stream/L2? as ())
   ----------------------- "program-wf/L2"
   (wf-config/L2? (((r d g) ...) s as))])

(module+ test
  (define cfg-l2
    (term (()
           (((succeed (label "a")) (state () () () () (label "sa")))
            <-+
            ((succeed (label "b")) (state () () () () (label "sb"))))
           (empty-stream))))
  (check-true (judgment-holds (wf-config/L2? ,cfg-l2))))
