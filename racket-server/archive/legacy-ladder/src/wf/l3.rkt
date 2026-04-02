#lang racket

(require rackunit
         redex/reduction-semantics
         "./l0.rkt"
         "../languages/l3-base.rkt")

(check-redundancy #t)

(provide wf-goal/L3?
         wf-tree/L3?
         wf-answer-stream/L3?
         wf-rel-env/L3?
         wf-config/L3?)

(define-metafunction L3
  same-length? : (any ...) (any ...) -> boolean
  [(same-length? () ()) #t]
  [(same-length? (any_1 any_rest_1 ...) (any_2 any_rest_2 ...))
   (same-length? (any_rest_1 ...) (any_rest_2 ...))]
  [(same-length? () (any_2 any_rest_2 ...)) #f]
  [(same-length? (any_1 any_rest_1 ...) ()) #f])

(define-metafunction L3
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ()) #f]
  [(relcall-arity-ok? r_call (t ...) ((r_call (x ...) g_env) (r_rest d_rest g_rest) ...))
   (same-length? (t ...) (x ...))]
  [(relcall-arity-ok? r_call (t ...) ((r_other d_other g_other) (r_rest d_rest g_rest) ...))
   (relcall-arity-ok? r_call (t ...) ((r_rest d_rest g_rest) ...))])

(define-judgment-form
  L3
  #:contract (wf-goal/L3? g ((r d_env g_env) ...) (x_1 ...) c)
  #:mode (wf-goal/L3? I I I I)

  [------------------ "trivial success wf/L3"
   (wf-goal/L3? (succeed tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [------------------ "trivial fail wf/L3"
   (wf-goal/L3? (fail tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/L3? g ((r d_env g_env) ...) (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/L3"
   (wf-goal/L3? (∃ (x_1 ...) g tag) ((r d_env g_env) ...) (x_2 ...) c)]

  [(wf-goal/L3? g_1 ((r d_env g_env) ...) (x_1 ...) c)
   (wf-goal/L3? g_2 ((r d_env g_env) ...) (x_1 ...) c)
   ------------------- "conj-wf/L3"
   (wf-goal/L3? (g_1 ∧ g_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-goal/L3? g_1 ((r d_env g_env) ...) (x_1 ...) c)
   (wf-goal/L3? g_2 ((r d_env g_env) ...) (x_1 ...) c)
   ------------------- "disj-wf/L3"
   (wf-goal/L3? (g_1 ∨ g_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-goal/L3? g ((r d_env g_env) ...) (x_1 ...) c)
   ------------------- "delay-goal-wf/L3"
   (wf-goal/L3? (suspend g tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/L3"
   (wf-goal/L3? (t_1 =? t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/L3"
   (wf-goal/L3? (t_1 != t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t (x_lex ...) c) ...
   (where #t (relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...)))
   ------------------- "relcall-wf/L3"
   (wf-goal/L3? (r_call t ... tag) ((r_1 d_1 g_1) ...) (x_lex ...) c)])

(define-judgment-form
  L3
  #:contract (wf-tree/L3? s ((r d g_env) ...) c)
  #:mode (wf-tree/L3? I I I)

  [------------------- "empty tree is wf/L3"
   (wf-tree/L3? (empty-tree) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/L3"
   (wf-tree/L3? (⊤ (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L3? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/L3"
   (wf-tree/L3? (g (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-tree/L3? s ((r d g_env) ...) c_i)
   (wf-goal/L3? g ((r d g_env) ...) () c_i)
   ------------------- "conj wf/L3"
   (wf-tree/L3? (s × g c_i) ((r d g_env) ...) c)]

  [(wf-tree/L3? s_1 ((r d g_env) ...) c)
   (wf-tree/L3? s_2 ((r d g_env) ...) c)
   ------------------- "left disj wf/L3"
   (wf-tree/L3? (s_1 <-+ s_2) ((r d g_env) ...) c)]

  [(wf-tree/L3? s ((r d g_env) ...) c)
   ------------------- "delay wf/L3"
   (wf-tree/L3? (delay s) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L3? (r_call t ... tag_call) ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "proceed relcall wf/L3"
   (wf-tree/L3?
    (proceed ((r_call t ... tag_call)
              (state sub dis c_i trail tag_state)))
    ((r d g_env) ...)
    c)]

  [(lvars-subset? c c_i)
   (wf-goal/L3? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "proceed expanded-goal wf/L3"
   (wf-tree/L3?
    (proceed (g (state sub dis c_i trail tag_state)))
    ((r d g_env) ...)
    c)])

(define-judgment-form
  L3
  #:contract (wf-answer-stream/L3? as c)
  #:mode (wf-answer-stream/L3? I I)

  [------------------- "empty answer stream wf/L3"
   (wf-answer-stream/L3? (empty-stream) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/L3"
   (wf-answer-stream/L3? (⊤ (state sub dis c_i trail tag)) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/L3? as_tail c)
   ------------------- "answer stream wf/L3"
   (wf-answer-stream/L3? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  L3
  #:contract (wf-rel-env/L3? Γ)
  #:mode (wf-rel-env/L3? I)
  [(wf-goal/L3? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/L3"
   (wf-rel-env/L3? ((r d g) ...))])

(define-judgment-form
  L3
  #:contract (wf-config/L3? config)
  #:mode (wf-config/L3? I)
  [(wf-rel-env/L3? ((r d g) ...))
   (wf-tree/L3? s ((r d g) ...) ())
   (wf-answer-stream/L3? as ())
   ----------------------- "program-wf/L3"
   (wf-config/L3? (((r d g) ...) s as))])

(module+ test
  (define cfg-l3
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           ((delay (proceed ((r:id (sym "ok") (label "call"))
                             (state () () () () (label "s")))))
            <-+
            ((succeed (label "b")) (state () () () () (label "sb"))))
           (empty-stream))))
  (check-true (judgment-holds (wf-config/L3? ,cfg-l3))))
