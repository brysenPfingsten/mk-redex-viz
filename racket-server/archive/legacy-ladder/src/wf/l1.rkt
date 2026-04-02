#lang racket

(require rackunit
         redex/reduction-semantics
         "./l0.rkt"
         "../languages/l1-calls-delay.rkt")

(check-redundancy #t)

(provide wf-goal/L1?
         wf-tree/L1?
         wf-answer-stream/L1?
         wf-rel-env/L1?
         wf-config/L1?)

(define-metafunction L1
  same-length? : (any ...) (any ...) -> boolean
  [(same-length? () ()) #t]
  [(same-length? (any_1 any_rest_1 ...) (any_2 any_rest_2 ...))
   (same-length? (any_rest_1 ...) (any_rest_2 ...))]
  [(same-length? () (any_2 any_rest_2 ...)) #f]
  [(same-length? (any_1 any_rest_1 ...) ()) #f])

(define-metafunction L1
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ()) #f]
  [(relcall-arity-ok? r_call (t ...) ((r_call (x ...) g_env) (r_rest d_rest g_rest) ...))
   (same-length? (t ...) (x ...))]
  [(relcall-arity-ok? r_call (t ...) ((r_other d_other g_other) (r_rest d_rest g_rest) ...))
   (relcall-arity-ok? r_call (t ...) ((r_rest d_rest g_rest) ...))])

(define-judgment-form
  L1
  #:contract (wf-goal/L1? g ((r d_env g_env) ...) (x_1 ...) c)
  #:mode (wf-goal/L1? I I I I)

  [------------------ "trivial success wf/L1"
   (wf-goal/L1? (succeed tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [------------------ "trivial fail wf/L1"
   (wf-goal/L1? (fail tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/L1? g ((r d_env g_env) ...) (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/L1"
   (wf-goal/L1? (∃ (x_1 ...) g tag) ((r d_env g_env) ...) (x_2 ...) c)]

  [(wf-goal/L1? g_1 ((r d_env g_env) ...) (x_1 ...) c)
   (wf-goal/L1? g_2 ((r d_env g_env) ...) (x_1 ...) c)
   ------------------- "conj-wf/L1"
   (wf-goal/L1? (g_1 ∧ g_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-goal/L1? g ((r d_env g_env) ...) (x_1 ...) c)
   ------------------- "delay-goal-wf/L1"
   (wf-goal/L1? (suspend g tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "==-wf/L1"
   (wf-goal/L1? (t_1 =? t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ------------------- "=/=-wf/L1"
   (wf-goal/L1? (t_1 != t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t (x_lex ...) c) ...
   (where #t (relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...)))
   ------------------- "relcall-wf/L1"
   (wf-goal/L1? (r_call t ... tag) ((r_1 d_1 g_1) ...) (x_lex ...) c)])

(define-judgment-form
  L1
  #:contract (wf-tree/L1? s ((r d g_env) ...) c)
  #:mode (wf-tree/L1? I I I)

  [------------------- "empty tree is wf/L1"
   (wf-tree/L1? (empty-tree) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/L1"
   (wf-tree/L1? (⊤ (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L1? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/L1"
   (wf-tree/L1? (g (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-tree/L1? s ((r d g_env) ...) c_i)
   (wf-goal/L1? g ((r d g_env) ...) () c_i)
   ------------------- "conj wf/L1"
   (wf-tree/L1? (s × g c_i) ((r d g_env) ...) c)]

  [(wf-tree/L1? s ((r d g_env) ...) c)
   ------------------- "delay wf/L1"
   (wf-tree/L1? (delay s) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L1? (r_call t ... tag_call) ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "proceed relcall wf/L1"
   (wf-tree/L1?
    (proceed ((r_call t ... tag_call)
              (state sub dis c_i trail tag_state)))
    ((r d g_env) ...)
    c)]

  [(lvars-subset? c c_i)
   (wf-goal/L1? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "proceed expanded-goal wf/L1"
   (wf-tree/L1?
    (proceed (g (state sub dis c_i trail tag_state)))
    ((r d g_env) ...)
    c)])

(define-judgment-form
  L1
  #:contract (wf-answer-stream/L1? as c)
  #:mode (wf-answer-stream/L1? I I)

  [------------------- "empty answer stream wf/L1"
   (wf-answer-stream/L1? (empty-stream) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/L1"
   (wf-answer-stream/L1? (⊤ (state sub dis c_i trail tag)) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/L1? as_tail c)
   ------------------- "answer stream wf/L1"
   (wf-answer-stream/L1? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  L1
  #:contract (wf-rel-env/L1? Γ)
  #:mode (wf-rel-env/L1? I)
  [(wf-goal/L1? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/L1"
   (wf-rel-env/L1? ((r d g) ...))])

(define-judgment-form
  L1
  #:contract (wf-config/L1? config)
  #:mode (wf-config/L1? I)
  [(wf-rel-env/L1? ((r d g) ...))
   (wf-tree/L1? s ((r d g) ...) ())
   (wf-answer-stream/L1? as ())
   ----------------------- "program-wf/L1"
   (wf-config/L1? (((r d g) ...) s as))])

(module+ test
  (define cfg-l1
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           (delay (proceed ((r:id (sym "ok") (label "call"))
                            (state () () () () (label "s")))))
           (empty-stream))))
  (check-true (judgment-holds (wf-config/L1? ,cfg-l1))))
