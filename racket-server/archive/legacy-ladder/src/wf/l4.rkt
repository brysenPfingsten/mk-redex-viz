#lang racket

(require redex/reduction-semantics
         rackunit
         "./l0.rkt"
         "../languages/all.rkt")

(check-redundancy #t)

(provide wf-goal/L4?
         wf-tree/L4?
         wf-answer-stream/L4?
         wf-rel-env/L4?
         wf-config/L4?)

(define-metafunction L4
  same-length? : (any ...) (any ...) -> boolean
  [(same-length? () ()) #t]
  [(same-length? (any_1 any_rest_1 ...) (any_2 any_rest_2 ...))
   (same-length? (any_rest_1 ...) (any_rest_2 ...))]
  [(same-length? () (any_2 any_rest_2 ...)) #f]
  [(same-length? (any_1 any_rest_1 ...) ()) #f])

(define-metafunction L4
  relcall-arity-ok? : r (t ...) ((r d g) ...) -> boolean
  [(relcall-arity-ok? r_call (t ...) ()) #f]
  [(relcall-arity-ok? r_call (t ...) ((r_call (x ...) g_env) (r_rest d_rest g_rest) ...))
   (same-length? (t ...) (x ...))]
  [(relcall-arity-ok? r_call (t ...) ((r_other d_other g_other) (r_rest d_rest g_rest) ...))
   (relcall-arity-ok? r_call (t ...) ((r_rest d_rest g_rest) ...))])

(define-judgment-form
  L4
  #:contract (wf-goal/L4? g ((r d_env g_env) ...) (x_1 ...) c)
  #:mode (wf-goal/L4? I I I I)

  [------------------ "trivial success wf/L4"
   (wf-goal/L4? (succeed tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [------------------ "trivial fail wf/L4"
   (wf-goal/L4? (fail tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal/L4? g ((r d_env g_env) ...) (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf/L4"
   (wf-goal/L4? (∃ (x_1 ...) g tag) ((r d_env g_env) ...) (x_2 ...) c)]

  [(wf-goal/L4? g_1 ((r d_env g_env) ...) (x_1 ...) c)
   (wf-goal/L4? g_2 ((r d_env g_env) ...) (x_1 ...) c)
   ---------- "conj-wf/L4"
   (wf-goal/L4? (g_1 ∧ g_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-goal/L4? g_1 ((r d_env g_env) ...) (x_1 ...) c)
   (wf-goal/L4? g_2 ((r d_env g_env) ...) (x_1 ...) c)
   ---------- "disj-wf/L4"
   (wf-goal/L4? (g_1 ∨ g_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-goal/L4? g ((r d_env g_env) ...) (x_1 ...) c)
   ---------- "delay-goal-wf/L4"
   (wf-goal/L4? (suspend g tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "==-wf/L4"
   (wf-goal/L4? (t_1 =? t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "=/=-wf/L4"
   (wf-goal/L4? (t_1 != t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t (x_lex ...) c) ...
   (where #t (relcall-arity-ok? r_call (t ...) ((r_1 d_1 g_1) ...)))
   ---------- "relcall-wf/L4"
   (wf-goal/L4? (r_call t ... tag) ((r_1 d_1 g_1) ...) (x_lex ...) c)])

(define-judgment-form
  L4
  #:contract (wf-tree/L4? s ((r d g_env) ...) c)
  #:mode (wf-tree/L4? I I I)

  [------------------- "empty tree is wf/L4"
   (wf-tree/L4? (empty-tree) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer/state wf/L4"
   (wf-tree/L4? (⊤ (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L4? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "goal/state wf/L4"
   (wf-tree/L4? (g (state sub dis c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-tree/L4? s ((r d g_env) ...) c_i)
   (wf-goal/L4? g ((r d g_env) ...) () c_i)
   ------------------- "conj wf/L4"
   (wf-tree/L4? (s × g c_i) ((r d g_env) ...) c)]

  [(wf-tree/L4? s_1 ((r d g_env) ...) c)
   (wf-tree/L4? s_2 ((r d g_env) ...) c)
   ------------------- "left disj wf/L4"
   (wf-tree/L4? (s_1 <-+ s_2) ((r d g_env) ...) c)]

  [(wf-tree/L4? s_1 ((r d g_env) ...) c)
   (wf-tree/L4? s_2 ((r d g_env) ...) c)
   ------------------- "right disj wf/L4"
   (wf-tree/L4? (s_1 +-> s_2) ((r d g_env) ...) c)]

  [(wf-tree/L4? s ((r d g_env) ...) c)
   ------------------- "delay wf/L4"
   (wf-tree/L4? (delay s) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L4? (r_call t ... tag_call) ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "proceed relcall wf/L4"
   (wf-tree/L4?
    (proceed ((r_call t ... tag_call)
              (state sub dis c_i trail tag_state)))
    ((r d g_env) ...)
    c)]

  [(lvars-subset? c c_i)
   (wf-goal/L4? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "proceed expanded-goal wf/L4"
   (wf-tree/L4?
    (proceed (g (state sub dis c_i trail tag_state)))
    ((r d g_env) ...)
    c)])

(define-judgment-form
  L4
  #:contract (wf-answer-stream/L4? as c)
  #:mode (wf-answer-stream/L4? I I)

  [------------------- "empty answer stream wf/L4"
   (wf-answer-stream/L4? (empty-stream) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   ------------------- "single answer stream wf/L4"
   (wf-answer-stream/L4? (⊤ (state sub dis c_i trail tag)) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-dis? dis c_i)
   (wf-answer-stream/L4? as_tail c)
   ------------------- "answer stream wf/L4"
   (wf-answer-stream/L4? ((⊤ (state sub dis c_i trail tag)) + as_tail) c)])

(define-judgment-form
  L4
  #:contract (wf-rel-env/L4? Γ)
  #:mode (wf-rel-env/L4? I)
  [(wf-goal/L4? g ((r d g) ...) d ()) ...
   ----------------------- "relation-env-wf/L4"
   (wf-rel-env/L4? ((r d g) ...))])

(define-judgment-form
  L4
  #:contract (wf-config/L4? config)
  #:mode (wf-config/L4? I)
  [(wf-rel-env/L4? ((r d g) ...))
   (wf-tree/L4? s ((r d g) ...) ())
   (wf-answer-stream/L4? as ())
   ----------------------- "program-wf/L4"
   (wf-config/L4? (((r d g) ...) s as))])

;; L1/L2/L3 are syntax subsets of L4; reuse the L4 wf judgment while
;; keeping language-specific contracts at each layer.
(module+ test
  (default-language L4)

  (define cfg-l1
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           (delay (proceed ((r:id (sym "ok") (label "call"))
                            (state () () () () (label "s")))))
          (empty-stream))))

  (define cfg-l2
    (term (()
           (((succeed (label "a")) (state () () () () (label "sa")))
            <-+
            ((succeed (label "b")) (state () () () () (label "sb"))))
           (empty-stream))))

  (define cfg-l3
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           ((delay (proceed ((r:id (sym "ok") (label "call"))
                             (state () () () () (label "s")))))
            <-+
            ((succeed (label "b")) (state () () () () (label "sb"))))
           (empty-stream))))

  (define cfg-l4
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           (((delay (proceed ((r:id (sym "ok") (label "call"))
                              (state () () () () (label "s")))))
             <-+
             ((succeed (label "b")) (state () () () () (label "sb"))))
            +-> (empty-tree))
           (empty-stream))))

  (define cfg-bad-arity
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           ((r:id (sym "ok") (sym "extra") (label "call"))
            (state () () () () (label "s")))
           (empty-stream))))

  (check-true  (judgment-holds (wf-config/L4? ,cfg-l4)))
  (check-false (judgment-holds (wf-config/L4? ,cfg-bad-arity))))
