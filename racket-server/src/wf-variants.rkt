#lang racket

(require redex/reduction-semantics
         rackunit
         "wf-core.rkt"
         "extensions/variant-languages.rkt")

(check-redundancy #t)

(provide wf-goal/L4?
         wf-tree/L4?
         wf-answer-stream/L4?
         wf-rel-env/L4?
         wf-config/L4?
         wf-config/L1?
         wf-config/L2?
         wf-config/L3?
         wf-config/target?
         config-in-target-domain?)

(define (fresh-lvars/rkt xs c)
  (let-values ([(rev-fresh _used)
                (for/fold ([rev-fresh '()]
                           [used c])
                          ([_x (in-list xs)])
                  (define u (variable-not-in (cons 'u: used) 'u:))
                  (values (cons u rev-fresh)
                          (cons u used)))])
    rev-fresh))

(define (lookup-rel-arity gamma rel-name)
  (for/first ([defn (in-list gamma)]
              #:when (equal? (first defn) rel-name))
    (length (second defn))))

(define-judgment-form
  L4
  #:contract (wf-goal/L4? g ((r d_env g_env) ...) (x_1 ...) c)
  #:mode (wf-goal/L4? I I I I)

  [------------------ "trivial success wf/L4"
   (wf-goal/L4? (succeed tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(where (u_old ...) c)
   (where (u_new ...) ,(fresh-lvars/rkt (term (x_1 ...)) (term c)))
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

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "==-wf/L4"
   (wf-goal/L4? (t_1 =? t_2 tag) ((r d_env g_env) ...) (x_1 ...) c)]

  [(wf-term? t (x_lex ...) c) ...
   (side-condition
    ,(let* ([gamma (term ((r_1 d_1 g_1) ...))]
            [rel-name (term r_call)]
            [arity (lookup-rel-arity gamma rel-name)])
       (and arity
            (= arity (length (term (t ...)))))))
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
   ------------------- "single answer/state wf/L4"
   (wf-tree/L4? (⊤ (state sub c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal/L4? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   ------------------- "goal/state wf/L4"
   (wf-tree/L4? (g (state sub c_i trail tag)) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-tree/L4? s ((r d g_env) ...) c_i)
   (wf-goal/L4? g ((r d g_env) ...) () c_i)
   ------------------- "conj wf/L4"
   (wf-tree/L4? (s × g c_i) ((r d g_env) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-tree/L4? s_tail ((r d g_env) ...) c)
   ------------------- "emit wf/L4"
   (wf-tree/L4? (emit (state sub c_i trail tag) s_tail) ((r d g_env) ...) c)]

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
   ------------------- "proceed relcall wf/L4"
   (wf-tree/L4?
    (proceed ((r_call t ... tag_call)
              (state sub c_i trail tag_state)))
    ((r d g_env) ...)
    c)]

  [(lvars-subset? c c_i)
   (wf-goal/L4? g ((r d g_env) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   ------------------- "proceed expanded-goal wf/L4"
   (wf-tree/L4?
    (proceed (g (state sub c_i trail tag_state)))
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
   ------------------- "single answer stream wf/L4"
   (wf-answer-stream/L4? (⊤ (state sub c_i trail tag)) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-answer-stream/L4? as_tail c)
   ------------------- "answer stream wf/L4"
   (wf-answer-stream/L4? ((⊤ (state sub c_i trail tag)) + as_tail) c)])

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
(define-judgment-form
  L1
  #:contract (wf-config/L1? config)
  #:mode (wf-config/L1? I)
  [(where #t ,(judgment-holds (wf-config/L4? ,(term config))))
   ----------------------- "L1 via L4 wf"
   (wf-config/L1? config)])

(define-judgment-form
  L2
  #:contract (wf-config/L2? config)
  #:mode (wf-config/L2? I)
  [(where #t ,(judgment-holds (wf-config/L4? ,(term config))))
   ----------------------- "L2 via L4 wf"
   (wf-config/L2? config)])

(define-judgment-form
  L3
  #:contract (wf-config/L3? config)
  #:mode (wf-config/L3? I)
  [(where #t ,(judgment-holds (wf-config/L4? ,(term config))))
   ----------------------- "L3 via L4 wf"
   (wf-config/L3? config)])

(define (config-in-target-domain? target-id cfg)
  (case (string->symbol target-id)
    [(L1/config) (redex-match? L1 config cfg)]
    [(L2/config) (redex-match? L2 config cfg)]
    [(L3/config) (redex-match? L3 config cfg)]
    [(L4/config) (redex-match? L4 config cfg)]
    [else #f]))

(define (wf-config/target? target-id cfg)
  (case (string->symbol target-id)
    [(L1/config) (and (redex-match? L1 config cfg)
                      (judgment-holds (wf-config/L1? ,cfg)))]
    [(L2/config) (and (redex-match? L2 config cfg)
                      (judgment-holds (wf-config/L2? ,cfg)))]
    [(L3/config) (and (redex-match? L3 config cfg)
                      (judgment-holds (wf-config/L3? ,cfg)))]
    [(L4/config) (and (redex-match? L4 config cfg)
                      (judgment-holds (wf-config/L4? ,cfg)))]
    [else #f]))

(module+ test
  (default-language L4)

  (define cfg-l1
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           (delay (proceed ((r:id (sym "ok") (label "call"))
                            (state () () () (label "s")))))
          (empty-stream))))

  (define cfg-l2
    (term (()
           (((succeed (label "a")) (state () () () (label "sa")))
            <-+
            ((succeed (label "b")) (state () () () (label "sb"))))
           (empty-stream))))

  (define cfg-l3
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           ((delay (proceed ((r:id (sym "ok") (label "call"))
                             (state () () () (label "s")))))
            <-+
            ((succeed (label "b")) (state () () () (label "sb"))))
           (empty-stream))))

  (define cfg-l4
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           (((delay (proceed ((r:id (sym "ok") (label "call"))
                              (state () () () (label "s")))))
             <-+
             ((succeed (label "b")) (state () () () (label "sb"))))
            +-> (empty-tree))
           (empty-stream))))

  (define cfg-bad-arity
    (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
           ((r:id (sym "ok") (sym "extra") (label "call"))
            (state () () () (label "s")))
           (empty-stream))))

  (check-true  (judgment-holds (wf-config/L1? ,cfg-l1)))
  (check-true  (judgment-holds (wf-config/L2? ,cfg-l2)))
  (check-true  (judgment-holds (wf-config/L3? ,cfg-l3)))
  (check-true  (judgment-holds (wf-config/L4? ,cfg-l4)))
  (check-false (judgment-holds (wf-config/L4? ,cfg-bad-arity)))
  (check-true  (wf-config/target? "L4/config" cfg-l4))
  (check-false (wf-config/target? "L4/config" cfg-bad-arity)))
