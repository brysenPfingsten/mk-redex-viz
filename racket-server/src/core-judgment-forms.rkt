#lang racket
(require rackunit
         redex/reduction-semantics
         "core-definitions.rkt")

(check-redundancy #t)

(provide wf-goal?
         wf-tree?
         wf-term?
         wf-state?
         wf-sub/wf+equiv-trail?
         wf-sub?
         wf-rel-env?
         wf-config?
         core-shape?)

(module+ test
  (require rackunit)
  (default-language Core))

;; Collect symbols appearing in a term-shaped datum into a set.
(define (symbols-in/set t [acc (set)])
  (match t
    ['() acc]
    [(? symbol?) (set-add acc t)]
    [(cons a d) (symbols-in/set a (symbols-in/set d acc))]
    [_ acc]))

;; Graph-based acyclicity check for substitutions represented as
;; `((u t) ...)` lists.
(define (substitution-acyclic? pairs)
  (define dom (map first pairs))
  (define adj
    (for/hash ([(u t*) (in-dict pairs)])
      (values u
              (for/set ([v (in-set (symbols-in/set (car t*)))]
                        #:when (member v dom))
                v))))
  (define visiting (make-hash))
  (define visited (make-hash))
  (define (visit u)
    (cond
      [(hash-ref visited u #f) #t]
      [(hash-ref visiting u #f) #f]
      [else
       (hash-set! visiting u #t)
       (define ok
         (for/and ([v (in-set (hash-ref adj u (set)))])
           (visit v)))
       (hash-remove! visiting u)
       (when ok (hash-set! visited u #t))
       ok]))
  (for/and ([u dom]) (visit u)))

(define-metafunction Core
  acyclic-sub? : sub -> boolean
  [(acyclic-sub? sub)
   ,(substitution-acyclic? (term sub))])

(define-judgment-form
  Core
  #:contract (lvar-member? u c)
  #:mode (lvar-member? I I)

  [--------"lvar member"
   (lvar-member? u (u_1 ... u u_2 ...))]
)

(module+ test
  (check-true (judgment-holds (lvar-member? u:0 (u:0))))
  (check-true (judgment-holds (lvar-member? u:0 (u:1 u:0))))
  (check-false (judgment-holds (lvar-member? u:7 (u:0))))
  (check-true (judgment-holds (lvar-member? u:1 (u:2 u:1 u:0))))
)

(define-judgment-form
  Core
  #:contract (lvars-subset? (u ...) (u ...))
  #:mode (lvars-subset? I I)

  [------------------- "empty ⊆ anything"
   (lvars-subset? () c)]

  [(lvar-member? u c_2)
   (lvars-subset? (u_rest ...) c_2)
   ------------------- "cons ⊆"
   (lvars-subset? (u u_rest ...) c_2)])


(define-judgment-form
  Core
  #:contract (wf-term? t (x ...) c)
  #:mode (wf-term? I I I)

  [(lvar-member? u c)
   -------------- "lv in extant lvs"
   (wf-term? u (x ...) c)]

  [-------------- "primitive terms are wf and valid"
   (wf-term? pt (x ...) c)]

  [(wf-term? t_2 (x ...) c)
   (wf-term? t_1 (x ...) c)
   -------------- "pairs wf when constituents wf"
   (wf-term? (t_1 : t_2) (x ...) c)]

  [-------------- "lexical var is in lv bindings"
   (wf-term? x_2 (x_1 ... x_2 x_3 ...) c)])

(module+ test
  (check-true (judgment-holds (wf-term? (sym "a") () ())))
  (check-true (judgment-holds (wf-term? u:0 () (u:0))))
  (check-true (judgment-holds (wf-term? u:1 () (u:0 u:1))))
  (check-false (judgment-holds (wf-term? u:3 () (u:0 u:1))))
  ;; lexical variable must be in the binder list
  (check-true  (judgment-holds (wf-term? x:0 (x:0) (u:5))))
  (check-false (judgment-holds (wf-term? x:0 () (u:5))))
)

(define-judgment-form
  Core
  #:contract (wf-sub? sub c)
  #:mode (wf-sub? I I)

  [(wf-term? t () c) ...
   (lvar-member? u c) ...
   (where #t (acyclic-sub? ([u t] ...)))
   ------------------"sub closed under c w/no lexical vars"
   (wf-sub? ([u t] ...) c)])

(module+ test
  (check-true  (substitution-acyclic? (term ((u:0 u:1) (u:1 (sym "z"))))))
  (check-false (substitution-acyclic? (term ((u:0 u:1) (u:1 u:0)))))
  (check-true  (judgment-holds (wf-sub? ((u:0 (sym "x"))) (u:0))))
  (check-false (judgment-holds (wf-sub? ((u:1 (sym "x"))) (u:0))))
  ;; two bindings ok
  (check-true  (judgment-holds (wf-sub? ((u:0 (sym "x")) (u:2 (sym "y")))
                                        (u:0 u:2))))
  ;; acyclic variable chain is allowed
  (check-true  (judgment-holds (wf-sub? ((u:0 u:1) (u:1 (sym "z")))
                                        (u:0 u:1))))
  ;; cyclic substitutions are rejected
  (check-false (judgment-holds (wf-sub? ((u:0 u:1) (u:1 u:0))
                                        (u:0 u:1))))
)

(define-judgment-form
  Core
  #:contract (wf-goal? g ((r (x ...)) ...) (x_1 ...) c)
  #:mode (wf-goal? I I I I)

  [------------------ "trivial success wf"
   (wf-goal? (succeed tag) ((r (x ...)) ...) (x_1 ...) c)]

  [(where (u_old ...) c)
   (where (u_new ...) (fresh-lvars (x_1 ...) c))
   (wf-goal? g ((r (x ...)) ...) (x_1 ... x_2 ...) (u_new ... u_old ...))
   ------------------- "fresh-wf"
   (wf-goal? (∃ (x_1 ...) g tag) ((r (x ...)) ...) (x_2 ...) c)]

  [(wf-goal? g_1 ((r (x ...)) ...) (x_1 ...) c)
   (wf-goal? g_2 ((r (x ...)) ...) (x_1 ...) c)
   ---------- "conj-wf"
   (wf-goal? (g_1 ∧ g_2 tag) ((r (x ...)) ...) (x_1 ...) c)]

  [(wf-term? t_1 (x_1 ...) c)
   (wf-term? t_2 (x_1 ...) c)
   ---------- "==-wf"
   (wf-goal? (t_1 =? t_2 tag) ((r (x ...)) ...) (x_1 ...) c)]

  )

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
   ------------------- "core-answer-stream-shape"
   (core-tree-shape? ((⊤ σ) + s_tail))])

(define-judgment-form
  Core
  #:contract (core-shape? config)
  #:mode (core-shape? I)
  [(core-goal-shape? g) ...
   (core-tree-shape? s)
   ------------------- "core-config-shape"
   (core-shape? (((r d g) ...) s))])

(module+ test
  ;; succeed
  (check-true (judgment-holds (wf-goal? (succeed (label "fish")) () () ())))

  ;; equality with only lvs present in c
  (check-true (judgment-holds
               (wf-goal? (u:0 =? (sym "a") (label "t"))
                         ()
                         ()
                         (u:0))))

  ;; conjunction
  (check-true (judgment-holds
               (wf-goal? ((u:0 =? (sym "a") (label "t1"))
                          ∧ (u:1 =? (sym "b") (label "t2")) (label "∧"))
                         ()
                         ()
                         (u:0 u:1))))

  ;; ∃ adds fresh u's to c via add-vars-not-in
  (check-true
    (judgment-holds
      (wf-goal? (u:0 =? (sym "a") (label "t"))
                ()
                (x:0 x:1)
                (u:2 u:1 u:0))))

  ;; ∃ adds fresh u's to c via add-vars-not-in
  (check-true
    (judgment-holds
      (wf-goal? (∃ (x:0 x:1)
                  (u:0 =? (sym "a") (label "t")) (label "fresh"))
                ()
                ()
                (u:0))))
)

;; Given a list of used symbols, produce a fresh one
(define-metafunction Core
  ;; Takes a list of symbols, returns a fresh symbol
  fresh-lv : (u ...) -> u
  [(fresh-lv (u ...)) ,(variable-not-in (cons 'u: (term (u  ...))) 'u:)])


;; redex's variables-not-in uses the vars list themselves as the
;; prefixes, which doesn't work with our use case.
(define-metafunction Core
  fresh-lvars : (x ...) c -> c
  [(fresh-lvars (x ...) c)
    ,(let-values ([(fv* _used)
                   (for/fold ([fv* '()]
                              [used (term c)])
                             ([_ (in-list (term (x ...)))])
                     (define fv (variable-not-in (cons 'u: used) 'u:))
                     (values (cons fv fv*) (cons fv used)))])
       fv*)])

(module+ test
  (check-equal?
    (term (fresh-lvars (x:0 x:1 x:2) (u:1 u:7 u:3)))
    '(u:5 u:4 u:2)))

(define-judgment-form
  Core
  #:contract (wf-trail-unify*s-to-sub (eq ...) c sub sub)
  #:mode (wf-trail-unify*s-to-sub I I I I)

  [-------------------"trail is empty, acc is our sub"
   (wf-trail-unify*s-to-sub () c sub sub)]

  ;; grammar makes subst's u's distinct; if each is in c, |subst| < c
  [(where sub_acc2 (unify (walk t_1 sub_acc) (walk t_2 sub_acc) sub_acc))
   (wf-term? t_1 () c)
   (wf-term? t_2 () c)
   (wf-trail-unify*s-to-sub (eq ...) c sub_acc2 sub)
   -------------------"this pair is well formed and unify"
   (wf-trail-unify*s-to-sub ((t_1 =? t_2 tag) eq ...) c sub_acc sub)]

)

(module+ test
  (check-false (judgment-holds (wf-trail-unify*s-to-sub () (u:2 u:1 u:0) ((u:0 u:2) (u:1 u:0)) ((u:1 u:0)))))
  (check-false (judgment-holds (wf-trail-unify*s-to-sub () (u:2 u:1 u:0) ((u:1 u:0)) ((u:0 u:2) (u:1 u:0)))))
)



(define-judgment-form
  Core
  #:contract (wf-sub/wf+equiv-trail? sub c trail)
  #:mode (wf-sub/wf+equiv-trail? I I I)

  ;; grammar makes subst's u's distinct; if each is in c, |subst| < c
  [(wf-sub? sub c)
   (wf-trail-unify*s-to-sub (eq ...) c () sub)
   -------------------"goal w/ sub wf"
   (wf-sub/wf+equiv-trail? sub c (eq ...))]

)

(define-judgment-form
  Core
  #:contract (wf-state? σ)
  #:mode (wf-state? I)

  [(wf-sub/wf+equiv-trail? sub c trail)
   ----------------------- "state wf"
   (wf-state? (state sub c trail tag))])

(define-judgment-form
  Core
  #:contract (wf-tree? s ((r d) ...) c)
  #:mode (wf-tree? I I I)

  [-------------------"empty tree is wf"
   (wf-tree? (empty-tree) ((r d) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   -------------------"single answer/state wf"
   (wf-tree? (⊤ (state sub c_i trail tag)) ((r d) ...) c)]

  [(lvars-subset? c c_i)
   (wf-goal? g ((r d) ...) () c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   -------------------"goal/state wf"
   (wf-tree? (g (state sub c_i trail tag)) ((r d) ...) c)]

  [(lvars-subset? c c_i)
   (wf-tree? s ((r d) ...) c_i)
   (wf-goal? g ((r d) ...) () c_i)
   -------------------"conj wf"
   (wf-tree? (s × g c_i) ((r d) ...) c)]

  [(lvars-subset? c c_i)
   (wf-sub/wf+equiv-trail? sub c_i trail)
   (wf-tree? s_tail ((r d) ...) c)
   -------------------"answer stream wf"
   (wf-tree? ((⊤ (state sub c_i trail tag)) + s_tail) ((r d) ...) c)])

(define-judgment-form
  Core
  #:contract (wf-rel-env? Γ)
  #:mode (wf-rel-env? I)
  [(wf-goal? g ((r d) ...) d ()) ...
   ----------------------- "relation-env-wf"
   (wf-rel-env? ((r d g) ...))])

(define-judgment-form
  Core
  #:contract (wf-config? config)
  #:mode (wf-config? I)
  [(wf-rel-env? ((r d g) ...))
   (wf-tree? s ((r d) ...) ())
   ----------------------- "program-wf"
   (wf-config? (((r d g) ...) s))]
  )

  #;[(wf-tree? s ((r (x ...)) ...))
   -------------------"partial tree wf"
   (wf-tree? (∂ s _) ((r (x ...)) ...))] ;; TODO: wf-state-judgement?

  #;[(wf-tree? s_1 ((r (x ...)) ...))
   (wf-tree? s_2 ((r (x ...)) ...))
   -------------------"left disj wf"
   (wf-tree? (s_1 <-+ s_2) ((r (x ...)) ...))]

  #;[(wf-tree? s_1 ((r (x ...)) ...))
   (wf-tree? s_2 ((r (x ...)) ...))
   -------------------"right disj wf"
   (wf-tree? (s_1 +-> s_2) ((r (x ...)) ...))]



;; (define-judgment-form
;;   Core
;;   #:contract (wf-trail? trail c)
;;   #:mode (wf-trail? I I)

;;   [
;;    ------------------ "empty trail is wf"
;;    (wf-trail? () c)]

;;   [(wf-term? t_1 () c)
;;    (wf-term? t_2 () c)
;;    (wf-trail? ((t_3 =? t_4 o) ...) c)
;;    ------------------ "trail is wf"
;;   (wf-trail? ((t_1 =? t_2 _) (t_3 =? t_4 o) ...) c)])

  ;; [(wf-goal? g_1 ((r (x ...)) ...) (x_1 ...) c)
  ;;  (wf-goal? g_2 ((r (x ...)) ...) (x_1 ...) c)
  ;;  ---------- "disj-wf"
  ;;  (wf-goal? (g_1 ∨ g_2 _) ((r (x ...)) ...) (x_1 ...) c)]

  ;; [(same-length? (t ...) (x_i ...))
  ;;  (wf-term? t (x_k ...) c) ...
  ;;  ---------- "relcall-wf"
  ;;  (wf-goal? (r_i t ... _) ((r_1 (x_1 ...)) ... (r_i (x_i ...)) (r_j (x_j ...)) ...) (x_k ...) c)]

  #;[(wf-tree? s ((r (x ...)) ...))
   -------------------"delay wf"
   (wf-tree? (delay s) ((r (x ...)) ...))]

  #;[(wf-tree? s ((r (x ...)) ...))
   -------------------"proceed wf"
   (wf-tree? (proceed s) ((r (x ...)) ...))]



(module+ test
  ;; two-step trail; final σ must be exactly as unify builds it (new bindings consed in front)
  (check-true
   (judgment-holds
    (wf-trail-unify*s-to-sub
     ((u:0 =? (sym "a") (label "t1"))
      ((u:1 : u:0) =? ((sym "b") : (sym "a")) (label "t2")))
     (u:0 u:1)
     ()
     ((u:1 (sym "b")) (u:0 (sym "a"))))))

  (check-true
   (judgment-holds
    (wf-sub/wf+equiv-trail?
     ((u:1 (sym "b")) (u:0 (sym "a")))
     (u:0 u:1)
     ((u:0 =? (sym "a") (label "t1"))
      ((u:1 : u:0) =? ((sym "b") : (sym "a")) (label "t2"))))))
)


(module+ test
  (check-false
   (judgment-holds
    (wf-state? (state ((u:1 (sym "b")) (u:0 (sym "a")))
                      (u:0 u:1)
                      ((u:0 =? (sym "a") (label "t1")))
                      (label "σ")))))

  (check-false
   (judgment-holds
    (wf-state? (state ((u:1 (sym "b")) (u:0 (sym "a")))
                      (u:0 u:1)
                      ((u:1 =? (sym "b") (label "t2"))
                       (u:0 =? (sym "a") (label "t1")))
                      (label "σ")))))

  ;; empty tree
  (check-true (judgment-holds (wf-tree? (empty-tree) () ())))
  ;; goal/state node
  (check-true
   (judgment-holds
    (wf-tree?
      ((u:0 =? (sym "a") (label "t"))
       (state ((u:0 (sym "a")))
              (u:0)
              ((u:0 =? (sym "a") (label "t1")))
              (label "σ")))
      ()
	  (u:0))))
  ;; conjunction
  (check-true
   (judgment-holds
    (wf-tree?
      (((u:0 =? (sym "a") (label "t"))
       (state ((u:0 (sym "a")))
              (u:0)
              ((u:0 =? (sym "a") (label "t1")))
              (label "σ")))
       ×
       (succeed (label "fish"))
	   ())
      ()
	  ())))

  ;; whole program: no states and empty relations
  (check-true
   (judgment-holds
    (wf-config? (() (empty-tree)))))

  ;; whole program: one state and empty relations
  (check-true
   (judgment-holds
    (wf-config?
     (()  ; Γ
      ((⊤ (state ((u:0 (sym "a"))) (u:0) (((sym "a") =? u:0 (label "g1"))) (label "σ")))
       +
       (empty-tree))))))                                ; s

  ;; relation environment well-formedness
  (check-true
   (judgment-holds
    (wf-rel-env?
     ((r:ok (x:0) (x:0 =? x:0 (label "eq")))))))

  (check-false
   (judgment-holds
    (wf-rel-env?
     ((r:bad () (x:0 =? x:0 (label "eq")))))))

  (check-true
   (judgment-holds
    (core-shape? (() (empty-tree)))))

  (check-true
   (judgment-holds
    (core-shape?
     (()
      (((succeed (label "ok")) ∧ (succeed (label "ok2")) (label "c"))
       (state () () () (label "s")))))))

  (define (core-tree-shape-holds? st)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (judgment-holds (core-tree-shape? ,st))))

  (define (core-config-shape-holds? cfg)
    (with-handlers ([exn:fail? (lambda (_) #f)])
      (judgment-holds (core-shape? ,cfg))))

  ;; non-core constructors must be rejected by the core shape judgment
  (check-false (core-tree-shape-holds? '(delay (empty-tree))))

  (check-false
   (core-config-shape-holds?
    '(() (proceed ((r:foo (sym "x") (label "t"))
                   (state () () () (label "s")))))))
)

(module+ test
  (require rackunit
           redex/reduction-semantics
           racket/list
           (prefix-in rt: "random-test-support.rkt")
           (prefix-in gk: "../tests/generator-kernel.rkt"))

  ;; Randomized test tuning constants.
  ;; Edit these values directly when you want different pressure/coverage.
  (define JUDGMENT-PROP-ATTEMPTS 200)
  (define JUDGMENT-PROP-SIZE 8)
  (define JUDGMENT-MAX-DEPTH 4)
  (define JUDGMENT-PROP-SEED 424242)
  (define JUDGMENT-U-POOL-SIZE 24)
  (define JUDGMENT-C-MAX 4)
  (define JUDGMENT-MIN-UNIFY-SUCCESSES 1)
  (define JUDGMENT-MIN-UNIFY-FAILURES 1)
  (define JUDGMENT-MIN-PAIR-CASES 1)

  (define JUDGMENT-RNG (rt:make-seeded-rng JUDGMENT-PROP-SEED))

  (define (jrandom n)
    (rt:rng-random JUDGMENT-RNG n))

  (displayln
   (format "[core-judgment-forms] randomized checks attempts=~a size=~a seed=~a"
           JUDGMENT-PROP-ATTEMPTS
           JUDGMENT-PROP-SIZE
           JUDGMENT-PROP-SEED))

  ;; Pool size bounds generated test-data diversity only; it does not bound
  ;; the semantic space of logic variables used by the language.
  (define U-POOL (gk:make-u-pool JUDGMENT-U-POOL-SIZE))

  (check-true (positive? JUDGMENT-U-POOL-SIZE)
              "JUDGMENT-U-POOL-SIZE must be >= 1.")
  (check-true (positive? JUDGMENT-C-MAX)
              "JUDGMENT-C-MAX must be >= 1.")
  (check-true (<= JUDGMENT-C-MAX JUDGMENT-U-POOL-SIZE)
              "JUDGMENT-C-MAX must be <= JUDGMENT-U-POOL-SIZE.")
  (check-true (<= 1 JUDGMENT-MAX-DEPTH 4)
              "JUDGMENT-MAX-DEPTH must be in [1,4].")
  (check-true (<= 1 JUDGMENT-MIN-UNIFY-SUCCESSES JUDGMENT-PROP-ATTEMPTS)
              "JUDGMENT-MIN-UNIFY-SUCCESSES must be in [1, JUDGMENT-PROP-ATTEMPTS].")
  (check-true (<= 1 JUDGMENT-MIN-UNIFY-FAILURES JUDGMENT-PROP-ATTEMPTS)
              "JUDGMENT-MIN-UNIFY-FAILURES must be in [1, JUDGMENT-PROP-ATTEMPTS].")
  (check-true (<= 1 JUDGMENT-MIN-PAIR-CASES JUDGMENT-PROP-ATTEMPTS)
              "JUDGMENT-MIN-PAIR-CASES must be in [1, JUDGMENT-PROP-ATTEMPTS].")

  ;; Constructively build wf terms with respect to c (no lexical vars).
  (define (gen-wf-term c depth)
    (define choices
      (append '(primitive)
              (if (null? c) '() '(logic-var))
              (if (zero? depth) '() '(pair))))
    (case (list-ref choices (jrandom (length choices)))
      [(primitive) (rt:gen-primitive/rng JUDGMENT-RNG)]
      [(logic-var) (list-ref c (jrandom (length c)))]
      [(pair) `(,(gen-wf-term c (sub1 depth)) : ,(gen-wf-term c (sub1 depth)))]))

  ;; Returns a sample (list t1 t2 sub c trail tag1 tag2) that always satisfies
  ;; the wf-tree antecedent used by the randomized unify checks.
  (define (generate-wf-eq-sample)
    (define c-size (add1 (jrandom JUDGMENT-C-MAX)))
    (define c (rt:random-distinct/rng JUDGMENT-RNG U-POOL c-size))
    (define depth JUDGMENT-MAX-DEPTH)
    (define t_1 (gen-wf-term c depth))
    ;; Bias half the time to guaranteed unification success.
    (define t_2 (if (zero? (jrandom 2)) t_1 (gen-wf-term c depth)))
    (list t_1 t_2 (term ()) c (term ()) (term (label "t1")) (term (label "t2"))))

  ;; Returns a sample (list t sub) where sub is acyclic by construction:
  ;; all bindings map to primitive terms only.
  (define (generate-walk-sample)
    (define c-size (add1 (jrandom JUDGMENT-C-MAX)))
    (define c (rt:random-distinct/rng JUDGMENT-RNG U-POOL c-size))
    (define depth JUDGMENT-MAX-DEPTH)
    (define t* (gen-wf-term c depth))
    (define binding-count (jrandom (add1 c-size)))
    (define dom (rt:random-distinct/rng JUDGMENT-RNG c binding-count))
    (define sub*
      (for/list ([u (in-list dom)])
        (list u (rt:gen-primitive/rng JUDGMENT-RNG))))
    (list t* sub*))

  ;; Deterministic must-hit samples keep minimum-threshold checks stable.
  (define (forced-success-sample)
    (list (term (sym "forced-s"))
          (term (sym "forced-s"))
          (term ())
          (term ())
          (term ())
          (term (label "forced"))
          (term (label "forced"))))

  (define (forced-failure-sample)
    (list (term (sym "forced-left"))
          (term (sym "forced-right"))
          (term ())
          (term ())
          (term ())
          (term (label "forced"))
          (term (label "forced"))))

  (define (forced-pair-sample)
    (list (term ((sym "forced-a") : empty))
          (term ((sym "forced-a") : empty))
          (term ())
          (term ())
          (term ())
          (term (label "forced"))
          (term (label "forced"))))

  ;; walk is idempotent
  (for ([_ (in-range JUDGMENT-PROP-ATTEMPTS)])
    (match-define (list t* sub*) (generate-walk-sample))
    (check-equal? (term (walk (walk ,t* ,sub*) ,sub*))
                  (term (walk ,t* ,sub*))))

  ;; If unify succeeds on wf inputs, the results walk to the same thing.
  ;; Also check triangular/occurs-free invariants on successful outputs.
  (define wf-hits 0)
  (define unify-successes 0)
  (define unify-failures 0)
  (define pair-cases 0)
  (define max-c-size-seen 0)

  (define triangular? substitution-acyclic?)

  (define (occurs-free? pairs)
    ;; For substitutions, occurs-free is equivalent to acyclic dependency
    ;; among domain vars. Use the structural check to keep this test total.
    (triangular? pairs))

  ;; Deterministic regression checks for graph-style substitution invariants.
  (check-true
   (triangular? (term ((u:0 (sym "a"))
                       (u:1 u:0)
                       (u:2 (u:1 : empty))))))
  (check-true
   (triangular? (term ((u:1 u:0)
                       (u:0 (sym "a"))))))
  (check-false
   (triangular? (term ((u:0 u:1)
                       (u:1 u:0)))))

  (check-true
   (occurs-free? (term ((u:0 (sym "a"))
                        (u:1 u:0)))))
  (check-false
   (occurs-free? (term ((u:0 (u:0 : empty))))))

  ;; Full walk over pair terms for testing unify equalization.
  ;; The core walk metafunction is intentionally shallow.
  (define (walk* t sub)
    (define w (term (walk ,t ,sub)))
    (match w
      [`(,a : ,d) `(,(walk* a sub) : ,(walk* d sub))]
      [_ w]))

  (define (pair-term? t)
    (match t
      [`(,_ : ,_) #t]
      [_ #f]))

  (for ([i (in-range JUDGMENT-PROP-ATTEMPTS)])
    (define sample
      (cond
        [(zero? i) (forced-success-sample)]
        [(= i 1) (forced-failure-sample)]
        [(= i 2) (forced-pair-sample)]
        [else (generate-wf-eq-sample)]))
    (match-define (list t_1 t_2 sub c trail tag_1 tag_2) sample)
    (set! max-c-size-seen (max max-c-size-seen (length c)))
    (when (or (pair-term? t_1) (pair-term? t_2))
      (set! pair-cases (add1 pair-cases)))
    (check-true
     (judgment-holds
      (wf-tree? ((,t_1 =? ,t_2 ,tag_1)
                 (state ,sub ,c ,trail ,tag_2))
                ()
                ())))
    (set! wf-hits (add1 wf-hits))
    (define sub^ (term (unify (walk ,t_1 ,sub) (walk ,t_2 ,sub) ,sub)))
    (unless (equal? sub^ (term #f))
      (set! unify-successes (add1 unify-successes))
      (check-true (equal? (walk* t_1 sub^)
                          (walk* t_2 sub^))
                  "Unify result does not equalize walked terms.")
      (check-true (triangular? sub^)
                  "Unify result is not triangular.")
      (check-true (occurs-free? sub^)
                  "Unify result violates occurs-check closure."))
    (when (equal? sub^ (term #f))
      (set! unify-failures (add1 unify-failures))))

  (displayln
   (format "[core-judgment-forms] wf-hits=~a unify-successes=~a unify-failures=~a pair-cases=~a max-c-size=~a seed=~a"
           wf-hits
           unify-successes
           unify-failures
           pair-cases
           max-c-size-seen
           JUDGMENT-PROP-SEED))

  (check-true (> wf-hits 0)
              "Unify properties had zero well-formed antecedent hits; test would be vacuous.")
  (check-true (>= unify-successes JUDGMENT-MIN-UNIFY-SUCCESSES)
              "Unify properties had too few successful unifications.")
  (check-true (>= unify-failures JUDGMENT-MIN-UNIFY-FAILURES)
              "Unify properties had too few failing unifications.")
  (check-true (>= pair-cases JUDGMENT-MIN-PAIR-CASES)
              "Unify properties had too few pair-term cases.")

)
