#lang racket

(require rackunit
         rackunit/text-ui
         racket/format
         racket/list
         redex/reduction-semantics
         (prefix-in rt: "../src/random-test-support.rkt")
         (prefix-in gk: "./generator-kernel.rkt")
         "./variant-test-support.rkt"
         "../src/languages/all.rkt"
         "../src/reduction-relations/all.rkt")

(provide PROPERTY-VARIANTS-RANDOM)

;; Randomized variant testing constants.
(define VR-ATTEMPTS 160)
(define VR-MAX-DEPTH 4)
(define VR-SEEDS '(424242 777777 20260227))
(define VR-U-POOL-SIZE 24)
(define VR-X-POOL-SIZE 16)
(define VR-R-POOL-SIZE 16)
(define VR-C-MAX 4)
(define VR-C-EXTRA-MAX 2)

(define VR-MIN-CALL-GEN-HITS 2)
(define VR-MIN-DISJ-GEN-HITS 2)
(define VR-MIN-LEFT-TREE-HITS 2)
(define VR-MIN-DELAY-HITS 2)
(define VR-MIN-RIGHT-TREE-HITS 1)
(define VR-EXPECTED-ANTE-HITS VR-ATTEMPTS)
(define VR-K-STEP-DEPTH 3)
(define VR-MIN-CALL-RULE-HITS 2)
(define VR-MIN-DISJ-RULE-HITS 2)
(define VR-MIN-FLIP-RULE-HITS 2)
(define VR-MIN-RAIL-RULE-HITS 2)

(gk:require-positive 'VR-ATTEMPTS VR-ATTEMPTS 'property-variants-random)
(gk:require-positive 'VR-MAX-DEPTH VR-MAX-DEPTH 'property-variants-random)
(gk:require-positive 'VR-U-POOL-SIZE VR-U-POOL-SIZE 'property-variants-random)
(gk:require-positive 'VR-X-POOL-SIZE VR-X-POOL-SIZE 'property-variants-random)
(gk:require-positive 'VR-R-POOL-SIZE VR-R-POOL-SIZE 'property-variants-random)
(gk:require-positive 'VR-C-MAX VR-C-MAX 'property-variants-random)
(gk:require-positive 'VR-EXPECTED-ANTE-HITS VR-EXPECTED-ANTE-HITS 'property-variants-random)
(gk:require-positive 'VR-K-STEP-DEPTH VR-K-STEP-DEPTH 'property-variants-random)
(gk:require-nonnegative 'VR-C-EXTRA-MAX VR-C-EXTRA-MAX 'property-variants-random)
(unless (<= VR-C-MAX VR-U-POOL-SIZE)
  (error 'property-variants-random
         (format "VR-C-MAX must be <= VR-U-POOL-SIZE, got ~a > ~a"
                 VR-C-MAX VR-U-POOL-SIZE)))
(unless (<= VR-EXPECTED-ANTE-HITS VR-ATTEMPTS)
  (error 'property-variants-random
         (format "VR-EXPECTED-ANTE-HITS must be <= VR-ATTEMPTS, got ~a > ~a"
                 VR-EXPECTED-ANTE-HITS VR-ATTEMPTS)))

(define U-POOL
  (gk:make-u-pool VR-U-POOL-SIZE))

(define X-POOL
  (gk:make-x-pool VR-X-POOL-SIZE))

(define R-POOL
  (gk:make-r-pool VR-R-POOL-SIZE))

(struct gopts (calls? disj-goal? left-tree? exists?) #:transparent)

(define (vrandom rng n)
  (rt:rng-random rng n))

(define (pick-one rng xs)
  (gk:pick-one/rng rng xs))

(define (make-label rng prefix)
  (gk:make-label/rng rng prefix))

(define (extend-c/rng rng c max-extra)
  (gk:extend-c/rng rng c U-POOL VR-C-MAX max-extra))

(define (gen-term/rng rng x-env c depth)
  (gk:gen-term/rng rng x-env c depth))

(define (gen-eq-goal/rng rng x-env c depth)
  `(,(gen-term/rng rng x-env c depth)
    =?
    ,(gen-term/rng rng x-env c depth)
    ,(make-label rng "eq")))

(define (fresh-x-list/rng rng x-env)
  (gk:fresh-x-list/rng rng x-env X-POOL))

(define (gen-goal/rng rng x-env c depth rel-sig opts)
  (define call-enabled? (and (gopts-calls? opts) (pair? rel-sig)))
  (define options
    (append '(succeed eq)
            (if (zero? depth) '() '(conj))
            (if (and (positive? depth) (gopts-exists? opts)) '(exists) '())
            (if call-enabled? '(call) '())
            (if (and (gopts-disj-goal? opts) (positive? depth)) '(disj) '())))
  (case (pick-one rng options)
    [(succeed) `(succeed ,(make-label rng "ok"))]
    [(eq) (gen-eq-goal/rng rng x-env c depth)]
    [(conj)
     `(,(gen-goal/rng rng x-env c (sub1 depth) rel-sig opts)
       ∧
       ,(gen-goal/rng rng x-env c (sub1 depth) rel-sig opts)
       ,(make-label rng "and"))]
    [(exists)
     (define d (fresh-x-list/rng rng x-env))
     (define body
       (if (null? d)
           `(succeed ,(make-label rng "ok"))
           `(,(car d)
             =?
             ,(rt:gen-primitive/rng rng)
             ,(make-label rng "eq"))))
     `(∃
       ,d
       ,body
       ,(make-label rng "ex"))]
    [(disj)
     `(,(gen-goal/rng rng x-env c (sub1 depth) rel-sig opts)
       ∨
       ,(gen-goal/rng rng x-env c (sub1 depth) rel-sig opts)
       ,(make-label rng "or"))]
    [(call)
     (match-define (cons r arity) (pick-one rng rel-sig))
     `(,r
       ,@(for/list ([_ (in-range arity)])
           (gen-term/rng rng x-env c (max 0 (sub1 depth))))
       ,(make-label rng "call"))]))

(define (gen-state/rng rng c)
  `(state () () ,c () ,(make-label rng "st")))

(define (gen-tree-user/rng rng c depth rel-sig opts)
  (define options
    (append '(empty answer goal-state)
            (if (zero? depth) '() '(conj-tree))
            (if (and (gopts-left-tree? opts) (positive? depth)) '(left-disj-tree) '())))
  (case (pick-one rng options)
    [(empty) '(empty-tree)]
    [(answer)
     (define c^ (extend-c/rng rng c VR-C-EXTRA-MAX))
     `(⊤ ,(gen-state/rng rng c^))]
    [(goal-state)
     (define c^ (extend-c/rng rng c VR-C-EXTRA-MAX))
     `(,(gen-goal/rng rng '() c^ depth rel-sig opts)
       ,(gen-state/rng rng c^))]
    [(conj-tree)
     (define c^ (extend-c/rng rng c VR-C-EXTRA-MAX))
     `(,(gen-tree-user/rng rng c^ (sub1 depth) rel-sig opts)
       ×
       ,(gen-goal/rng rng '() c^ (sub1 depth) rel-sig opts)
       ,c^)]
    [(left-disj-tree)
     `(,(gen-tree-user/rng rng c (sub1 depth) rel-sig opts)
       <-+
       ,(gen-tree-user/rng rng c (sub1 depth) rel-sig opts))]))

(define (gen-rel-sig/rng rng calls?)
  (define count
    (if calls?
        (add1 (vrandom rng 3))
        (vrandom rng 3)))
  (for/list ([r (in-list (rt:random-distinct/rng rng R-POOL count))])
    (cons r (vrandom rng 3))))

(define (gen-rel-def/rng rng rel-ar rel-sig opts)
  (match-define (cons r arity) rel-ar)
  (define d
    (take (rt:random-distinct/rng rng X-POOL arity) arity))
  ;; Relation bodies are restricted to core-goal forms because subst-goal in
  ;; core-definitions currently covers succeed/eq/conj/exists only.
  (define core-opts (gopts #f #f #f #t))
  `(,r
    ,d
    ,(gen-goal/rng rng d '() VR-MAX-DEPTH rel-sig core-opts)))

(define (gen-rel-env/rng rng opts)
  (define rel-sig (gen-rel-sig/rng rng (gopts-calls? opts)))
  (values
   (for/list ([ra (in-list rel-sig)])
     (gen-rel-def/rng rng ra rel-sig opts))
   rel-sig))

(define (gen-config-user/rng rng opts)
  (define-values (gamma rel-sig) (gen-rel-env/rng rng opts))
  (define c0 (extend-c/rng rng '() VR-C-EXTRA-MAX))
  (define s (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  `(,gamma ,s (empty-stream)))

(define (gen-config-delay-left-disj-admin/rng rng)
  (define opts (gopts #t #t #t #t))
  (define-values (gamma rel-sig) (gen-rel-env/rng rng opts))
  (define c0 (extend-c/rng rng '() VR-C-EXTRA-MAX))
  (define s1 (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  (define s2 (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  `(,gamma ((delay ,s1) <-+ ,s2) (empty-stream)))

(define (state-c-size st)
  (match st
    [`(state ,_ ,_ ,c ,_ ,_) (length c)]
    [_ 0]))

(define (goal-flags g [call? #f] [disj? #f] [exists? #f] [conj? #f])
  (match g
    [`(succeed ,_) (values call? disj? exists? conj?)]
    [`(,_ =? ,_ ,_) (values call? disj? exists? conj?)]
    [`(∃ ,_ ,g2 ,_)
     (goal-flags g2 call? disj? #t (or #t conj?))]
    [`(,g1 ∧ ,g2 ,_)
     (define-values (call1 disj1 exists1 _conj1)
       (goal-flags g1 call? disj? exists? #t))
     (goal-flags g2 call1 disj1 exists1 #t)]
    [`(,g1 ∨ ,g2 ,_)
     (define-values (call1 _disj1 exists1 conj1)
       (goal-flags g1 call? #t exists? conj?))
     (goal-flags g2 call1 #t exists1 conj1)]
    [`(,r ,_ ... ,_)
     (if (and (symbol? r)
              (regexp-match? #rx"^r:" (symbol->string r)))
         (values #t disj? exists? conj?)
         (values call? disj? exists? conj?))]
    [_ (values call? disj? exists? conj?)]))

(define (tree-flags s
                    [call? #f]
                    [disj? #f]
                    [exists? #f]
                    [conj? #f]
                    [left? #f]
                    [delay? #f]
                    [right? #f]
                    [cmax 0])
  (match s
    [`(empty-tree) (values call? disj? exists? conj? left? delay? right? cmax)]
    [`(⊤ ,st)
     (values call? disj? exists? conj? left? delay? right?
             (max cmax (state-c-size st)))]
    [`(,g ,st)
     (define-values (call1 disj1 exists1 conj1)
       (goal-flags g call? disj? exists? conj?))
     (values call1 disj1 exists1 conj1 left? delay? right?
             (max cmax (state-c-size st)))]
    [`(,s1 × ,g ,c)
     (define-values (call1 disj1 exists1 conj1 left1 delay1 right1 cmax1)
       (tree-flags s1 call? disj? exists? conj? left? delay? right? cmax))
     (define-values (call2 disj2 exists2 conj2)
       (goal-flags g call1 disj1 exists1 conj1))
     (values call2 disj2 exists2 conj2 left1 delay1 right1
             (max cmax1 (length c)))]
    [`(delay ,s1)
     (tree-flags s1 call? disj? exists? conj? left? #t right? cmax)]
    [`(proceed (,g ,_σ))
     (define-values (call1 disj1 exists1 conj1)
       (goal-flags g call? disj? exists? conj?))
     (values call1 disj1 exists1 conj1 left? delay? right? cmax)]
    [`(proceed ((,r ,_ ... ,_) ,_σ))
     (if (and (symbol? r)
              (regexp-match? #rx"^r:" (symbol->string r)))
         (values #t disj? exists? conj? left? delay? right? cmax)
         (values call? disj? exists? conj? left? delay? right? cmax))]
    [`(,s1 <-+ ,s2)
     (define-values (call1 disj1 exists1 conj1 left1 delay1 right1 cmax1)
       (tree-flags s1 call? disj? exists? conj? #t delay? right? cmax))
     (tree-flags s2 call1 disj1 exists1 conj1 #t delay1 right1 cmax1)]
    [`(,s1 +-> ,s2)
     (define-values (call1 disj1 exists1 conj1 left1 delay1 right1 cmax1)
       (tree-flags s1 call? disj? exists? conj? left? delay? #t cmax))
     (tree-flags s2 call1 disj1 exists1 conj1 left1 delay1 #t cmax1)]
    [_ (values call? disj? exists? conj? left? delay? right? cmax)]))

(define (config-flags cfg)
  (match cfg
    [`(,gamma ,s ,_as)
     (define-values (call? disj? exists? conj?)
       (for/fold ([call? #f]
                  [disj? #f]
                  [exists? #f]
                  [conj? #f])
                 ([rel (in-list gamma)])
         (match rel
           [`(,_ ,_ ,g) (goal-flags g call? disj? exists? conj?)]
           [_ (values call? disj? exists? conj?)])))

     (define-values (hc hd he hj hl hdelay hr cmax-tree) (tree-flags s))
     (values (or call? hc)
             (or disj? hd)
             (or exists? he)
             (or conj? hj)
             hl
             hdelay
             hr
             cmax-tree)]
    [`(,gamma ,s)
     (define-values (call? disj? exists? conj?)
       (for/fold ([call? #f]
                  [disj? #f]
                  [exists? #f]
                  [conj? #f])
                 ([rel (in-list gamma)])
         (match rel
           [`(,_ ,_ ,g) (goal-flags g call? disj? exists? conj?)]
           [_ (values call? disj? exists? conj?)])))

     (define-values (hc hd he hj hl hdelay hr cmax-tree) (tree-flags s))
     (values (or call? hc)
             (or disj? hd)
             (or exists? he)
             (or conj? hj)
             hl
             hdelay
             hr
             cmax-tree)]
    [_ (values #f #f #f #f #f #f #f 0)]))

(define (tree-contains-left? s)
  (match s
    [`(,s1 <-+ ,s2) #t]
    [(cons a d) (or (tree-contains-left? a) (tree-contains-left? d))]
    [_ #f]))

(define (tree-contains-delay? s)
  (match s
    [`(delay ,_) #t]
    [(cons a d) (or (tree-contains-delay? a) (tree-contains-delay? d))]
    [_ #f]))

(define (tree-contains-right? s)
  (match s
    [`(,s1 +-> ,s2) #t]
    [(cons a d) (or (tree-contains-right? a) (tree-contains-right? d))]
    [_ #f]))

(define (name-has-prefix? name prefix)
  (regexp-match? (regexp (format "^~a" prefix))
                 (cond
                   [(symbol? name) (symbol->string name)]
                   [(string? name) name]
                   [else (format "~a" name)])))

(define (step-names-for rel cfg)
  (map first (apply-reduction-relation/tag-with-names rel cfg)))

;; Check theorem-style consequents up to k steps:
;; if cfg is wf, then it stays in-language, remains wf, progresses unless final,
;; and (optionally) has unique decomposition at each explored node.
(define (k-step-consequent-failure rel shape-match? cfg k require-unique? require-progress?)
  (define (loop cfg fuel)
    (cond
      [(not (shape-match? cfg))
       (list 'shape cfg)]
      [(not (states-wf? cfg))
       (list 'state-wf cfg)]
      [else
       (define next* (apply-reduction-relation rel cfg))
       (cond
         [(and require-progress?
               (not (final-config? cfg))
               (null? next*))
          (list 'progress
                cfg
                (step-names-for rel cfg))]
         [(and require-unique?
               (if (final-config? cfg)
                   (not (null? next*))
                   (not (= (length next*) 1))))
          (list 'unique
                cfg
                (length next*)
                (step-names-for rel cfg))]
         [(zero? fuel) #f]
         [else
          (for/or ([cfg^ (in-list next*)])
            (loop cfg^ (sub1 fuel)))])]))
  (loop cfg k))

(define metric-count-keys
  '(fail-count
    ante-hits
    call-gen-hits
    disj-gen-hits
    left-tree-hits
    delay-hits
    right-tree-hits
    right-next-hits
    call-rule-hits
    disj-rule-hits
    flip-rule-hits
    rail-rule-hits
    max-c-seen
    k-shape-fails
    k-state-wf-fails
    k-progress-fails
    k-unique-fails))

(define (metrics-empty)
  (for/fold ([m (hasheq 'fail-samples '())])
            ([k (in-list metric-count-keys)])
    (hash-set m k 0)))

(define (metrics-ref m key)
  (hash-ref m key 0))

(define (metrics-inc m key [delta 1])
  (hash-update m key (lambda (v) (+ v delta)) 0))

(define (metrics-max m key n)
  (hash-set m key (max (metrics-ref m key) n)))

(define (metrics-inc-if m key pred?)
  (if pred? (metrics-inc m key) m))

(define (metrics-add-fail m sample)
  (define m+ (metrics-inc m 'fail-count))
  (define fail-samples (hash-ref m+ 'fail-samples '()))
  (if (< (length fail-samples) 3)
      (hash-set m+ 'fail-samples (cons sample fail-samples))
      m+))

(define (metrics-inc-rule-prefixes m name)
  (define call-step?
    (regexp-match?
     #rx"/(?:suspend-goal|eager-expand|lazy-expand|invoke-delay|eager-resume-goal|lazy-expand-on-resume|delay-through-conj)$"
     name))
  (define disj-step?
    (regexp-match?
     #rx"/(?:goal-to-tree|distribute-over-conj|bubble-left-answer|promote-left-answer|bubble-left-fail|skip-left-fail)$"
     name))
  (define m1 (metrics-inc-if m 'call-rule-hits call-step?))
  (define m2 (metrics-inc-if m1 'disj-rule-hits disj-step?))
  (define m3 (metrics-inc-if m2 'flip-rule-hits (name-has-prefix? name "l3-flip/")))
  (metrics-inc-if m3 'rail-rule-hits (name-has-prefix? name "l4-rail/")))

(define (metrics-inc-k-fail m fail-info)
  (match fail-info
    [(list 'shape _) (metrics-inc m 'k-shape-fails)]
    [(list 'state-wf _) (metrics-inc m 'k-state-wf-fails)]
    [(list 'progress _ _) (metrics-inc m 'k-progress-fails)]
    [(list 'unique _ _ _) (metrics-inc m 'k-unique-fails)]
    [_ m]))

(define (record-config-coverage m cfg)
  (define-values (has-call has-disj _has-exists _has-conj has-left has-delay has-right cmax)
    (config-flags cfg))
  (define tree (second cfg))
  (metrics-max
   (metrics-inc-if
    (metrics-inc-if
     (metrics-inc-if
      (metrics-inc-if
       (metrics-inc-if m 'call-gen-hits has-call)
       'disj-gen-hits has-disj)
      'left-tree-hits (or has-left (tree-contains-left? tree)))
     'delay-hits (or has-delay (tree-contains-delay? tree)))
    'right-tree-hits (or has-right (tree-contains-right? tree)))
   'max-c-seen
   cmax))

(define (record-rule-coverage m rel cfg)
  (for/fold ([m* m])
            ([item (in-list (apply-reduction-relation/tag-with-names rel cfg))])
    (match item
      [(list name cfg-next)
       (define m+ (metrics-inc-rule-prefixes m* name))
       (metrics-inc-if m+
                       'right-next-hits
                       (tree-contains-right? (second cfg-next)))]
      [_ m*])))

(define (record-k-step-consequent m rel shape-match? cfg k-depth require-unique? require-progress?)
  (if (and (shape-match? cfg) (states-wf? cfg))
      (let* ([m+ (metrics-inc m 'ante-hits)]
             [fail-info (k-step-consequent-failure rel
                                                   shape-match?
                                                   cfg
                                                   k-depth
                                                   require-unique?
                                                   require-progress?)])
        (if fail-info
            (metrics-add-fail
             (metrics-inc-k-fail m+ fail-info)
             (list 'k-step-consequent-fail fail-info cfg))
            m+))
      m))

(define (record-shape-closure m rel shape-closed? cfg)
  (if (shape-closed? rel cfg)
      m
      (metrics-add-fail m (list 'shape-closed cfg))))

(define (record-attempt m rel shape-match? shape-closed? cfg
                        k-depth require-unique? require-progress?)
  (define m1
    (if (shape-match? cfg)
        m
        (metrics-add-fail m (list 'shape cfg))))
  (define m2 (record-config-coverage m1 cfg))
  (with-handlers
      ([exn:fail?
        (lambda (e)
          (metrics-add-fail
           m2
           (list 'exception (exn-message e) cfg)))])
    (define m3 (record-rule-coverage m2 rel cfg))
    (define m4
      (record-k-step-consequent m3
                                rel
                                shape-match?
                                cfg
                                k-depth
                                require-unique?
                                require-progress?))
    (record-shape-closure m4 rel shape-closed? cfg)))

(define (run-random-seed rel shape-match? shape-closed? cfg-generator seed
                         k-depth require-unique? require-progress?)
  (define rng (rt:make-seeded-rng seed))
  (for/fold ([m (metrics-empty)])
            ([_ (in-range VR-ATTEMPTS)])
    (record-attempt m
                    rel
                    shape-match?
                    shape-closed?
                    (cfg-generator rng)
                    k-depth
                    require-unique?
                    require-progress?)))

(define (metrics->view metrics)
  (define (m key) (metrics-ref metrics key))
  (values (m 'fail-count)
          (hash-ref metrics 'fail-samples '())
          (m 'ante-hits)
          (m 'call-gen-hits)
          (m 'disj-gen-hits)
          (m 'left-tree-hits)
          (m 'delay-hits)
          (m 'right-tree-hits)
          (m 'right-next-hits)
          (m 'call-rule-hits)
          (m 'disj-rule-hits)
          (m 'flip-rule-hits)
          (m 'rail-rule-hits)
          (m 'max-c-seen)
          (m 'k-shape-fails)
          (m 'k-state-wf-fails)
          (m 'k-progress-fails)
          (m 'k-unique-fails)))

(define (run-random-variant label rel shape-match? shape-closed? cfg-generator
                            #:require-unique? [require-unique? #t]
                            #:require-progress? [require-progress? #t]
                            #:expected-ante-hits [expected-ante-hits VR-EXPECTED-ANTE-HITS]
                            #:min-call-gen [min-call-gen 0]
                            #:min-disj-gen [min-disj-gen 0]
                            #:min-left-tree [min-left-tree 0]
                            #:min-delay [min-delay 0]
                            #:min-right-tree [min-right-tree 0]
                            #:min-right-next [min-right-next 0]
                            #:min-call-rules [min-call-rules 0]
                            #:min-disj-rules [min-disj-rules 0]
                            #:min-flip-rules [min-flip-rules 0]
                            #:min-rail-rules [min-rail-rules 0]
                            #:k-depth [k-depth VR-K-STEP-DEPTH])
  (for ([seed (in-list VR-SEEDS)])
    (define metrics
      (run-random-seed rel
                       shape-match?
                       shape-closed?
                       cfg-generator
                       seed
                       k-depth
                       require-unique?
                       require-progress?))
    (define-values (fail-count
                    fail-samples
                    ante-hits
                    call-gen-hits
                    disj-gen-hits
                    left-tree-hits
                    delay-hits
                    right-tree-hits
                    right-next-hits
                    call-rule-hits
                    disj-rule-hits
                    flip-rule-hits
                    rail-rule-hits
                    max-c-seen
                    k-shape-fails
                    k-state-wf-fails
                    k-progress-fails
                    k-unique-fails)
      (metrics->view metrics))

    (displayln
     (format "[property-variants-random] ~a seed=~a attempts=~a ante-hits=~a fails=~a k-fails(shape/state/progress/unique)=~a/~a/~a/~a gen(call/disj/left/delay/right)=~a/~a/~a/~a/~a next-right=~a rule(call/disj/flip/rail)=~a/~a/~a/~a max-c=~a"
             label
             seed
             VR-ATTEMPTS
             ante-hits
             fail-count
             k-shape-fails
             k-state-wf-fails
             k-progress-fails
             k-unique-fails
             call-gen-hits
             disj-gen-hits
             left-tree-hits
             delay-hits
             right-tree-hits
             right-next-hits
             call-rule-hits
             disj-rule-hits
             flip-rule-hits
             rail-rule-hits
             max-c-seen))

    (check-equal? fail-count
                  0
                  (format "~a seed=~a counterexamples (up to 3): ~s"
                          label
                          seed
                          (reverse fail-samples)))
    (check-equal? ante-hits expected-ante-hits
                  (format "~a seed=~a: antecedent coverage mismatch (~a != ~a)"
                          label seed ante-hits expected-ante-hits))
    (check-equal? k-shape-fails
                  0
                  (format "~a seed=~a: k-step shape preservation failures: ~a"
                          label seed k-shape-fails))
    (check-equal? k-state-wf-fails
                  0
                  (format "~a seed=~a: k-step state-wf preservation failures: ~a"
                          label seed k-state-wf-fails))
    (when require-progress?
      (check-equal? k-progress-fails
                    0
                    (format "~a seed=~a: k-step progress failures: ~a"
                            label seed k-progress-fails)))
    (when require-unique?
      (check-equal? k-unique-fails
                    0
                    (format "~a seed=~a: k-step uniqueness failures: ~a"
                            label seed k-unique-fails)))
    (check-true (>= call-gen-hits min-call-gen)
                (format "~a seed=~a: call coverage too low (~a < ~a)"
                        label seed call-gen-hits min-call-gen))
    (check-true (>= disj-gen-hits min-disj-gen)
                (format "~a seed=~a: disj coverage too low (~a < ~a)"
                        label seed disj-gen-hits min-disj-gen))
    (check-true (>= left-tree-hits min-left-tree)
                (format "~a seed=~a: left-tree coverage too low (~a < ~a)"
                        label seed left-tree-hits min-left-tree))
    (check-true (>= delay-hits min-delay)
                (format "~a seed=~a: delay coverage too low (~a < ~a)"
                        label seed delay-hits min-delay))
    (check-true (>= right-tree-hits min-right-tree)
                (format "~a seed=~a: right-tree coverage too low (~a < ~a)"
                        label seed right-tree-hits min-right-tree))
    (check-true (>= right-next-hits min-right-next)
                (format "~a seed=~a: next-state right-tree coverage too low (~a < ~a)"
                        label seed right-next-hits min-right-next))
    (check-true (>= call-rule-hits min-call-rules)
                (format "~a seed=~a: call-rule coverage too low (~a < ~a)"
                        label seed call-rule-hits min-call-rules))
    (check-true (>= disj-rule-hits min-disj-rules)
                (format "~a seed=~a: disj-rule coverage too low (~a < ~a)"
                        label seed disj-rule-hits min-disj-rules))
    (check-true (>= flip-rule-hits min-flip-rules)
                (format "~a seed=~a: flip-rule coverage too low (~a < ~a)"
                        label seed flip-rule-hits min-flip-rules))
    (check-true (>= rail-rule-hits min-rail-rules)
                (format "~a seed=~a: rail-rule coverage too low (~a < ~a)"
                        label seed rail-rule-hits min-rail-rules))))

(define-test-suite VARIANT-RANDOM-PROPERTIES
  (test-case "L1 Rl1-call-eager randomized"
    (define opts (gopts #t #f #f #t))
    (run-random-variant "Rl1-call-eager"
                        Rl1-call-eager
                        (lambda (cfg) (redex-match? L1 config cfg))
                        shape-closed/L1?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS))

  (test-case "L1 Rl1-call-lazy randomized"
    (define opts (gopts #t #f #f #t))
    (run-random-variant "Rl1-call-lazy"
                        Rl1-call-lazy
                        (lambda (cfg) (redex-match? L1 config cfg))
                        shape-closed/L1?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS))

  (test-case "L2 Rl2-disj-left randomized"
    (define opts (gopts #f #t #f #f))
    (run-random-variant "Rl2-disj-left"
                        Rl2-disj-left
                        (lambda (cfg) (redex-match? L2 config cfg))
                        shape-closed/L2?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-disj-rules VR-MIN-DISJ-RULE-HITS))

  (test-case "L3 Rl3-base-eager randomized"
    (define opts (gopts #t #t #f #t))
    (run-random-variant "Rl3-base-eager"
                        Rl3-base-eager
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #f
                        #:require-progress? #f
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS
                        #:min-disj-rules VR-MIN-DISJ-RULE-HITS))

  (test-case "L3 Rl3-base-lazy randomized"
    (define opts (gopts #t #t #f #t))
    (run-random-variant "Rl3-base-lazy"
                        Rl3-base-lazy
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #f
                        #:require-progress? #f
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS
                        #:min-disj-rules VR-MIN-DISJ-RULE-HITS))

  (test-case "L3 Rl3-flip-eager randomized admin-fragment"
    (run-random-variant "Rl3-flip-eager"
                        Rl3-flip-eager
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        gen-config-delay-left-disj-admin/rng
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-left-tree VR-MIN-LEFT-TREE-HITS
                        #:min-delay VR-MIN-DELAY-HITS
                        #:min-flip-rules VR-MIN-FLIP-RULE-HITS))

  (test-case "L3 Rl3-flip-lazy randomized admin-fragment"
    (run-random-variant "Rl3-flip-lazy"
                        Rl3-flip-lazy
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        gen-config-delay-left-disj-admin/rng
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-left-tree VR-MIN-LEFT-TREE-HITS
                        #:min-delay VR-MIN-DELAY-HITS
                        #:min-flip-rules VR-MIN-FLIP-RULE-HITS))

  (test-case "L4 Rl4-rail-eager randomized admin-fragment"
    (run-random-variant "Rl4-rail-eager"
                        Rl4-rail-eager
                        (lambda (cfg) (redex-match? L4 config cfg))
                        shape-closed/L4?
                        gen-config-delay-left-disj-admin/rng
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-left-tree VR-MIN-LEFT-TREE-HITS
                        #:min-delay VR-MIN-DELAY-HITS
                        #:min-right-tree 0
                        #:min-right-next VR-MIN-RIGHT-TREE-HITS
                        #:min-rail-rules VR-MIN-RAIL-RULE-HITS))

  (test-case "L4 Rl4-rail-lazy randomized admin-fragment"
    (run-random-variant "Rl4-rail-lazy"
                        Rl4-rail-lazy
                        (lambda (cfg) (redex-match? L4 config cfg))
                        shape-closed/L4?
                        gen-config-delay-left-disj-admin/rng
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-left-tree VR-MIN-LEFT-TREE-HITS
                        #:min-delay VR-MIN-DELAY-HITS
                        #:min-right-tree 0
                        #:min-right-next VR-MIN-RIGHT-TREE-HITS
                        #:min-rail-rules VR-MIN-RAIL-RULE-HITS)))

(define/provide-test-suite PROPERTY-VARIANTS-RANDOM
  #:before (thunk
            (displayln
             (format "Running variant randomized tests (attempts=~a, depth=~a, seeds=~s)..."
                     VR-ATTEMPTS VR-MAX-DEPTH VR-SEEDS)))
  #:after (thunk (displayln "Finished variant randomized tests."))
  VARIANT-RANDOM-PROPERTIES)

(module+ test
  (run-tests PROPERTY-VARIANTS-RANDOM))
