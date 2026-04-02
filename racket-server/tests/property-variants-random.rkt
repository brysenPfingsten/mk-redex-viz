#lang racket

(require rackunit
         rackunit/text-ui
         racket/format
         racket/list
         redex/reduction-semantics
         (prefix-in h: "./helpers.rkt")
         "./variant-test-support.rkt"
         "../src/extensions/variant-languages.rkt"
         "../src/reduction-relations/extensions/variant-relations.rkt")

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

(define (require-positive who n)
  (unless (positive? n)
    (error 'property-variants-random (format "~a must be >= 1, got ~a" who n))))

(define (require-nonnegative who n)
  (unless (>= n 0)
    (error 'property-variants-random (format "~a must be >= 0, got ~a" who n))))

(require-positive 'VR-ATTEMPTS VR-ATTEMPTS)
(require-positive 'VR-MAX-DEPTH VR-MAX-DEPTH)
(require-positive 'VR-U-POOL-SIZE VR-U-POOL-SIZE)
(require-positive 'VR-X-POOL-SIZE VR-X-POOL-SIZE)
(require-positive 'VR-R-POOL-SIZE VR-R-POOL-SIZE)
(require-positive 'VR-C-MAX VR-C-MAX)
(require-positive 'VR-EXPECTED-ANTE-HITS VR-EXPECTED-ANTE-HITS)
(require-positive 'VR-K-STEP-DEPTH VR-K-STEP-DEPTH)
(require-nonnegative 'VR-C-EXTRA-MAX VR-C-EXTRA-MAX)
(unless (<= VR-C-MAX VR-U-POOL-SIZE)
  (error 'property-variants-random
         (format "VR-C-MAX must be <= VR-U-POOL-SIZE, got ~a > ~a"
                 VR-C-MAX VR-U-POOL-SIZE)))
(unless (<= VR-EXPECTED-ANTE-HITS VR-ATTEMPTS)
  (error 'property-variants-random
         (format "VR-EXPECTED-ANTE-HITS must be <= VR-ATTEMPTS, got ~a > ~a"
                 VR-EXPECTED-ANTE-HITS VR-ATTEMPTS)))

(define U-POOL
  (for/list ([i (in-range VR-U-POOL-SIZE)])
    (string->symbol (format "u:~a" i))))

(define X-POOL
  (for/list ([i (in-range VR-X-POOL-SIZE)])
    (string->symbol (format "x:~a" i))))

(define R-POOL
  (for/list ([i (in-range VR-R-POOL-SIZE)])
    (string->symbol (format "r:~a" i))))

(struct gopts (calls? disj-goal? left-tree? exists?) #:transparent)

(define (vrandom rng n)
  (h:rng-random rng n))

(define (pick-one rng xs)
  (list-ref xs (vrandom rng (length xs))))

(define (make-label rng prefix)
  `(label ,(format "~a-~a" prefix (vrandom rng 1000000))))

(define (extend-c/rng rng c max-extra)
  (define unused
    (filter (lambda (u) (not (member u c))) U-POOL))
  (define room (- VR-C-MAX (length c)))
  (define extra-limit (min max-extra room (length unused)))
  (define extra-count (vrandom rng (add1 extra-limit)))
  (append c (h:random-distinct/rng rng unused extra-count)))

(define (gen-term/rng rng x-env c depth)
  (define options
    (append '(primitive)
            (if (null? c) '() '(logic-var))
            (if (null? x-env) '() '(lex-var))
            (if (zero? depth) '() '(pair))))
  (case (pick-one rng options)
    [(primitive) (h:gen-primitive/rng rng)]
    [(logic-var) (pick-one rng c)]
    [(lex-var) (pick-one rng x-env)]
    [(pair)
     `(,(gen-term/rng rng x-env c (sub1 depth))
       :
       ,(gen-term/rng rng x-env c (sub1 depth)))]))

(define (gen-eq-goal/rng rng x-env c depth)
  `(,(gen-term/rng rng x-env c depth)
    =?
    ,(gen-term/rng rng x-env c depth)
    ,(make-label rng "eq")))

(define (fresh-x-list/rng rng x-env)
  (define available (filter (lambda (x) (not (member x x-env))) X-POOL))
  (h:random-distinct/rng rng
                         available
                         (vrandom rng (add1 (min 2 (length available))))))

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
             ,(h:gen-primitive/rng rng)
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
     (define ra (pick-one rng rel-sig))
     (define r (car ra))
     (define arity (cdr ra))
     `(,r
       ,@(for/list ([_ (in-range arity)])
           (gen-term/rng rng x-env c (max 0 (sub1 depth))))
       ,(make-label rng "call"))]))

(define (gen-state/rng rng c)
  `(state () ,c () ,(make-label rng "st")))

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
  (for/list ([r (in-list (h:random-distinct/rng rng R-POOL count))])
    (cons r (vrandom rng 3))))

(define (gen-rel-def/rng rng rel-ar rel-sig opts)
  (define r (car rel-ar))
  (define arity (cdr rel-ar))
  (define d
    (take (h:random-distinct/rng rng X-POOL arity) arity))
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

(define (gen-answers/rng rng)
  (define count (vrandom rng 3))
  (for/list ([_ (in-range count)])
    (gen-state/rng rng (extend-c/rng rng '() VR-C-EXTRA-MAX))))

(define (gen-config-user/rng rng opts)
  (define-values (gamma rel-sig) (gen-rel-env/rng rng opts))
  (define ans* (gen-answers/rng rng))
  (define c0 (extend-c/rng rng '() VR-C-EXTRA-MAX))
  (define s (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  `(,gamma ,ans* ,s))

(define (gen-config-flip-admin/rng rng)
  (define opts (gopts #t #t #t #t))
  (define-values (gamma rel-sig) (gen-rel-env/rng rng opts))
  (define ans* (gen-answers/rng rng))
  (define c0 (extend-c/rng rng '() VR-C-EXTRA-MAX))
  (define s1 (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  (define s2 (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  `(,gamma ,ans* ((delay ,s1) <-+ ,s2)))

(define (gen-config-rail-admin/rng rng)
  (define opts (gopts #t #t #t #t))
  (define-values (gamma rel-sig) (gen-rel-env/rng rng opts))
  (define ans* (gen-answers/rng rng))
  (define c0 (extend-c/rng rng '() VR-C-EXTRA-MAX))
  (define s1 (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  (define s2 (gen-tree-user/rng rng c0 VR-MAX-DEPTH rel-sig opts))
  `(,gamma ,ans* ((delay ,s1) <-+ ,s2)))

(define (state-c-size st)
  (match st
    [`(state ,_ ,c ,_ ,_) (length c)]
    [_ 0]))

(define (goal-flags g)
  (match g
    [`(succeed ,_) (values #f #f #f #f)]
    [`(,_ =? ,_ ,_) (values #f #f #f #f)]
    [`(∃ ,_ ,g2 ,_)
     (define-values (hc hd he hj) (goal-flags g2))
     (values hc hd #t (or #t hj))]
    [`(,g1 ∧ ,g2 ,_)
     (define-values (hc1 hd1 he1 hj1) (goal-flags g1))
     (define-values (hc2 hd2 he2 hj2) (goal-flags g2))
     (values (or hc1 hc2) (or hd1 hd2) (or he1 he2) #t)]
    [`(,g1 ∨ ,g2 ,_)
     (define-values (hc1 hd1 he1 hj1) (goal-flags g1))
     (define-values (hc2 hd2 he2 hj2) (goal-flags g2))
     (values (or hc1 hc2) #t (or he1 he2) (or hj1 hj2))]
    [`(,r ,_ ... ,_)
     (if (and (symbol? r)
              (regexp-match? #rx"^r:" (symbol->string r)))
         (values #t #f #f #f)
         (values #f #f #f #f))]
    [_ (values #f #f #f #f)]))

(define (tree-flags s)
  (match s
    [`(empty-tree) (values #f #f #f #f #f #f #f 0)]
    [`(⊤ ,st)
     (values #f #f #f #f #f #f #f (state-c-size st))]
    [`(,g ,st)
     (define-values (hc hd he hj) (goal-flags g))
     (values hc hd he hj #f #f #f (state-c-size st))]
    [`(,s1 × ,g ,c)
     (define-values (hc1 hd1 he1 hj1 hl1 hdl1 hr1 cm1) (tree-flags s1))
     (define-values (hc2 hd2 he2 hj2) (goal-flags g))
     (define csz (length c))
     (values (or hc1 hc2)
             (or hd1 hd2)
             (or he1 he2)
             (or hj1 hj2)
             hl1
             hdl1
             hr1
             (max cm1 csz))]
    [`(delay ,s1)
     (define-values (hc hd he hj hl hdl hr cm) (tree-flags s1))
     (values hc hd he hj hl #t hr cm)]
    [`(proceed (,g ,_σ))
     (define-values (hc hd he hj) (goal-flags g))
     (values hc hd he hj #f #f #f 0)]
    [`(proceed ((,r ,_ ... ,_) ,_σ))
     (if (and (symbol? r)
              (regexp-match? #rx"^r:" (symbol->string r)))
         (values #t #f #f #f #f #f #f 0)
         (values #f #f #f #f #f #f #f 0))]
    [`(,s1 <-+ ,s2)
     (define-values (hc1 hd1 he1 hj1 hl1 hdl1 hr1 cm1) (tree-flags s1))
     (define-values (hc2 hd2 he2 hj2 hl2 hdl2 hr2 cm2) (tree-flags s2))
     (values (or hc1 hc2)
             (or hd1 hd2)
             (or he1 he2)
             (or hj1 hj2)
             #t
             (or hdl1 hdl2)
             (or hr1 hr2)
             (max cm1 cm2))]
    [`(,s1 +-> ,s2)
     (define-values (hc1 hd1 he1 hj1 hl1 hdl1 hr1 cm1) (tree-flags s1))
     (define-values (hc2 hd2 he2 hj2 hl2 hdl2 hr2 cm2) (tree-flags s2))
     (values (or hc1 hc2)
             (or hd1 hd2)
             (or he1 he2)
             (or hj1 hj2)
             (or hl1 hl2)
             (or hdl1 hdl2)
             #t
             (max cm1 cm2))]
    [_ (values #f #f #f #f #f #f #f 0)]))

(define (config-flags cfg)
  (match cfg
    [`(,gamma ,ans* ,s)
     (define call? #f)
     (define disj? #f)
     (define exists? #f)
     (define conj? #f)
     (define left-tree? #f)
     (define delay? #f)
     (define right-tree? #f)
     (define max-c 0)

     (for ([rel (in-list gamma)])
       (match rel
         [`(,_ ,_ ,g)
          (define-values (hc hd he hj) (goal-flags g))
          (when hc (set! call? #t))
          (when hd (set! disj? #t))
          (when he (set! exists? #t))
          (when hj (set! conj? #t))]
         [_ (void)]))

     (for ([st (in-list ans*)])
       (set! max-c (max max-c (state-c-size st))))

     (define-values (hc hd he hj hl hdelay hr cmax-tree) (tree-flags s))
     (values (or call? hc)
             (or disj? hd)
             (or exists? he)
             (or conj? hj)
             hl
             hdelay
             hr
             (max max-c cmax-tree))]
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

;; Check theorem-style consequents up to k steps:
;; if cfg is wf, then it stays in-language, remains wf, progresses unless final,
;; and (optionally) has unique decomposition at each explored node.
(define (k-step-consequent-failure rel shape-match? cfg k require-unique?)
  (define (loop cfg fuel)
    (cond
      [(not (shape-match? cfg))
       (list 'shape cfg)]
      [(not (states-wf? cfg))
       (list 'state-wf cfg)]
      [else
       (define next* (apply-reduction-relation rel cfg))
       (cond
         [(and (not (final-config? cfg)) (null? next*))
          (list 'progress cfg)]
         [(and require-unique?
               (if (final-config? cfg)
                   (not (null? next*))
                   (not (= (length next*) 1))))
          (list 'unique cfg (length next*))]
         [(zero? fuel) #f]
         [else
          (for/or ([cfg^ (in-list next*)])
            (loop cfg^ (sub1 fuel)))])]))
  (loop cfg k))

(define (run-random-variant label rel shape-match? shape-closed? cfg-generator
                            #:require-unique? [require-unique? #t]
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
    (define rng (h:make-seeded-rng seed))
    (define fail-count 0)
    (define fail-samples '())
    (define ante-hits 0)
    (define call-gen-hits 0)
    (define disj-gen-hits 0)
    (define left-tree-hits 0)
    (define delay-hits 0)
    (define right-tree-hits 0)
    (define right-next-hits 0)
    (define call-rule-hits 0)
    (define disj-rule-hits 0)
    (define flip-rule-hits 0)
    (define rail-rule-hits 0)
    (define max-c-seen 0)
    (define k-shape-fails 0)
    (define k-state-wf-fails 0)
    (define k-progress-fails 0)
    (define k-unique-fails 0)

    (for ([_ (in-range VR-ATTEMPTS)])
      (define cfg (cfg-generator rng))
      (unless (shape-match? cfg)
        (set! fail-count (add1 fail-count))
        (when (< (length fail-samples) 3)
          (set! fail-samples (cons (list 'shape cfg) fail-samples))))

      (define-values (has-call has-disj has-exists has-conj has-left has-delay has-right cmax)
        (config-flags cfg))
      (define tree (third cfg))
      (when has-call (set! call-gen-hits (add1 call-gen-hits)))
      (when has-disj (set! disj-gen-hits (add1 disj-gen-hits)))
      (when (or has-left (tree-contains-left? tree))
        (set! left-tree-hits (add1 left-tree-hits)))
      (when (or has-delay (tree-contains-delay? tree))
        (set! delay-hits (add1 delay-hits)))
      (when (or has-right (tree-contains-right? tree))
        (set! right-tree-hits (add1 right-tree-hits)))
      (set! max-c-seen (max max-c-seen cmax))
      (with-handlers
          ([exn:fail?
            (lambda (e)
              (set! fail-count (add1 fail-count))
              (when (< (length fail-samples) 3)
                (set! fail-samples
                      (cons (list 'exception (exn-message e) cfg) fail-samples)))
              #f)])
        (define steps-named (apply-reduction-relation/tag-with-names rel cfg))
        (for ([item (in-list steps-named)])
          (define name (first item))
          (when (name-has-prefix? name "call/")
            (set! call-rule-hits (add1 call-rule-hits)))
          (when (name-has-prefix? name "disj/")
            (set! disj-rule-hits (add1 disj-rule-hits)))
          (when (name-has-prefix? name "flip/")
            (set! flip-rule-hits (add1 flip-rule-hits)))
          (when (name-has-prefix? name "rail/")
            (set! rail-rule-hits (add1 rail-rule-hits)))
          (match item
            [(list _ cfg-next)
             (when (tree-contains-right? (third cfg-next))
               (set! right-next-hits (add1 right-next-hits)))]
            [_ (void)]))

        (when (and (shape-match? cfg) (states-wf? cfg))
          (set! ante-hits (add1 ante-hits))
          (define fail-info
            (k-step-consequent-failure rel
                                       shape-match?
                                       cfg
                                       k-depth
                                       require-unique?))
          (when fail-info
            (match fail-info
              [(list 'shape _)
               (set! k-shape-fails (add1 k-shape-fails))]
              [(list 'state-wf _)
               (set! k-state-wf-fails (add1 k-state-wf-fails))]
              [(list 'progress _)
               (set! k-progress-fails (add1 k-progress-fails))]
              [(list 'unique _ _)
               (set! k-unique-fails (add1 k-unique-fails))]
              [_ (void)])
            (set! fail-count (add1 fail-count))
            (when (< (length fail-samples) 3)
              (set! fail-samples
                    (cons (list 'k-step-consequent-fail fail-info cfg) fail-samples)))))

        (unless (shape-closed? rel cfg)
          (set! fail-count (add1 fail-count))
          (when (< (length fail-samples) 3)
            (set! fail-samples (cons (list 'shape-closed cfg) fail-samples))))))

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
    (check-equal? k-progress-fails
                  0
                  (format "~a seed=~a: k-step progress failures: ~a"
                          label seed k-progress-fails))
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
  (test-case "L1 Rcall-eager randomized"
    (define opts (gopts #t #f #f #t))
    (run-random-variant "Rcall-eager"
                        Rcall-eager
                        (lambda (cfg) (redex-match? L1 config cfg))
                        shape-closed/L1?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS))

  (test-case "L1 Rcall-lazy randomized"
    (define opts (gopts #t #f #f #t))
    (run-random-variant "Rcall-lazy"
                        Rcall-lazy
                        (lambda (cfg) (redex-match? L1 config cfg))
                        shape-closed/L1?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS))

  (test-case "L2 Rdisj-left randomized"
    (define opts (gopts #f #t #f #f))
    (run-random-variant "Rdisj-left"
                        Rdisj-left
                        (lambda (cfg) (redex-match? L2 config cfg))
                        shape-closed/L2?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-disj-rules VR-MIN-DISJ-RULE-HITS))

  (test-case "L3 Rbase-e randomized"
    (define opts (gopts #t #t #f #t))
    (run-random-variant "Rbase-e"
                        Rbase-e
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS
                        #:min-disj-rules VR-MIN-DISJ-RULE-HITS))

  (test-case "L3 Rbase-l randomized"
    (define opts (gopts #t #t #f #t))
    (run-random-variant "Rbase-l"
                        Rbase-l
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        (lambda (rng) (gen-config-user/rng rng opts))
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-call-gen VR-MIN-CALL-GEN-HITS
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-call-rules VR-MIN-CALL-RULE-HITS
                        #:min-disj-rules VR-MIN-DISJ-RULE-HITS))

  (test-case "L3 Rflip-e randomized admin-fragment"
    (run-random-variant "Rflip-e"
                        Rflip-e
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        gen-config-flip-admin/rng
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-left-tree VR-MIN-LEFT-TREE-HITS
                        #:min-delay VR-MIN-DELAY-HITS
                        #:min-flip-rules VR-MIN-FLIP-RULE-HITS))

  (test-case "L3 Rflip-l randomized admin-fragment"
    (run-random-variant "Rflip-l"
                        Rflip-l
                        (lambda (cfg) (redex-match? L3 config cfg))
                        shape-closed/L3?
                        gen-config-flip-admin/rng
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-left-tree VR-MIN-LEFT-TREE-HITS
                        #:min-delay VR-MIN-DELAY-HITS
                        #:min-flip-rules VR-MIN-FLIP-RULE-HITS))

  (test-case "L4 Rrail-e randomized admin-fragment"
    (run-random-variant "Rrail-e"
                        Rrail-e
                        (lambda (cfg) (redex-match? L4 config cfg))
                        shape-closed/L4?
                        gen-config-rail-admin/rng
                        #:expected-ante-hits VR-EXPECTED-ANTE-HITS
                        #:require-unique? #t
                        #:min-disj-gen VR-MIN-DISJ-GEN-HITS
                        #:min-left-tree VR-MIN-LEFT-TREE-HITS
                        #:min-delay VR-MIN-DELAY-HITS
                        #:min-right-tree 0
                        #:min-right-next VR-MIN-RIGHT-TREE-HITS
                        #:min-rail-rules VR-MIN-RAIL-RULE-HITS))

  (test-case "L4 Rrail-l randomized admin-fragment"
    (run-random-variant "Rrail-l"
                        Rrail-l
                        (lambda (cfg) (redex-match? L4 config cfg))
                        shape-closed/L4?
                        gen-config-rail-admin/rng
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
