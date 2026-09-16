#lang racket

(require rackunit redex/reduction-semantics
         "data.rkt" "relations.rkt" "source.rkt" "stages.rkt"
         "readback.rkt" "machine-correspondence.rkt" "administration.rkt"
         "compression-correspondence.rkt"
         (prefix-in direct: "interpreter.rkt")
         (prefix-in cps: "cps.rkt")
         (prefix-in defunc: "defunc.rkt")
         (prefix-in inspection: "inspection.rkt")
         (prefix-in f: "machine.rkt")
         (prefix-in r: "registers.rkt")
         (prefix-in c: "compressed.rkt")
         (prefix-in derive: "derive.rkt")
         (prefix-in registers: "register-derive.rkt")
         (prefix-in compression: "compression-derive.rkt")
         (prefix-in matrix: "../matrix/full-source.rkt")
         (prefix-in a: "../shared/stages/schema.rkt")
         (prefix-in maps: "../shared/stages/maps.rkt")
         (prefix-in q: "../shared/maps.rkt")
         "../shared/wf.rkt" "../shared/kernel-equations.rkt" "../shared/runtime.rkt")

(define definitions
  '((r:id (x:q) (x:q =? (sym "A") (label "id-body")))
    (r:other (x:q) (x:q =? (sym "B") (label "other-body")))
    (r:choice (x:q)
              ((r:id x:q (label "id-call")) ∨
               (r:other x:q (label "other-call")) (label "choice")))
    (r:delayed (x:q)
               (∃ (x:unused)
                    (suspend (r:id x:q (label "resumed-call")) (label "pause"))
                    (label "retained-unused")))
    (r:shadow (x:q)
              (∃ (x:q) (x:q =? (sym "private") (label "shadowed")) (label "shadow")))
    (r:even (x:xs)
            ((x:xs =? empty (label "even-empty")) ∨
             (∃ (x:h x:t)
                  ((x:xs =? (x:h : x:t) (label "even-pair")) ∧
                   (r:odd x:t (label "to-odd")) (label "even-body"))
                  (label "even-fresh")) (label "even-choice")))
    (r:odd (x:xs)
           (∃ (x:h x:t)
                ((x:xs =? (x:h : x:t) (label "odd-pair")) ∧
                 (r:even x:t (label "to-even")) (label "odd-body"))
                (label "odd-fresh")))))

(define goals
  (list
   '(∃ (x:q) (r:id x:q (label "call")) (label "query"))
   '(∃ (x:q) (r:choice x:q (label "call")) (label "query"))
   '(∃ (x:q) ((r:delayed x:q (label "delay-call")) ∨ (fail (label "no"))
             (label "internal-force")) (label "query"))
   ;; The relation body allocates x:unused only after this public Delay is
   ;; crossed, retaining the query and sparse ancestor introductions.
   '(∃ (x:q)
        (suspend (r:delayed x:q (label "fresh-after-resume")) (label "before-fresh"))
        (label "query"))
   '(∃ (x:q) ((r:choice x:q (label "choice-call")) ∧
              (r:delayed x:q (label "continuation-call")) (label "eager-bind"))
        (label "query"))
   '(∃ (x:q) ((r:shadow x:q (label "shadow-call")) ∧
              (r:id x:q (label "outer-unchanged")) (label "check-shadow")) (label "query"))
   '(r:even ((sym "a") : ((sym "b") : empty)) (label "mutual-even"))
   '(r:odd ((sym "a") : empty) (label "mutual-odd"))
   '(∃ (x:q)
        ((suspend (suspend (r:id x:q (label "A")) (label "inner-A")) (label "outer-A")) ∨
         ((suspend (r:other x:q (label "B")) (label "delay-B")) ∨
          (suspend (r:id x:q (label "C")) (label "delay-C")) (label "inner-choice"))
         (label "outer-choice")) (label "query"))))

(define (whole current)
  (match current
    [(f:Call pc operands) (readback-call pc operands)]
    [(f:Halted value) (readback-halted value)]))

(define (data-only? value)
  (match value
    [(? procedure?) #f]
    [(cons first rest) (and (data-only? first) (data-only? rest))]
    [(? struct?)
     (for/and ([field (in-vector (struct->vector value) 1)]) (data-only? field))]
    [_ #t]))

(define (normalize-admin current [fuel 10000])
  (if (a:m-admin? RetainedSRel current)
      (begin
        (when (zero? fuel) (error 'normalize-admin "administrative fuel exhausted"))
        (match (a:m-step RetainedSRel current)
          [(list "admin" next)
           (check-equal? (a:readback-M current) (a:readback-M next))
           (normalize-admin next (sub1 fuel))]))
      current))

(define (check-configuration current)
  (check-true (data-only? current))
  (define source (whole current))
  (define native (functional->M current))
  (check-equal? (a:readback-M native) source)
  (check-true (wf-s-rel? source))
  (define native-e (maps:M-SE native))
  (define native-n (maps:M-SN native))
  (check-equal? (maps:M-EN native-e) native-n)
  (check-equal? (a:readback-M native-e) (q:Q-SE source))
  (check-equal? (a:readback-M native-n) (q:Q-SN source))
  (check-true (wf-e-rel? (a:readback-M native-e)))
  (check-true (wf-n-rel? (a:readback-M native-n)))
  native)

(define (check-path current [fuel 20000])
  (define native (normalize-admin (check-configuration current)))
  (match current
    [(f:Halted value)
     (check-true (a:m-final? RetainedSRel native))
     value]
    [_
     (when (zero? fuel) (error 'check-path "functional relation fuel exhausted"))
     (define label (functional-step-label current))
     (define next (f:step current))
     (define before (whole current))
     (define after (whole next))
     (define target (normalize-admin (functional->M next)))
     (cond
       [label
        (check-equal?
         (apply-reduction-relation/tag-with-names retained-rel-red before)
         (list (list label after)))
        (check-equal?
         (apply-reduction-relation/tag-with-names matrix:strict-s-rel-red before)
         (list (list label after)))
        (check-equal?
         (apply-reduction-relation/tag-with-names matrix:strict-e-rel-red (q:Q-SE before))
         (list (list label (q:Q-SE after))))
        (check-equal?
         (apply-reduction-relation/tag-with-names matrix:strict-n-rel-red (q:Q-SN before))
         (list (list label (q:Q-SN after))))
        (match-define (list actual native-next) (a:m-step RetainedSRel native))
        (check-equal? label actual)
        (check-equal? (normalize-admin native-next) target)]
       [else
        (check-equal? before after)
        (check-equal? native target)
        (check-true (< (functional-admin-rank next) (functional-admin-rank current)))])
     (check-path next (sub1 fuel))]))

(define atomic-focus
  (term-match/single ScopeSRel
    [(program Γ (in-hole C (eval owners a σ))) (list (term a) (term σ))]))

(define (source-boundary initial [fuel 20000] [work '()])
  (match (apply-reduction-relation/tag-with-names retained-rel-red initial)
    ['() (values initial (reverse work))]
    [(list (list label next))
     (when (zero? fuel) (error 'source-boundary "source fuel exhausted"))
     (source-boundary next (sub1 fuel)
                      (if (equal? label "eval-atom") (cons (atomic-focus initial) work) work))]
    [other (error 'source-boundary "nonunique relation source: ~e" other)]))

(define (functional-observe descriptions thunk)
  (define work '())
  (define frontier
    (parameterize ([direct:current-closure-observer
                    (lambda (family procedure captures)
                      (inspection:record-closure! descriptions family procedure captures))]
                   [current-atomic-observer
                    (lambda (goal state) (set! work (cons (list goal state) work)))])
      (thunk)))
  (values frontier (reverse work)))

(define (program-operation pc frontier)
  (match-define `(program ,environment ,body) frontier)
  (f:Call pc (list body '() (KProgram environment (KDone)))))

(define (check-register-path bank decode step! [compressed? #f] [fuel 20000])
  (define original (decode bank))
  (check-configuration original)
  (cond
    [(f:Halted? original) (f:Halted-value original)]
    [else
     (when (zero? fuel) (error 'check-register-path "register fuel exhausted"))
     (define expected
       (if compressed?
           (for/fold ([current original]) ([edge (in-list (compressed-step-span bank))])
             (check-not-false (member (f:Call-pc current) (OriginalStep-pcs edge)))
             (check-equal? (functional-step-label current) (OriginalStep-label edge))
             (f:step current))
           (f:step original)))
     (step! bank)
     (check-equal? (decode bank) expected)
     (check-register-path bank decode step! compressed? (sub1 fuel))]))

(module+ test
  (check-true (derive:check-generated!))
  (check-true (registers:check-generated!))
  (check-true (compression:check-generated!))

  (for ([goal (in-list goals)] [index (in-naturals)])
    (test-case (format "full functional/source/native/register correspondence ~a" index)
      (define owners '(Owners (Owner (u:2) (label "ancestry"))
                              (Owner () (label "empty-owner"))
                              (Owner (u:0 u:8) (label "shared"))))
      (define state '(state ((u:8 u:2)) ((u:2 (sym "avoid")))
                           ((u:8 =? u:2 (label "seed-alias"))) (label "initial")))
      (define initial (f:initial goal #:relations definitions #:owners owners #:state state))
      (define source `(program ,definitions (commit (eval ,owners ,goal ,state))))
      (check-equal? (whole initial) source)
      (define frontier (check-path initial))
      (define expected (reify-frontier frontier))
      (define-values (source-frontier expected-work) (source-boundary source))
      (check-equal? expected source-frontier)
      (for ([run (in-list (list direct:run cps:run))]
            [advance (in-list (list direct:resume-once cps:resume-once))]
            [collect (in-list (list direct:collect-all cps:collect-all))])
        (define descriptions (make-hasheq))
        (define-values (actual work)
          (functional-observe descriptions
                              (lambda () (run goal #:relations definitions #:owners owners #:state state))))
        (check-equal? work expected-work)
        (check-equal? (inspection:reify-frontier descriptions actual) expected)
        (match-define `(program ,environment ,body) expected)
        (define-values (next-source next-work) (source-boundary `(program ,environment (advance ,body))))
        (define-values (next actual-work) (functional-observe descriptions (lambda () (advance actual))))
        (check-equal? actual-work next-work)
        (check-equal? (inspection:reify-frontier descriptions next) next-source)
        (define-values (all-source all-work) (source-boundary `(program ,environment (collect ,body))))
        (define-values (all all-actual-work) (functional-observe descriptions (lambda () (collect actual))))
        (check-equal? all-actual-work all-work)
        (check-equal? (inspection:reify-frontier descriptions all) all-source))
      (check-equal? (defunc:run goal #:relations definitions #:owners owners #:state state) frontier)
      (check-equal? (check-register-path (r:from-machine initial) r:decode r:step!) frontier)
      (check-equal? (check-register-path (c:from-machine initial) c:decode c:step! #t) frontier)
      (for ([pc (in-list '(advance/d collect/d))])
        (define operation (program-operation pc frontier))
        (define result (check-path operation))
        (check-equal? (check-register-path (r:from-machine operation) r:decode r:step!) result)
        (check-equal? (check-register-path (c:from-machine operation) c:decode c:step! #t) result))))

  (test-case "recursive calls advance productively only at explicit suspension"
    (define environment
      '((r:repeat (x:q)
                   ((x:q =? (sym "answer") (label "answer")) ∨
                    (suspend (r:repeat x:q (label "again")) (label "explicit-delay"))
                    (label "choice")))))
    (define query '(∃ (x:q) (r:repeat x:q (label "initial-call")) (label "query")))
    (define frontier (check-path (f:initial query #:relations environment)))
    (void
     (for/fold ([current frontier]) ([round (in-range 3)])
       (check-path (program-operation 'advance/d current)))))

  (test-case "unguarded call retains strict right operand and never commits"
    (define environment '((r:loop () (r:loop (label "again")))))
    (define query '((succeed (label "known-left")) ∨ (r:loop (label "right")) (label "choice")))
    (define initial (f:initial query #:relations environment))
    (for/fold ([current initial]) ([index (in-range 64)])
      (check-configuration current)
      (check-false (regexp-match? #rx"^(commit|advance|collect)" (or (functional-step-label current) "admin")))
      (f:step current))
    (check-exn exn:fail:budget? (lambda () (f:drive initial #:fuel 64)))
    (check-exn exn:fail:budget? (lambda () (r:drive! (r:from-machine initial) #:fuel 64)))
    (check-exn exn:fail:budget? (lambda () (c:drive! (c:from-machine initial) #:fuel 64))))

  (test-case "environment boundary and pending captures cannot disagree"
    (define goal '(r:id (sym "A") (label "call")))
    (define state '(state () () () (label "initial")))
    (define wrong (f:Call 'eval/d (list (ProgramGoal definitions goal) state '(Owners) '()
                                        (KCommit (KProgram '() (KDone))))))
    (check-exn #rx"environment" (lambda () (functional->M wrong)))
    (check-exn #rx"environment" (lambda () (whole wrong)))
    (define lost (f:Call 'eval/d (list goal state '(Owners) '() (KCommit (KProgram definitions (KDone))))))
    (check-exn #rx"environment" (lambda () (whole lost)))
    (define changed-resumption
      `(program ,definitions (More (Delay (Owners) ,(REval (ProgramGoal '() goal) state)))))
    (check-exn #rx"environment" (lambda () (readback-halted changed-resumption)))
    (check-equal? (f:run '(succeed (label "empty-program")) #:relations '())
                  '(program () (Solo (Owners) (state () () () (label "initial")))))))
