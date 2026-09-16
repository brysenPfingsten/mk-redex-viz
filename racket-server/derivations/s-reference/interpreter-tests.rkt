#lang racket

(require rackunit redex/reduction-semantics
         "source.rkt" "inspection.rkt"
         (prefix-in direct: "interpreter.rkt")
         (prefix-in cps: "cps.rkt")
         "../shared/kernel-equations.rkt"
         "../test-support/witnesses.rkt"
         "../test-support/corpus.rkt"
         "../test-support/frontiers.rkt"
         "../shared/wf.rkt"
         "../shared/maps.rkt")

(define initial-state '(state () () () (label "initial")))
(define yes '(succeed (label "yes")))
(define no '(fail (label "no")))
(define fresh '(∃ (x:y) (x:y =? (sym "new") (label "new")) (label "fresh-y")))

;; These force a locally owned Delay internally; a lone public Delay retains
;; its ownership on Forced and would not exercise the changed entry protocol.
(define (owned-resumption body [binders '(x:x)])
  `((∃ ,binders (suspend ,body (label "pause")) (label "fresh-x"))
    ∨ ,no (label "choice")))

(define extra-goals
  (append
   (for/list ([body (in-list
                    (list yes no `(,yes ∨ ,yes (label "more"))
                          `(,no ∨ ,yes (label "reuse-right"))
                          `(suspend ,yes (label "nested"))
                          `(x:x =? (sym "old") (label "old")) fresh
                          `(suspend (x:x =? (sym "old") (label "old"))
                                    (label "nested-old"))))])
     (owned-resumption body))
   (list (owned-resumption yes '())
         `(,(owned-resumption yes) ∧ ,fresh (label "fresh-after-return"))
         `(,(owned-resumption `(x:x =? (sym "old") (label "old")))
           ∧ ,fresh (label "delayed-bind")))))

(define cases
  (append validation-witnesses
          (for/list ([goal (in-list (remove-duplicates (append search-corpus extra-goals)))]
                     [index (in-naturals)])
            (witness (string->symbol (format "goal-~a" index)) goal '(Owners)
                     initial-state "strict and allocation regression"))))

(define atomic-focus
  (term-match/single ScopeS
    [(in-hole C (eval owners a σ)) (list (term a) (term σ))]))

(define (source-work initial steps)
  (for/list ([before (in-list (cons initial (map second steps)))]
             [step (in-list steps)]
             #:when (equal? (first step) "eval-atom"))
    (atomic-focus before)))

(define (observe-work compute)
  (define reversed '())
  (define value
    (parameterize ([current-atomic-observer
                    (lambda (goal state) (set! reversed (cons (list goal state) reversed)))])
      (compute)))
  (values value (reverse reversed)))

(define (check-boundary descriptions compute initial)
  (define steps (retained-trace initial))
  (define expected (if (null? steps) initial (second (last steps))))
  (define-values (frontier work) (observe-work compute))
  (define actual (reify-frontier descriptions frontier))
  (check-equal? actual expected "exact Frontier, including unexecuted Delay bodies")
  (check-equal? work (source-work initial steps) "actual atom work before this boundary")
  (check-true (wf-s? actual))
  (check-true (wf-e? (Q-SE actual)))
  (check-true (wf-n? (Q-SN actual)))
  (values frontier actual))

;; Inspect only the settled Frontier path; a Delay body is never invoked.
(define (frontier-shape frontier)
  (match frontier
    [`(More (Delay ,owners ,_)) `(More (Delay ,owners pending))]
    [`(Emit ,owners ,answer ,rest)
     `(Emit ,owners ,answer ,(frontier-shape rest))]
    [`(Forced ,owners ,rest) `(Forced ,owners ,(frontier-shape rest))]
    [terminal terminal]))

;; Retain complete Answers and the exact position of each Owner group.
(define (settled-prefix frontier)
  (match frontier
    [`(More (Delay ,_ ,_)) '()]
    [`(Emit ,owners ,answer ,rest)
     (cons `(Emit ,owners ,answer) (settled-prefix rest))]
    [`(Forced ,owners ,rest) (cons `(Forced ,owners) (settled-prefix rest))]
    [terminal (list terminal)]))

(define (check-advances descriptions advance frontier source [fuel 100])
  (cond
    [(retained-observation? source)
     (define-values (unchanged work) (observe-work (lambda () (advance frontier))))
     (check-equal? (reify-frontier descriptions unchanged) source)
     (check-equal? work '())]
    [else
     (when (zero? fuel) (error 'check-advances "unexpected unproductive fixture"))
     (define-values (next next-source)
       (check-boundary descriptions (lambda () (advance frontier)) `(advance ,source)))
     (check-equal? (reify-frontier descriptions frontier) source
                   "resumption leaves its input Frontier unchanged")
     (define before-prefix (settled-prefix source))
     (check-equal? (take (settled-prefix next-source) (length before-prefix)) before-prefix
                   "advancement preserves every settled Answer and Owner position")
     (check-advances descriptions advance next next-source (sub1 fuel))]))

(define (check-runner name run advance collect arity)
  (test-case (format "~a: literal S allocation and strictness witnesses" name)
    (check-exact-discriminants run collect))
  (test-case (format "~a: literal incremental boundaries" name)
    (check-literal-boundaries run advance))
  (for ([sample (in-list cases)])
    (test-case (format "~a: ~a" name (witness-name sample))
      (define descriptions (make-weak-hasheq))
      (parameterize
          ([direct:current-closure-observer
            (lambda (family procedure captures)
              (when (member family '(resume-eval resume-merge resume-bind))
                (check-true (procedure-arity-includes? procedure arity)))
              (record-closure! descriptions family procedure captures))])
        (define initial
          (retained-query-initial (witness-goal sample)
                                  #:owners (witness-owners sample)
                                  #:state (witness-state sample)))
        (define-values (frontier source)
          (check-boundary
           descriptions
           (lambda () (run (witness-goal sample) #:owners (witness-owners sample)
                          #:state (witness-state sample)))
           initial))
        (check-advances descriptions advance frontier source)
        (define-values (_all _all-source)
          (check-boundary descriptions (lambda () (collect frontier)) `(collect ,source)))
        (void)))))

(define (named-witness name)
  (or (findf (lambda (w) (eq? (witness-name w) name)) validation-witnesses)
      (error 'named-witness "missing fixture: ~a" name)))

(define (run-witness run collect-all name)
  (define w (named-witness name))
  (collect-all
   (run (witness-goal w) #:owners (witness-owners w) #:state (witness-state w))))

;; These literal expectations inspect the structural allocation information
;; that answer-only or S->N checks would erase. They do not invoke any other
;; evaluator to obtain expected provenance or names.
(define (ownership-shape observation)
  (match observation
    [`(Done ,owners) `(Done ,owners)]
    [`(Solo ,owners ,_) `(Solo ,owners)]
    [`(Emit ,owners (Answer ,local ,_) ,rest)
     `(Emit ,owners (Answer ,local) ,(ownership-shape rest))]
    [`(Forced ,owners ,rest) `(Forced ,owners ,(ownership-shape rest))]))

(define exact-ownership
  '((empty-binder
     (Solo (Owners (Owner () (label "empty-fresh")))))
    (unused-binder
     (Solo (Owners (Owner (u:0 u:1) (label "multi")))))
    (shadowed-binder
     (Solo (Owners (Owner (u:0) (label "outer-fresh"))
                   (Owner (u:1) (label "inner-fresh")))))
    (shared-outer
     (Emit (Owners (Owner (u:0) (label "shared"))) (Answer (Owners))
           (Solo (Owners))))
    (sibling-reuse
     (Emit (Owners) (Answer (Owners (Owner (u:0) (label "left-owner"))))
           (Solo (Owners (Owner (u:0) (label "right-owner"))))))
    (answer-local-continuation
     (Emit (Owners)
           (Answer (Owners (Owner (u:0 u:1) (label "left-two"))
                           (Owner (u:2) (label "later-owner"))))
           (Solo (Owners (Owner (u:0) (label "right-one"))
                         (Owner (u:1) (label "later-owner"))))))
    (allocated-failure (Done (Owners (Owner (u:0) (label "failed-owner")))))
    (failed-sibling
     (Solo (Owners (Owner (u:0) (label "surviving-owner")))))
    (allocation-across-delay
     (Forced (Owners (Owner (u:0) (label "outer-owner")))
             (Solo (Owners (Owner (u:1) (label "inner-owner"))))))
    (delayed-sibling-capture
     (Forced (Owners (Owner (u:0) (label "shared-owner")))
             (Emit (Owners) (Answer (Owners (Owner (u:1 u:2) (label "eager-owner"))))
                   (Solo (Owners (Owner (u:1) (label "delayed-owner")))))))
    (eager-bind-residual
     (Emit (Owners)
           (Answer (Owners (Owner (u:0) (label "fresh-A"))
                           (Owner (u:1) (label "later-fresh"))))
           (Solo (Owners (Owner (u:0) (label "fresh-B"))
                         (Owner (u:1) (label "later-fresh"))))))
    (nested-rail
     (Forced (Owners)
             (Forced (Owners)
                     (Forced (Owners)
                             (Forced (Owners)
                                     (Emit (Owners)
                                           (Answer (Owners (Owner (u:0) (label "fresh-A"))))
                                           (Emit (Owners)
                                                 (Answer (Owners (Owner (u:0) (label "fresh-B"))))
                                                 (Solo (Owners (Owner (u:0) (label "fresh-C")))))))))))
    (sparse-inherited-ancestry
     (Solo (Owners (Owner (u:9 u:2) (label "outer-pair"))
                   (Owner (u:7) (label "unused-ancestor"))
                   (Owner (u:0) (label "fresh")))))
    (inherited-state-and-trail
     (Solo (Owners (Owner (u:9 u:2) (label "sparse"))
                   (Owner (u:0) (label "new-owner")))))))

(define (answer-states observation)
  (match observation
    [`(Done ,_) '()]
    [`(Solo ,_ ,state) (list state)]
    [`(Emit ,_ (Answer ,_ ,state) ,rest) (cons state (answer-states rest))]
    [`(Forced ,_ ,rest) (answer-states rest)]))

(define (work-labels run name #:collect-all [collect-all #f])
  (define w (named-witness name))
  (define-values (_result work)
    (observe-work
     (lambda ()
       (define frontier
         (run (witness-goal w) #:owners (witness-owners w) #:state (witness-state w)))
       (if collect-all (collect-all frontier) frontier))))
  (map (lambda (event) (last (first event))) work))

(define (check-exact-discriminants run collect-all)
  (for ([entry (in-list exact-ownership)])
    (match-define (list name expected) entry)
    (check-equal? (ownership-shape (run-witness run collect-all name)) expected
                  (format "literal ownership: ~a" name)))
  (check-equal?
   (map second (answer-states (run-witness run collect-all 'answer-local-continuation)))
   '(((u:2 (nat 9)) (u:1 (sym "A")))
     ((u:1 (nat 9)) (u:0 (sym "B")))))
  (check-equal?
   (map second (answer-states (run-witness run collect-all 'delayed-sibling-capture)))
   '(((u:2 u:0)) ((u:1 u:0))))
  (check-equal?
   (map second (answer-states (run-witness run collect-all 'nested-rail)))
   '(((u:0 (sym "A"))) ((u:0 (sym "B"))) ((u:0 (sym "C")))))
  (check-equal?
   (answer-states (run-witness run collect-all 'inherited-state-and-trail))
   '((state ((u:0 (sym "A")) (u:2 (sym "A")) (u:9 u:2))
            ((u:0 (sym "other")) (u:9 (sym "avoid")))
            ((u:9 =? u:2 (label "alias")) (u:2 =? (sym "A") (label "value"))
             (u:0 =? u:9 (label "new-alias")))
            (label "replay"))))
  ;; These are eager-boundary checks: full observations alone would accept
  ;; an online implementation that performs continuation work after emit.
  (check-equal? (work-labels run 'eager-bind-residual)
                '((label "A") (label "B") (label "later") (label "later")))
  (check-equal? (work-labels run 'nested-rail) '())
  (check-equal? (work-labels run 'allocation-across-delay)
                '((label "outer-value")))
  (check-equal? (work-labels run 'allocation-across-delay #:collect-all collect-all)
                '((label "outer-value") (label "inner-alias")))
  (check-equal? (work-labels run 'delayed-sibling-capture)
                '((label "eager-alias")))
  (check-equal? (work-labels run 'delayed-sibling-capture #:collect-all collect-all)
                '((label "eager-alias") (label "delayed-alias"))))

(define (check-literal-boundaries run resume-once)
  ;; A suspended value is not itself evidence of a forcing event. In the
  ;; nested rail each call adds one visible Forced, even though executing a
  ;; raw resumption can force internally while restoring nested orientation.
  (define rail (run-witness run values 'nested-rail))
  (check-equal? (frontier-shape rail) '(More (Delay (Owners) pending)))
  (define rail1 (resume-once rail))
  (check-equal? (frontier-shape rail1) '(Forced (Owners) (More (Delay (Owners) pending))))
  (define rail2 (resume-once rail1))
  (check-equal? (frontier-shape rail2)
                '(Forced (Owners) (Forced (Owners) (More (Delay (Owners) pending)))))
  (define rail3 (resume-once rail2))
  (check-equal? (frontier-shape rail3)
                '(Forced (Owners) (Forced (Owners) (Forced (Owners) (More (Delay (Owners) pending))))))
  (define rail4 (resume-once rail3))
  (check-match rail4
               `(Forced (Owners) (Forced (Owners) (Forced (Owners) (Forced (Owners)
                 (Emit (Owners)
                       (Answer (Owners (Owner (u:0) (label "fresh-A")))
                               (state ((u:0 (sym "A"))) ()
                                      ((u:0 =? (sym "A") (label "A"))) (label "initial")))
                       (Emit (Owners)
                             (Answer (Owners (Owner (u:0) (label "fresh-B")))
                                     (state ((u:0 (sym "B"))) ()
                                            ((u:0 =? (sym "B") (label "B"))) (label "initial")))
                             (Solo (Owners (Owner (u:0) (label "fresh-C")))
                                   (state ((u:0 (sym "C"))) ()
                                          ((u:0 =? (sym "C") (label "C")))
                                          (label "initial"))))))))))
  (check-equal?
   (frontier-shape (run-witness run values 'allocation-across-delay))
   '(More (Delay (Owners (Owner (u:0) (label "outer-owner"))) pending)))
  (check-equal?
   (resume-once (run-witness run values 'allocation-across-delay))
   '(Forced
     (Owners (Owner (u:0) (label "outer-owner")))
     (Solo (Owners (Owner (u:1) (label "inner-owner")))
           (state ((u:1 (sym "A")) (u:0 (sym "A"))) ()
                  ((u:0 =? (sym "A") (label "outer-value"))
                   (u:1 =? u:0 (label "inner-alias")))
                  (label "initial")))))
  (check-equal?
   (frontier-shape (run-witness run values 'delayed-sibling-capture))
   '(More (Delay (Owners (Owner (u:0) (label "shared-owner"))) pending)))
  (check-equal?
   (run-witness run values 'empty-binder)
   '(Solo (Owners (Owner () (label "empty-fresh")))
         (state () () () (label "initial"))))
  (check-equal?
   (run-witness run values 'allocated-failure)
   '(Done (Owners (Owner (u:0) (label "failed-owner")))))
  (check-false (pending? (run-witness run values 'answer-local-continuation)))
  (check-false (pending? (run-witness run values 'eager-bind-residual)))
  (check-equal? (run-witness run values 'intermediate-success-then-failure)
                '(Done (Owners (Owner (u:0) (label "temporary-owner")))))
  (check-equal? (frontier-shape (run-witness run values 'delayed-continuation-schedule))
                '(More (Delay (Owners (Owner (u:0) (label "shared-input-owner"))) pending)))
  (define-values (_ready sibling-work)
    (observe-work (lambda () (run-witness run values 'strict-sibling-maturation))))
  (check-equal? (map (lambda (event) (last (first event))) sibling-work)
                '((label "left-ready") (label "right-start") (label "right-finished")))
  (define-values (_pending continuation-work)
    (observe-work (lambda () (run-witness run values 'delayed-continuation-schedule))))
  (check-equal? (map (lambda (event) (last (first event))) continuation-work)
                '((label "input-A") (label "input-B"))))

(module+ test
  (check-runner 'direct direct:run direct:resume-once direct:collect-all 2)
  (check-runner 'cps cps:run cps:resume-once cps:collect-all 3))
