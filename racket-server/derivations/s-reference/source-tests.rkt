#lang racket

(require rackunit redex/reduction-semantics
         "source.rkt" "stages.rkt"
         "../shared/stages/schema.rkt"
         (only-in "../test-support/stage-checks.rkt" check-row)
         (only-in "../shared/kernel.rkt" owners-append)
         (only-in "../shared/wf.rkt" wf-s?)
         (prefix-in q: "../shared/maps.rkt")
         (prefix-in corpus: "../test-support/corpus.rkt")
         (only-in "../test-support/witnesses.rkt"
                  validation-witnesses witness-initial)
         (only-in "../test-support/frontiers.rkt" pending?))

(define (check-no-prefix-frames computation)
  (define initial (initial-Z RetainedS computation))
  (for ([configuration
         (in-list (cons initial (map second (z-trace RetainedS initial))))])
    (match-define (Z _ frames) configuration)
    (for ([frame (in-list frames)])
      (check-not-equal? (Frame-kind frame) 'prefix))
    (check-true (wf-s? (readback-Z configuration)))
    (check-equal? (frame-support RetainedS frames)
                  (retained-context-support (plug-frames (term hole) frames)))))

(define (check-boundaries frontier completed [fuel 100])
  (when (zero? fuel) (error 'check-boundaries "public advancement exhausted fuel"))
  (check-true (retained-frontier? frontier))
  (check-true (wf-s? frontier))
  (check-equal? (retained-run `(collect ,frontier)) completed)
  (define next (retained-run `(advance ,frontier)))
  (if (pending? frontier)
      (check-boundaries next completed (sub1 fuel))
      (begin
        (check-equal? next frontier)
        (check-equal? frontier completed))))

(define state '(state () () () (label "initial")))
(define success '(succeed (label "success")))
(define failure '(fail (label "failure")))
(define fresh
  '(∃ (x:new) (x:new =? (sym "new") (label "new-value")) (label "fresh")))

;; Each wrapper reaches internal forcing through actual strict merge. Empty,
;; One, Yield, Delay, saved-right reuse and an old variable are all represented.
(define (delayed-owner body [binders '(x:old)])
  `((∃ ,binders (suspend ,body (label "suspend")) (label "owner"))
    ∨ ,failure (label "choice")))

(define allocation-goals
  (append
   (for/list ([body (in-list (list failure success
                                   `(,success ∨ ,success (label "two"))
                                   `(suspend ,success (label "nested"))
                                   `(,failure ∨ ,success (label "saved-right"))
                                   '(x:old =? (sym "old") (label "old-value"))
                                   fresh))])
     (delayed-owner body))
   (list (delayed-owner success '())
         `(,(delayed-owner success) ∧ ,fresh (label "fresh-after"))
         `(,(delayed-owner '(x:old =? (sym "old") (label "old-value")))
           ∧ ,fresh (label "delayed-bind"))
         (delayed-owner '(suspend (x:old =? (sym "old") (label "old-value"))
                                 (label "nested-old"))))))

;; Mature a Search while genuinely retaining its ancestor context. Stop before
;; the observer processes the value; this extracts no scope by syntactic guess.
(define (mature-under computation owners [fuel 10000])
  (mature-context `(Forced ,owners (commit ,computation)) fuel))

(define (mature-context computation fuel)
  (match computation
    [`(Forced ,_ (commit ,(? retained-value? value))) value]
    [_
     (when (zero? fuel) (error 'mature-context "eager chunk exhausted fuel"))
     (check-true (wf-s? computation))
     (match (apply-reduction-relation retained-red computation)
       [(list next) (mature-context next (sub1 fuel))]
       [other (error 'mature-context "stuck or nonunique: ~e" other)])]))

(define atomic-focus
  (term-match/single ScopeS [(in-hole C (eval owners a σ)) (term a)]))

(define (atomic-work computation)
  (define edges (retained-trace computation))
  (for/list ([before (in-list (cons computation (map second edges)))]
             [edge (in-list edges)]
             #:when (equal? (first edge) "eval-atom"))
    (atomic-focus before)))

(module+ test
  (test-case "terminal commitment keeps introductions directly on Solo and separates it from One"
    (define owners '(Owners (Owner () (label "empty-introduction"))
                            (Owner (u:9 u:2) (label "unused-introductions"))))
    (define candidate `(One ,owners ,state))
    (define frontier `(Solo ,owners ,state))
    (check-true (retained-value? candidate))
    (check-false (retained-frontier? candidate))
    (check-false (retained-value? frontier))
    (check-true (retained-frontier? frontier))
    (check-true (wf-s? frontier))
    (check-equal? (retained-contract `(commit ,candidate)) (list "commit-one" frontier))
    (check-equal? (retained-contract `(render ,candidate)) (list "render-one" frontier))
    (check-equal? (retained-contract `(advance ,frontier)) (list "advance-solo" frontier))
    (check-equal? (retained-contract `(collect ,frontier)) (list "collect-solo" frontier)))

  (define ordinary-inputs
    (remove-duplicates
     (append (map witness-initial validation-witnesses)
             (for/list ([goal (in-list (append corpus:search-corpus allocation-goals))])
               (retained-initial goal)))))

  ;; Check each native derivation edge and the public completion boundary.
  (for ([evaluation (in-list ordinary-inputs)] [index (in-naturals)])
    (test-case (format "retained R-D-Z-M-B native stage squares ~a" index)
      (define query `(commit ,evaluation))
      (define frontier (retained-run query))
      (check-boundaries frontier (retained-run `(render ,evaluation)))
      (for ([operation (in-list (list query `(advance ,frontier) `(collect ,frontier)))])
        (check-row RetainedS retained-red operation)
        (check-no-prefix-frames operation))))

  (test-case "internal force transfers saved scope before resumption work"
    (define saved '(Owners (Owner (u:2 u:0) (label "saved"))
                           (Owner () (label "empty-saved"))))
    (define body `(eval (Owners) ,fresh ,state))
    (define input `(Forced (Owners (Owner (u:9) (label "ancestor")))
                           (commit (force (Delay ,saved ,body)))))
    (check-equal?
     (map first (retained-trace input))
     '("force-delay" "allocate-fresh" "eval-atom" "commit-one"))
    (match-define (list "force-delay" after-force) (first (retained-trace input)))
    (check-equal? after-force
                  `(Forced (Owners (Owner (u:9) (label "ancestor")))
                           (commit (eval ,saved ,fresh ,state))))
    (check-equal?
     (retained-run input)
     '(Forced (Owners (Owner (u:9) (label "ancestor")))
              (Solo (Owners (Owner (u:2 u:0) (label "saved"))
                            (Owner () (label "empty-saved"))
                            (Owner (u:1) (label "fresh")))
                    (state ((u:1 (sym "new"))) ()
                           ((u:1 =? (sym "new") (label "new-value")))
                           (label "initial")))))
    (check-no-prefix-frames input))

  (test-case "root scope transport preserves support, allocation and every Search constructor"
    (define ancestor '(Owners (Owner (u:9) (label "ancestor"))))
    (define saved '(Owners (Owner (u:2 u:0) (label "saved"))
                           (Owner () (label "empty-saved"))))
    (define local '(Owners (Owner () (label "empty-local"))))
    (define goal-work `(eval ,local ,fresh ,state))
    (define answer `(Answer (Owners (Owner (u:1) (label "head-only"))) ,state))
    (define computations
      (list `(Empty ,local) `(One ,local ,state)
            `(Yield ,local ,answer ,goal-work)
            `(Delay ,local ,goal-work)
            goal-work
            `(mplus ,local (Empty (Owners)) ,goal-work)
            `(mplus ,local (One (Owners) ,state) ,goal-work)
            `(bind ,local (One (Owners) ,state) ,fresh)
            `(bind ,local (Yield (Owners) (Answer (Owners) ,state)
                               (One (Owners) ,state)) ,fresh)
            `(force (Delay ,local ,goal-work))))
    (for* ([prefix (in-list (list '(Owners)
                                 '(Owners (Owner () (label "only-empty"))) saved))]
           [computation (in-list computations)])
      (define inherited (owners-append ancestor prefix))
      (define raw (mature-under computation inherited))
      (define lifted (lift-owners prefix computation))
      (define result (mature-under lifted ancestor))
      (match-define `(,constructor ,result-owners ,rest ...) result)
      (match-define `(,raw-constructor ,raw-owners ,raw-rest ...) raw)
      (check-equal? constructor raw-constructor)
      (check-equal? result-owners (owners-append prefix raw-owners))
      (check-equal? rest raw-rest)
      (define prefix-names
        (match prefix [`(Owners ,groups ...) (append-map second groups)]))
      (check-equal? (q:Q-SE result '(u:9))
                    (q:Q-SE raw (append '(u:9) prefix-names)))
      (check-equal? (q:Q-SN result '(u:9))
                    (q:Q-SN raw (append '(u:9) prefix-names))))
    ;; The head's private u:1 does not occupy the residual's allocation world.
    (match-define `(Yield ,_ ,_ (One ,tail-owners ,_))
      (mature-under (lift-owners saved `(Yield ,local ,answer ,goal-work)) ancestor))
    (check-equal? tail-owners
                  '(Owners (Owner () (label "empty-local"))
                           (Owner (u:1) (label "fresh")))))

  (test-case "strict disjunction, eager bind and commitment keep their original boundaries"
    (define left '(succeed (label "left")))
    (define right '(succeed (label "right")))
    (define continuation '(succeed (label "continue")))
    (define query
      (retained-query-initial `((,left ∨ ,right (label "choice"))
                                ∧ ,continuation (label "bind"))))
    (check-equal? (atomic-work query) (list left right continuation continuation))
    (define delayed (retained-query-initial `(suspend ,left (label "delay"))))
    (check-equal? (atomic-work delayed) '())
    (define frontier (retained-run delayed))
    (check-true (pending? frontier))
    (check-equal? (atomic-work `(advance ,frontier)) (list left))
    (check-equal? (atomic-work `(commit (Yield (Owners) (Answer (Owners) ,state)
                                            (Delay (Owners) (eval (Owners) ,right ,state)))))
                  '())
    (check-equal?
     (retained-run
      (retained-query-initial `((,left ∨ ,right (label "choice"))
                                ∧ ,failure (label "unsettled"))))
     '(Done (Owners)))))
