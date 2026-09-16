#lang racket

(require rackunit redex/reduction-semantics
         "shared/grammar-s.rkt" "shared/grammar-e.rkt" "shared/grammar-n.rkt"
         "shared/wf.rkt"
         (prefix-in q: "shared/maps.rkt")
         (only-in "s-reference/source.rkt" ScopeS)
         (prefix-in i: "s-reference/interpreter.rkt"))

(define state '(state () () () (label "initial")))
(define goal '(succeed (label "next")))
(define active `(Yield (Owners) (Answer (Owners) ,state) (Empty (Owners))))
(define pending `(More (Delay (Owners) (eval (Owners) ,goal ,state))))

(module+ test
  (test-case "reference and shared S grammars separate Yield Search from More Frontier"
    (for ([predicates
           (in-list
            (list (list (lambda (x) (redex-match? ScopeS SV x))
                        (lambda (x) (redex-match? ScopeS F x))
                        (lambda (x) (redex-match? ScopeS c x)))
                  (list (lambda (x) (redex-match? StrictS SV x))
                        (lambda (x) (redex-match? StrictS F x))
                        (lambda (x) (redex-match? StrictS c x)))))])
      (match-define (list search? frontier? computation?) predicates)
      (check-true (search? active))
      (check-false (frontier? active))
      (check-true (frontier? pending))
      (check-false (computation? pending))
      ;; Deliberately construct the obsolete active spelling as a negative
      ;; grammar witness. It is not an accepted alias or a runtime adapter.
      (check-false (computation? (cons 'More (cdr active))))))

  (test-case "ownerless E and N maps retain the same constructor distinction"
    (for ([row
           (in-list
            (list (list q:Q-SE
                        (lambda (x) (redex-match? StrictE SV x))
                        (lambda (x) (redex-match? StrictE F x))
                        (lambda (x) (redex-match? StrictE c x)))
                  (list q:Q-SN
                        (lambda (x) (redex-match? StrictN SV x))
                        (lambda (x) (redex-match? StrictN F x))
                        (lambda (x) (redex-match? StrictN c x)))))])
      (match-define (list project search? frontier? computation?) row)
      (define search (project active))
      (define frontier (project pending))
      (check-equal? (car search) 'Yield)
      (check-equal? (car frontier) 'More)
      (check-equal? (length frontier) 2)
      (check-true (search? search))
      (check-false (frontier? search))
      (check-true (frontier? frontier))
      (check-false (computation? frontier))
      (check-false (computation? (cons 'More (cdr search))))))

  (test-case "Yield contexts keep eager work and cannot descend beneath Frontier More"
    (define work `(eval (Owners) ,goal ,state))
    (define eager `(Yield (Owners) (Answer (Owners) ,state) ,work))
    (check-true (redex-match? ScopeS c eager))
    (check-false (redex-match? ScopeS SV eager))
    (check-true (redex-match? ScopeS E `(Yield (Owners) (Answer (Owners) ,state) ,(term hole))))
    (check-false (redex-match? ScopeS C `(More (Delay (Owners) ,(term hole))))))

  (test-case "the direct interpreter builds Yield before commitment"
    (define search (i:eval/s `(,goal ∨ ,goal (label "choice")) state '(Owners) '()))
    (check-match search `(Yield (Owners) (Answer (Owners) ,_) (One (Owners) ,_)))
    (check-true (redex-match? ScopeS SV search))
    (check-match (i:commit/s search) `(Emit (Owners) ,_ (Solo (Owners) ,_))))

  (test-case "commit retains unary More and public advance crosses its Delay"
    (define search (i:eval/s `(suspend ,goal (label "delay")) state '(Owners) '()))
    (define frontier (i:commit/s search))
    (check-equal? frontier `(More ,search))
    (check-match (i:resume-once frontier) `(Forced (Owners) (Solo (Owners) ,_))))

  (test-case "terminal success and failure retain Solo and Done"
    (define answer `(Answer (Owners) ,state))
    (check-equal? (i:commit/s '(Empty (Owners))) '(Done (Owners)))
    (check-equal? (i:commit/s `(One (Owners) ,state)) `(Solo (Owners) ,state))
    ;; An existing eager cell followed by failure has different structure
    ;; from terminal success; commitment does not normalize them together.
    (check-equal? (i:commit/s active) `(Emit (Owners) ,answer (Done (Owners)))))

  (test-case "terminal grammar rejects the former Last shell and Answer payload"
    (define owners '(Owners (Owner () (label "empty"))
                            (Owner (u:a) (label "unused"))))
    (define solo `(Solo ,owners ,state))
    (check-equal? (i:commit/s `(One ,owners ,state)) solo)
    (check-true (redex-match? ScopeS F solo))
    (check-true (redex-match? StrictS O solo))
    (check-true (wf-s? solo))
    (check-false (redex-match? ScopeS SV solo))
    (check-false (redex-match? ScopeS c solo))
    (for ([obsolete (in-list (list `(Last (Owners) (Answer ,owners ,state))
                                  `(Last ,owners (Answer (Owners) ,state))
                                  `(Solo ,owners (Answer (Owners) ,state))))])
      (check-false (redex-match? ScopeS F obsolete))
      (check-false (redex-match? StrictS F obsolete))
      (check-false (wf-s? obsolete)))
    (for ([row (in-list
                (list (list q:Q-SE wf-e? (lambda (x) (redex-match? StrictE F x)))
                      (list q:Q-SN wf-n? (lambda (x) (redex-match? StrictN F x)))))])
      (match-define (list project well-formed? frontier?) row)
      (define projected (project solo))
      (check-match projected `(Solo (state ,_ ,_ ,_ ,_ ,_)))
      (check-true (frontier? projected))
      (check-true (well-formed? projected))
      (check-false (frontier? (cons 'Last (cdr projected)))))))
