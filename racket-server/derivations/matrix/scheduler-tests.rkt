#lang racket

(require rackunit redex/reduction-semantics
         "scheduler-source.rkt" "full-source.rkt"
         (prefix-in direct: "../s-reference/interpreter.rkt")
         "../test-support/witnesses.rkt")

(define (atom name) `((sym ,name) =? (sym ,name) (label ,name)))
(define (choice a b) `(,a ∨ ,b (label "choice")))
(define (delay a) `(suspend ,a (label "delay")))
(define (named relation current)
  (apply-reduction-relation/tag-with-names relation current))

;; Every Railroad edge must have exactly one matching reference Flip edge.
;; No normalizer, future-state search, or answer-only comparison is used.
(define (check-rail current [fuel 1000] [labels '()])
  (check-true (scheduler-in-domain? "rail" current))
  (check-true (scheduler-well-formed? current))
  (check-equal? (named strict-flip-red (erase-orientation current))
                (for/list ([edge (in-list (named strict-rail-red current))])
                  (match-define (list name next) edge)
                  (list (erase-rule name) (erase-orientation next))))
  (match (scheduler-status current)
    ['complete (values current (reverse labels))]
    [status
     (when (zero? fuel) (error 'check-rail "finite witness did not complete"))
     (match-define `(program ,definitions ,body) current)
     (match-define (list name next)
       (if (eq? status 'paused)
           (list "advance" `(program ,definitions (advance ,body)))
           (match (named strict-rail-red current)
             [(list edge) edge]
             [edges (error 'check-rail "nonunique or stuck transition: ~e" edges)])))
     (check-rail next (sub1 fuel) (cons name labels))]))

(define (round relation current [fuel 500])
  (match (named relation current)
    ['() current]
    [(list (list _ next))
     (when (zero? fuel) (error 'round "finite round did not stop"))
     (round relation next (sub1 fuel))]))

(module+ test
  (test-case "Flip is the existing strict matrix relation, not a copied policy"
    (check-eq? strict-flip-red strict-s-rel-red))

  (for ([example (in-list validation-witnesses)])
    (test-case (format "Railroad exact reference edges and scope: ~a" (witness-name example))
      (define goal (witness-goal example))
      (define owners (witness-owners example))
      (define state (witness-state example))
      (define-values (final _labels)
        (check-rail (s-rel-query-initial goal #:owners owners #:state state)))
      (check-equal? (erase-orientation final)
                    `(program () ,(direct:collect-all (direct:run goal #:owners owners #:state state))))))

  (test-case "nested oriented bind and full calls retain exact strict transitions"
    (define branch
      (choice (delay '(x:q =? (sym "A") (label "A")))
              (choice '(x:q =? (sym "B") (label "B"))
                      (delay '(x:q =? (sym "C") (label "C"))))))
    (define definitions
      `((r:choose (x:q) ,branch)))
    ;; Write the goal independently: right-oriented bind must evaluate its
    ;; answer continuation BEFORE its residual, although that head is right.
    (define goal
      '(∃ (x:q)
          ((r:choose x:q (label "call")) ∧ (succeed (label "continue")) (label "bind"))
          (label "fresh")))
    (define-values (_final labels) (check-rail (s-rel-query-initial goal #:relations definitions)))
    (check-not-false (member "bind-yield-right" labels)))

  (test-case "strict DFS matures a sibling before pausing but retains left priority"
    (define initial (s-rel-query-initial (choice (delay (atom "A")) (atom "B"))))
    (define paused (round strict-dfs-red initial))
    (check-match paused `(program () (More (Delay (Owners) (mplus (Owners) (force ,_) (One ,_ ,_))))))
    (match-define `(program ,definitions ,body) paused)
    (define completed (round strict-dfs-red `(program ,definitions (advance ,body))))
    (check-match completed `(program () (Forced ,_ (Emit ,_ (Answer ,_ (state ,_ ,_ (((sym "A") =? ,_ ,_)) ,_)) (Solo ,_ ,_))))))

  (test-case "oriented empty and nested eager chunks obey the same exact step squares"
    (define state '(state () () () (label "state")))
    (define one `(One (Owners) ,state))
    (define answer `(Answer (Owners) ,state))
    (define left `(Yield (Owners) ,answer ,one))
    (define right `(YieldR (Owners) ,one ,answer))
    (for ([term (list `(mplusR (Owners) ,one (Empty (Owners)))
                     `(mplusR (Owners) ,one ,left)
                     `(mplusR (Owners) ,one ,right)
                     `(mplus (Owners) ,right ,one)
                     `(mplus (Owners) (Empty (Owners)) ,right))])
      (define-values (final _labels) (check-rail `(program () (commit ,term))))
      (check-equal? (scheduler-status final) 'complete))))
