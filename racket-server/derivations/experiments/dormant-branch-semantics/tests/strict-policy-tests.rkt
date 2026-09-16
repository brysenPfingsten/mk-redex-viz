#lang racket

(require rackunit redex/reduction-semantics
         (prefix-in direct: "../../../s-reference/interpreter.rkt")
         (only-in "../../../shared/kernel-equations.rkt" current-atomic-observer)
         (only-in "../../../shared/kernel.rkt" owners-append)
         (prefix-in online: "../source/reduction-relations/rail-red.rkt"))

;; A single discriminating witness for the live application boundary. This
;; is not a general fusion theorem or another interpreter implementation.
(define goal
  '((succeed (label "A"))
    ∨ (((nat 0) =? (nat 0) (label "p"))
       ∧ (((nat 1) =? (nat 1) (label "q"))
          ∧ (suspend (succeed (label "h")) (label "delay"))
          (label "rest"))
       (label "right"))
    (label "choice")))
(define state '(state () () () (label "initial")))

(define (online-trace configuration [remaining 64] [reversed '()])
  (match (apply-reduction-relation/tag-with-names online:rail-red configuration)
    ['() (values (reverse reversed) configuration)]
    [(list (list label next))
     (when (zero? remaining) (error 'online-trace "witness exhausted its budget"))
     (online-trace next (sub1 remaining) (cons label reversed))]
    [other (error 'online-trace "nonunique successor: ~e" other)]))

;; Read both native terminal shapes into test-only observation data. Keep all
;; other Frontier fields literally; neither source runs the other's terminal.
(define (completed-observation frontier)
  (match frontier
    [`(Done ,owners) `(Done ,owners)]
    [`(Last ,owners (Answer ,private ,state))
     `(terminal-answer ,(owners-append owners private) ,state)]
    [`(Solo ,owners ,state) `(terminal-answer ,owners ,state)]
    [`(Emit ,owners ,answer ,rest)
     `(Emit ,owners ,answer ,(completed-observation rest))]
    [`(Forced ,owners ,rest) `(Forced ,owners ,(completed-observation rest))]))

(module+ test
  (test-case "equal completed observations do not identify strict and online work order"
    (define work '())
    (define final
      (parameterize ([current-atomic-observer
                      (lambda (atomic incoming)
                        (set! work (append work (list (second (last atomic))))))])
        (define search (direct:eval/s goal state '(Owners) '()))
        (check-equal? work '("A" "p" "q"))
        (check-match search `(Yield (Owners) ,_ (Delay (Owners) ,_)))
        (define frontier (direct:commit/s search))
        (check-equal? work '("A" "p" "q"))
        (check-match frontier `(Emit (Owners) ,_ (More (Delay (Owners) ,_))))
        (define completed (direct:collect-all frontier))
        (check-equal? work '("A" "p" "q" "h"))
        completed))
    (define-values (labels online-final)
      (online-trace `(More (Work (Owners) ,goal ,state))))
    (check-true (< (index-of labels "commit-choice-answer")
                   (index-of labels "unify-success")))
    (check-match final `(Emit (Owners) ,_ (Forced (Owners) (Solo (Owners) ,_))))
    (check-match online-final
                 `(Emit (Owners) ,_ (Forced (Owners) (Last (Owners) (Answer (Owners) ,_)))))
    (check-equal? (completed-observation online-final) (completed-observation final))))
