#lang racket

(require rackunit
         "source-s.rkt" "source-e.rkt" "source-n.rkt"
         "../shared/maps.rkt" "../shared/wf.rkt"
         "../shared/stages/schema.rkt" "stages/instances.rkt"
         "../test-support/corpus.rkt" "../test-support/generated-goals.rkt")

;; Record each native execution's atomic goal and exact incoming state. The
;; source maps address only the recorded S/E observations; every row executes
;; independently in its own carrier. The separate s-reference-tests suite
;; connects these native machines with the selected functional machine.
(define (native-result/events stage configuration project-event [events '()] [fuel 10000])
  (match configuration
    [(DFinal value) (values value (reverse events))]
    [(D redex frames)
     (when (zero? fuel) (error 'native-result/events "test budget exhausted"))
     (match-define (list label next) (d-step stage configuration))
     (define next-events
       (match label
         ["eval-atom" (cons (project-event redex frames) events)]
         [_ events]))
     (native-result/events stage next project-event next-events (sub1 fuel))]))

(define (check-work computation)
  (define e-computation (Q-SE computation))
  (define n-computation (Q-SN computation))
  (check-true (wf-s? computation))
  (check-true (wf-e? e-computation))
  (check-true (wf-n? n-computation))
  (define-values (s-result s-events)
    (native-result/events S (decompose S computation)
                          (lambda (redex frames) (Q-SN redex (frame-support S frames)))))
  (define-values (e-result e-events)
    (native-result/events E (decompose E e-computation)
                          (lambda (redex _) (Q-EN redex))))
  (define-values (n-result n-events)
    (native-result/events N (decompose N n-computation)
                          (lambda (redex _) redex)))
  (check-equal? (Q-SE s-result) e-result)
  (check-equal? (Q-EN e-result) n-result)
  (check-equal? (Q-SN s-result) n-result)
  (check-equal? s-events e-events)
  (check-equal? s-events n-events)
  (values s-result n-events))

(module+ test
  ;; Preserve the former numeric/register test's whole corpus and both input
  ;; states as native S/E/N checks, with deliberately sparse ordered names.
  (for* ([goal (in-list (append search-corpus generated-goals))]
         [sparse? (in-list '(#f #t))])
    (test-case (format "native S/E/N atomic work and frontier, sparse=~a: ~s" sparse? goal)
      (define owners
        (if sparse? '(Owners (Owner (u:9 u:2 u:7) (label "initial-world"))) '(Owners)))
      (define state
        (if sparse?
            '(state ((u:7 u:9)) ((u:9 (sym "avoid")))
                    ((u:7 =? u:9 (label "seed-alias"))) (label "initial"))
            '(state () () () (label "initial"))))
      (define-values (result _events)
        (check-work `(render ,(s-initial goal #:owners owners #:state state))))
      (check-true (s-observation? result))))

  (test-case "strict operands precede eager bind, with branch-local kernel inputs"
    (define goal
      '(∃ (x:q)
          (((x:q =? (sym "A") (label "left")) ∨
            (x:q =? (sym "B") (label "right")) (label "choice"))
           ∧ (succeed (label "continue")) (label "bind")) (label "fresh")))
    (define-values (result events) (check-work `(commit ,(s-initial goal))))
    (define empty-state '(state 1 () () () (label "initial")))
    (define a-state
      '(state 1 ((0 (sym "A"))) () ((0 =? (sym "A") (label "left"))) (label "initial")))
    (define b-state
      '(state 1 ((0 (sym "B"))) () ((0 =? (sym "B") (label "right"))) (label "initial")))
    (check-equal? events
                  `((eval (0 =? (sym "A") (label "left")) ,empty-state)
                    (eval (0 =? (sym "B") (label "right")) ,empty-state)
                    (eval (succeed (label "continue")) ,a-state)
                    (eval (succeed (label "continue")) ,b-state)))
    (check-equal? (Q-SN result) `(Emit ,a-state (Solo ,b-state))))

  (test-case "strict right work occurs before commitment, delayed work waits for collection"
    (define state '(state 0 () () () (label "initial")))
    (define goal
      '((succeed (label "left")) ∨
        ((succeed (label "right-before-delay")) ∧
         (suspend (succeed (label "right-after-delay")) (label "delay"))
         (label "right-bind")) (label "choice")))
    (define-values (frontier initial-events) (check-work `(commit ,(s-initial goal))))
    (check-equal? initial-events
                  `((eval (succeed (label "left")) ,state)
                    (eval (succeed (label "right-before-delay")) ,state)))
    (check-equal? (Q-SN frontier)
                  `(Emit ,state
                         (More (Delay (eval (succeed (label "right-after-delay")) ,state)))))
    (define-values (complete later-events) (check-work `(collect ,frontier)))
    (check-equal? later-events `((eval (succeed (label "right-after-delay")) ,state)))
    (check-equal? (Q-SN complete) `(Emit ,state (Forced (Solo ,state)))))

  (test-case "delayed choice retains its pending orientation across separate advances"
    (define state '(state 0 () () () (label "initial")))
    (define goal
      '((suspend (succeed (label "left")) (label "left-delay")) ∨
        (suspend (succeed (label "right")) (label "right-delay")) (label "choice")))
    (define-values (frontier initial-events) (check-work `(commit ,(s-initial goal))))
    (check-equal? initial-events '())
    (define-values (first left-events) (check-work `(advance ,frontier)))
    (check-equal? left-events `((eval (succeed (label "left")) ,state)))
    (check-false (s-observation? first))
    (define-values (second right-events) (check-work `(advance ,first)))
    (check-equal? right-events `((eval (succeed (label "right")) ,state)))
    (check-equal? (Q-SN second) `(Forced (Forced (Emit ,state (Solo ,state))))))

  (test-case "failing eager continuation discards every active candidate before commitment"
    (define state '(state 1 () () () (label "initial")))
    (define goal
      '(∃ (x:unused)
          (((succeed (label "left")) ∨ (succeed (label "right")) (label "choice"))
           ∧ (fail (label "discard")) (label "bind")) (label "fresh")))
    (define-values (result events) (check-work `(commit ,(s-initial goal))))
    (check-equal? events
                  `((eval (succeed (label "left")) ,state)
                    (eval (succeed (label "right")) ,state)
                    (eval (fail (label "discard")) ,state)
                    (eval (fail (label "discard")) ,state)))
    (check-equal? (Q-SN result) '(Done 1))))
