#lang racket

(require rackunit
         rackunit/text-ui
         "../src/minikanren.rkt"
         (only-in "../src/search-runtime.rkt" configuration-status)
         (only-in "../src/search-picture.rkt" committed-answer-nodes)
         (only-in "../derivations/shared/kernel-equations.rkt"
                  current-atomic-observer))

(define mini-same-program
  "(defrel (same x y)
     (== x y))
   (run* (q)
     (same q 'cat))")

(define micro-same-program
  "(run* (q)
     (== q 'cat))")

(define bounded-program
  "(run 2 (q)
     (conde
       [(== q 'a)]
       [(== q 'b)]
       [(== q 'c)]))")

(define diverging-program
  "(defrel (loopo x)
     (loopo x))
   (run* (q)
     (loopo q))")

(define/provide-test-suite PROGRAM-RUNNER
  (test-case "run-source->answers returns reified answers for mini source"
    (check-equal? (run-source->answers mini-same-program)
                  (list (hasheq 'sym "cat"))))

  (test-case "run-source exposes answer nodes and picture for direct micro source"
    (define result
      (run-source micro-same-program
                  #:source-mode "micro"))
    (check-equal? (run-result-answers result)
                  (list (hasheq 'sym "cat")))
    (check-true (positive? (run-result-step-count result)))
    (check-equal? (length (run-result-answer-nodes result))
                  1)
    (check-equal? (hash-ref (car (run-result-answer-nodes result)) 'renderRole)
                  "terminal-answer")
    (check-equal? (hash-ref (car (run-result-answer-nodes result)) 'name) "Solo")
    (check-equal? (hash-ref (car (run-result-answer-nodes result)) 'children) '())
    (check-true (hash? (run-result-picture result))))

  (test-case "run-source->picture preserves reified answers"
    (define picture
      (run-source->picture mini-same-program))
    ;; Projection shape includes the actual Forced/Solo observer spine. The
    ;; library's answer list remains authoritative; this checks its rendering.
    (define (picture-answers node)
      (match node
        [(hash* ['renderRole (or "answer-node" "terminal-answer")] #:open) (list node)]
        [(hash* ['children children] #:open) (append-map picture-answers children)]
        [_ '()]))
    (check-equal? (map (lambda (node) (hash-ref node 'reified))
                       (picture-answers picture))
                  (list (hasheq 'sym "cat"))))

  (test-case "run-source->host-answers returns Racket-shaped answers"
    (check-equal? (run-source->host-answers mini-same-program)
                  '(cat)))

  (test-case "answer-limit returns its prefix after completing the final round"
    (check-equal? (run-source->host-answers bounded-program
                                            #:answer-limit 2)
                  '(a b))
    (check-equal? (run-source->host-answers bounded-program
                                            #:answer-limit 0)
                  '()))

  (test-case "a surplus chunk is fully committed and its next Delay stays untouched"
    (define source
      "(run* (q) (disj (disj (== q 'a) (== q 'b)) (Zzz (== q 'later))))")
    (define work '())
    (define result
      (parameterize ([current-atomic-observer
                      (lambda (goal state) (set! work (cons goal work)))])
        (run-source source #:source-mode "micro" #:answer-limit 1)))
    (check-equal? (run-result-host-answers result) '(a))
    (check-equal? (map third (reverse work)) '((sym "a") (sym "b")))
    (check-equal? (configuration-status (run-result-final-config result)) 'paused)
    (check-equal? (map (lambda (answer) (hash-ref answer 'reified))
                       (committed-answer-nodes (run-result-final-config result) '(u:0)))
                  (list (hasheq 'sym "a") (hasheq 'sym "b")))
    ;; Compare the exact stopping configuration with manual stepping to the
    ;; first Frontier boundary, including retained owners and the residual.
    (define (first-boundary session)
      (match (model-session-status session)
        [(or 'paused 'complete) session]
        [_ (first-boundary (model-session-step session))]))
    (define manual (first-boundary (open-source source #:source-mode "micro")))
    (check-equal? (run-result-final-config result) (model-session-current-config manual))
    (check-equal? (run-result-step-count result) (model-session-step-index manual))
    (check-equal? (model-session-current-step-name manual) "commit-delay")
    ;; Reaching the boundary exactly at the fuel limit succeeds.
    (check-equal? (run-result-final-config
                  (run-source source #:source-mode "micro" #:answer-limit 1
                              #:step-cap (run-result-step-count result)))
                  (run-result-final-config result))
    ;; The GUI's manual step remains able to cross that same Delay.
    (check-equal? (model-session-current-step-name (model-session-step manual)) "advance"))

  (test-case "below-limit frontiers advance until a whole round supplies enough"
    (define source
      "(run* (q) (disj (== q 'a)
        (Zzz (disj (== q 'b) (Zzz (== q 'later))))))")
    (define work '())
    (define result
      (parameterize ([current-atomic-observer
                      (lambda (goal state) (set! work (cons goal work)))])
        (run-source source #:source-mode "micro" #:answer-limit 2)))
    (check-equal? (run-result-host-answers result) '(a b))
    (check-equal? (map third (reverse work)) '((sym "a") (sym "b")))
    (check-equal? (configuration-status (run-result-final-config result)) 'paused)
    (check-match (run-result-final-config result)
                 `(program () (Emit ,_ ,_ (Forced ,_ (Emit ,_ ,_ (More (Delay ,_ ,_))))))))

  (test-case "completion below the requested count and empty delay rounds are boundaries"
    (define result
      (run-source "(run* (q) (Zzz (Zzz (== q 'a))))"
                  #:source-mode "micro" #:answer-limit 3))
    (check-equal? (run-result-host-answers result) '(a))
    (check-equal? (configuration-status (run-result-final-config result)) 'complete)
    (check-match (run-result-final-config result)
                 `(program () (Forced ,_ (Forced ,_ (Solo ,_ ,_)))))
    (define failed
      (run-source "(run* (q) (== 'a 'b))" #:source-mode "micro" #:answer-limit 1))
    (check-equal? (run-result-host-answers failed) '())
    (check-equal? (configuration-status (run-result-final-config failed)) 'complete))

  (test-case "a candidate cannot satisfy the limit before strict work or pending bind"
    (check-exn
     #rx"step cap"
     (lambda ()
       (run-source "(defrel (loopo q) (loopo q))
                    (run* (q) (disj (== q 'a) (loopo q)))"
                   #:source-mode "micro" #:answer-limit 1 #:step-cap 20)))
    (define result
      (run-source "(run* (q) (conj (disj (== q 'a) (== q 'b)) (== q 'b)))"
                  #:source-mode "micro" #:answer-limit 1))
    (check-equal? (run-result-host-answers result) '(b))
    (check-equal? (configuration-status (run-result-final-config result)) 'complete))

  (test-case "zero keeps the existing no-work request convention"
    (define work '())
    (define result
      (parameterize ([current-atomic-observer
                      (lambda (goal state) (set! work (cons goal work)))])
        (run-source diverging-program #:source-mode "micro" #:answer-limit 0)))
    (check-equal? work '())
    (check-equal? (run-result-step-count result) 0)
    (check-equal? (run-result-final-config result) (run-result-initial-config result))
    (check-equal? (run-result-host-answers result) '()))

  (test-case "lattice limits also stop at the next exposed Delay, not the first committed head"
    (define finite
      "(run* (q) (disj (disj (== q 'a) (== q 'b)) (Zzz (== q 'later))))")
    (define unguarded
      "(defrel (loopo q) (loopo q)) (run* (q) (disj (== q 'a) (loopo q)))")
    (for ([strategy (in-list all-surfaced-search-strategies)])
      (define result
        (run-source finite #:source-mode "micro" #:search-strategy strategy #:answer-limit 1))
      (check-equal? (run-result-host-answers result) '(a))
      (check-equal? (configuration-status (run-result-final-config result)) 'paused)
      (check-match (run-result-final-config result)
                   `(program ,_ (Emit ,_ ,_ (Emit ,_ ,_ (More (Delay ,_ ,_))))))
      (check-equal? (length (committed-answer-nodes (run-result-final-config result) '(u:0))) 2)
      ;; Strict operand maturation cannot expose a committed answer before
      ;; this unguarded residual, even in a manually stepped session.
      (check-exn #rx"step cap"
                 (lambda ()
                   (run-source unguarded #:source-mode "micro" #:search-strategy strategy
                               #:answer-limit 1 #:step-cap 30)))
      (check-equal? (run-result-step-count
                    (run-source unguarded #:source-mode "micro" #:search-strategy strategy
                                #:answer-limit 0)) 0)))

  (test-case "run-source enforces a step cap for diverging programs"
    (check-exn
     (lambda (e)
       (and (exn:fail? e)
            (regexp-match? #rx"step cap" (exn-message e))))
     (lambda ()
       (run-source->answers diverging-program #:step-cap 4)))))

(module+ test
  (run-tests PROGRAM-RUNNER))
