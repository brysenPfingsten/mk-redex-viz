#lang racket

(require racket/hash "../../../../src/search-picture-common.rkt"
         (only-in "../../../shared/kernel.rkt" owners-support))

(provide cfg->operational-picture committed-answer-nodes)

;; The earlier work-tree view lives with its semantic account. Goals, logical
;; states, and owner groups use the same presentation primitives as the GUI.
;; There is no conversion to a strict Search computation here.
(define (configuration-frontier configuration)
  (match configuration
    [`(,(? list?) ,frontier) frontier]
    [frontier frontier]))

(define (tree->picture tree introductions queries [committed? #f])
  (define (render child [world introductions] [answer? #f])
    (tree->picture child world queries answer?))
  (match tree
    [`(,constructor ,(and owners `(Owners ,_ ...)) ,parts ...)
     (with-owners
      owners introductions
      (lambda (here)
        (match (cons constructor parts)
          [(list (and terminal (or 'Done 'Dead)))
           (node (symbol->string terminal)
                 (if (eq? terminal 'Done) "completed" "search-empty") '())]
          [`(Work ,goal ,state)
           (hash-union
            (with-source-id (node "Work" "evaluation" (list (goal->picture goal)) 0 "#666")
                            (last goal))
            (state-fields state here queries))]
          [`(Returned ,state)
           (node "Returned" "search-value" (list (state-node state here queries #f)))]
          [`(Answer ,state) (state-node state here queries committed?)]
          [`(PendingDelay ,body)
           (hash-set (node "Delay" "delay" (list (render body here))) 'suspended #t)]
          [`(,(and orientation (or 'DisjL 'DisjR)) ,left ,right)
           (node (if (eq? orientation 'DisjL) "<-+" "+->") "search-branch"
                 (list (render left here) (render right here))
                 (if (eq? orientation 'DisjL) 0 1) "#ff8000")]
          [`(Conj ,work ,goal)
           (node "Conjunction" "search-conjunction"
                 (list (render work here) (goal->picture goal)) 0 "blue")]
          [`(Last ,answer)
           (hash-set* (node "Last" "completed" (list (render answer here #t)))
                      'resolvedChildIndices '(0) 'resolvedColor "green")]
          [`(Emit ,answer ,tail)
           (hash-set* (node "Emit" "stream-emit"
                            (list (render answer here #t) (render tail here)) 1)
                      'resolvedChildIndices '(0) 'resolvedColor "green")]
          [`(Forced ,tail) (node "Forced" "forced" (list (render tail here)) 0)]
          [_ (error 'tree->picture "unknown dormant work tree: ~e" tree)])))]
    [`(More ,(and delay `(PendingDelay ,_ ,_)))
     (node "More" "paused-frontier" (list (render delay)))]
    [`(More ,work) (node "More" "unfinished-frontier" (list (render work)) 0)]
    [_ (error 'tree->picture "unknown dormant frontier: ~e" tree)]))

(define (cfg->operational-picture configuration query-variables)
  (tree->picture (configuration-frontier configuration) '() query-variables))

(define (committed-answer-nodes configuration query-variables)
  ;; Last belongs to this earlier source grammar. Inspect it here rather than
  ;; making the current strict GUI accept a second terminal representation.
  (define (walk frontier introductions)
    (match frontier
      [`(Emit ,owners (Answer ,private ,state) ,tail)
       (define here (owners-support owners introductions))
       (cons (state-node state (owners-support private here) query-variables #t)
             (walk tail here))]
      [`(Last ,owners (Answer ,private ,state))
       (list (state-node state (owners-support private (owners-support owners introductions))
                         query-variables #t))]
      [`(Forced ,owners ,tail) (walk tail (owners-support owners introductions))]
      [_ '()]))
  (walk (configuration-frontier configuration) '()))
