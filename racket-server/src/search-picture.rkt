#lang racket

(require racket/hash
         "search-picture-common.rkt"
         (only-in "../derivations/matrix/scheduler-source.rkt" scheduler-value?))

(provide cfg->operational-picture committed-answer-nodes
         label->visible-id term->visible-json)

;; The live view reads strict configurations directly, including the native
;; Railroad orientation. Only the outer Frontier supplies committed answers.
(define (configuration-body configuration)
  (match configuration
    [`(program ,_ ,body) body]
    [body body]))

(define (tree->picture term introductions query-variables [committed? #f] [oriented? #f])
  (define (render child [world introductions] [answer? #f])
    (tree->picture child world query-variables answer? oriented?))
  (define picture
    (match term
    [`(,constructor ,(and owners `(Owners ,_ ...)) ,parts ...)
     (with-owner-annotations
      owners introductions
      (lambda (here)
        (match (cons constructor parts)
          [(list (and terminal (or 'Empty 'Done)))
           (node (symbol->string terminal) (if (eq? terminal 'Done) "completed" "search-empty") '())]
          [`(eval ,goal ,state)
           (hash-union (with-source-id
                        (node "Eval"
                              "evaluation" (list (goal->picture goal)) 0 "#666")
                        (last goal))
                       (state-fields state here query-variables))]
          [`(One ,state)
           (node "One" "search-value"
                 (list (state-node state here query-variables #f)))]
          [`(Answer ,state) (state-node state here query-variables committed?)]
          [`(Yield ,answer ,tail)
           (node "Yield" "search-yield" (list (render answer here) (render tail here))
                 (and (not (scheduler-value? tail)) 1) "#a66b00")]
          [`(YieldR ,tail ,answer)
           (node "YieldR" "search-yield" (list (render tail here) (render answer here))
                 (and (not (scheduler-value? tail)) 0) "#a66b00")]
          [`(Delay ,body)
           (hash-set (node "Delay" "delay" (list (render body here))) 'suspended #t)]
          [`(,(and orientation (or 'mplus 'mplusR)) ,left ,right)
           (define right? (eq? orientation 'mplusR))
           (node (if oriented? (if right? "+->" "<-+") (if right? "MplusR" "Mplus"))
                 "search-merge" (list (render left here) (render right here))
                 (if right?
                     (cond [(not (scheduler-value? right)) 1] [(not (scheduler-value? left)) 0] [else #f])
                     (cond [(not (scheduler-value? left)) 0] [(not (scheduler-value? right)) 1] [else #f]))
                 "#ff8000")]
          [`(bind ,search ,goal)
           (node "Bind" "search-bind" (list (render search here) (goal->picture goal))
                 (and (not (scheduler-value? search)) 0) "blue")]
          [`(Solo ,state) (solo-node state here query-variables)]
          [`(Emit ,answer ,tail)
           (hash-set* (node "Emit" "stream-emit"
                            (list (render answer here #t) (render tail here)) 1)
                      'resolvedChildIndices '(0) 'resolvedColor "green")]
          [`(Forced ,tail) (node "Forced" "forced" (list (render tail here)) 0)]
          [_ (error 'tree->picture "unknown owned source term: ~e" term)])))]
    [`(More ,(and delay `(Delay ,_ ,_)))
     (node "More" "paused-frontier" (list (render delay)))]
    [`(,(and operation (or 'force 'commit 'advance 'collect 'render)) ,body)
     (node (string-titlecase (symbol->string operation))
           (if (eq? operation 'force) "internal-force" "observation")
           (list (render body)) 0)]
    [_ (error 'tree->picture "unknown source computation: ~e" term)]))
  (hash-set
   picture 'semanticKind
   (match term
     [`(,(or 'Empty 'One 'Delay) ,_ ...) "search"]
     [`(,(or 'Yield 'YieldR) ,_ ...)
      (if (scheduler-value? term) "search" "operation")]
     [`(Answer ,_ ,_) (if committed? "answer" "search")]
     [`(,(or 'Done 'Solo 'Emit 'Forced 'More) ,_ ...) "frontier"]
     [_ "operation"])))

(define (cfg->operational-picture configuration query-variables [oriented? #f])
  (tree->picture (configuration-body configuration) '() query-variables #f oriented?))

(define (committed-answer-nodes configuration query-variables)
  (frontier-answer-nodes (configuration-body configuration) query-variables))
