#lang racket

(require redex/reduction-semantics
         (prefix-in wf: "../src/search-lattice/wf/all.rkt"))

(provide count-bounced
         count-answers
         count-freshened
         count-freshened-tree
         count-freshened-shell
         state-c-agrees-with-scope?
         core-c-scope-agreement?
         config-c-scope-agreement?
         core-exact-scope?
         config-exact-scope?
         visible-json-wf?
         visible-json-trace-wf?
         trace-deterministic)

(define (first-summary holds)
  (match holds
    [(list summary) summary]
    [_ #f]))

(define (safe-summary thunk)
  (with-handlers ([exn:fail? (lambda (_exn) #f)])
    (first-summary (thunk))))

(define (summary-for-config cfg)
  (or (safe-summary (lambda () (judgment-holds (wf:wf-summary-config/rail-relcall? ,cfg summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-config/search-relcall? ,cfg summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-config/relcall? ,cfg summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-cfg/rail? ,cfg summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-cfg/search? ,cfg summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-cfg/disj? ,cfg summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-cfg/delay? ,cfg summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-cfg/core? ,cfg summary) summary)))))

(define (summary-for-frontier cfg scope)
  (or (safe-summary (lambda () (judgment-holds (wf:wf-summary-frontier/rail? ,cfg ,scope summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-frontier/search? ,cfg ,scope summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-frontier/disj? ,cfg ,scope summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-frontier/delay? ,cfg ,scope summary) summary)))
      (safe-summary (lambda () (judgment-holds (wf:wf-summary-frontier/core? ,cfg ,scope summary) summary)))))

(define (state-c-agrees-with-scope? st scope)
  (match st
    [`(state ,sub ,dis ,c ,trail ,_tag)
     (equal? c scope)]
    [_ #f]))

(define (core-c-scope-agreement? f [scope '()])
  (and (summary-for-frontier f scope) #t))

(define (core-exact-scope? f [scope '()])
  (and (summary-for-frontier f scope) #t))

(define (config-c-scope-agreement? cfg)
  (and (summary-for-config cfg) #t))

(define (config-exact-scope? cfg)
  (and (summary-for-config cfg) #t))

(define (count-bounced datum)
  (match (summary-for-config datum)
    [summary #:when summary
             (wf:summary-bounced-count/host summary)]
    [_ 0]))

(define (count-answers datum)
  (match (summary-for-config datum)
    [summary #:when summary
             (wf:summary-answer-count/host summary)]
    [_ 0]))

(define (count-freshened datum)
  (match (summary-for-config datum)
    [summary #:when summary
             (wf:summary-freshened-count/host summary)]
    [_ 0]))

(define (count-freshened-tree datum)
  (match (summary-for-config datum)
    [summary #:when summary
             (wf:summary-freshened-tree-count/host summary)]
    [_ 0]))

(define (count-freshened-shell datum)
  (match (summary-for-config datum)
    [summary #:when summary
             (wf:summary-freshened-shell-count/host summary)]
    [_ 0]))

(define (trace-deterministic rel cfg [step-cap 64])
  (define next*
    (remove-duplicates
     (apply-reduction-relation/tag-with-names rel cfg)))
  (match next*
    ['()
     (values '() cfg 'done)]
    [_ #:when (zero? step-cap)
       (values '() cfg 'cap)]
    [(list (list name cfg1))
     (define-values (steps cfg^ status)
       (trace-deterministic rel cfg1 (sub1 step-cap)))
     (values (cons (~a name) steps)
             cfg^
             status)]
    [_ (values '() cfg 'nondeterministic)]))

(define (valid-child-indexes? node)
  (match node
    [(hash* ['children children] #:open)
     (and
      (match (hash-ref node 'activeChildIndex #f)
        [#f #t]
        [(? exact-nonnegative-integer? idx)
         (< idx (length children))]
        [_ #f])
      (match (hash-ref node 'resolvedChildIndices #f)
        [#f #t]
        [(list idxs ...)
         (for/and ([idx (in-list idxs)])
           (and (exact-nonnegative-integer? idx)
                (< idx (length children))))]
        [_ #f]))]
    [_ #t]))

(define (visible-answer-node? node)
  (match node
    [(hash* ['name "Answer"]
            ['renderRole "answer-node"]
            ['nodeColor "green"]
            #:open)
     #t]
    [(hash* ['name "Freshened"]
            ['renderRole "freshened"]
            ['children (list child)]
            #:open)
     (visible-answer-node? child)]
    [_ #f]))

(define (visible-search-tree? node)
  (match node
    [(hash* ['name "Empty"]
            ['renderRole "terminal"]
            #:open)
     #t]
    [(hash* ['name "Succeed"]
            ['renderRole "terminal"]
            #:open)
     #t]
    [(hash* ['name "Fail"]
            ['renderRole "terminal"]
            #:open)
     #t]
    [(hash* ['name "Unify"]
            ['renderRole "goal-leaf"]
            #:open)
     #t]
    [(hash* ['name "Disequality"]
            ['renderRole "goal-leaf"]
            #:open)
     #t]
    [(hash* ['name "Rel-Call"]
            ['renderRole "goal-leaf"]
            #:open)
     #t]
    [(hash* ['name "Fresh"]
            ['renderRole "goal-fresh"]
            ['activeChildIndex 0]
            ['children (list child)]
            #:open)
     (visible-search-tree? child)]
    [(hash* ['name "Goal-Delay"]
            ['renderRole "delay"]
            ['activeChildIndex 0]
            ['children (list child)]
            #:open)
     (visible-search-tree? child)]
    [(hash* ['name "Goal-Conj"]
            ['renderRole "goal-branch"]
            ['focusColor "blue"]
            ['activeChildIndex 0]
            ['children (list left right)]
            #:open)
     (and (visible-search-tree? left)
          (visible-search-tree? right))]
    [(hash* ['name "Goal-Disj"]
            ['renderRole "goal-branch"]
            ['focusColor "#ff8000"]
            ['activeChildIndex 0]
            ['children (list left right)]
            #:open)
     (and (visible-search-tree? left)
          (visible-search-tree? right))]
    [(hash* ['name "Conjunction"]
            ['renderRole "search-conjunction"]
            ['focusColor "blue"]
            ['activeChildIndex 0]
            ['children (list left right)]
            #:open)
     (and (visible-root? left)
          (visible-search-tree? right))]
    [(hash* ['name "Freshened"]
            ['renderRole "freshened"]
            ['activeChildIndex 0]
            ['children (list child)]
            #:open)
     (visible-root? child)]
    [(hash* ['name "Deferred"]
            ['renderRole "bounced"]
            ['activeChildIndex 0]
            ['children (list child)]
            #:open)
     (visible-root? child)]
    [(hash* ['name "Emit"]
            ['renderRole "stream-emit"]
            ['children (list left right)]
            #:open)
     (and (visible-root? left)
          (visible-root? right))]
    [(hash* ['name "Delay"]
            ['renderRole "delay"]
            ['activeChildIndex 0]
            ['children (list child)]
            #:open)
     (visible-root? child)]
    [(hash* ['name "<-+"]
            ['renderRole "search-branch"]
            ['focusColor "#ff8000"]
            ['activeChildIndex 0]
            ['children (list left right)]
            #:open)
     (and (visible-root? left)
          (visible-root? right))]
    [(hash* ['name "+->"]
            ['renderRole "search-branch"]
            ['focusColor "#ff8000"]
            ['activeChildIndex 1]
            ['children (list left right)]
            #:open)
     (and (visible-root? left)
          (visible-root? right))]
    [_ #f]))

(define (visible-stream? node)
  (match node
    [(hash* ['name "Empty"]
            ['renderRole "terminal"]
            #:open)
     #t]
    [(hash* ['name "Emit"]
            ['renderRole "stream-emit"]
            ['children (list left right)]
            #:open)
     (and (visible-root? left)
          (visible-root? right))]
    [(hash* ['name "Deferred"]
            ['renderRole "bounced"]
            ['children (list child)]
            #:open)
     (visible-root? child)]
    [(hash* ['name "Freshened"]
            ['renderRole "freshened"]
            ['children (list child)]
            #:open)
     (visible-root? child)]
    [_ #f]))

(define (visible-root? node)
  (and (hash? node)
       (valid-child-indexes? node)
       (or (visible-stream? node)
           (visible-answer-node? node)
           (visible-search-tree? node))))

(define (visible-json-wf? node)
  (visible-root? node))

(define (visible-json-trace-wf? nodes)
  (for/and ([node (in-list nodes)])
    (visible-json-wf? node)))
