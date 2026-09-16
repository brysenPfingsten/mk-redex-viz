#lang racket

(require json racket/runtime-path rackunit rackunit/text-ui
         "../src/app.rkt" "../src/program-runner.rkt" "../src/search-picture.rkt"
         "./example-compat-tests.rkt" "./test-http-helpers.rkt")

(provide VISIBLE-CONTRACTS)

(define-runtime-path contract-path "../../contracts/visible-node-contract.json")
(define allowed-names (hash-ref (call-with-input-file contract-path read-json) 'visibleNodeNames))
(define representative-labels '("fives/fours" "same" "factored continuation" "fresh branch disj"))
(define profiles
  (for*/list ([conj '("left" "right")] [disj '("left" "right")]
              [delay '("relbody" "relcall" "disj")])
    (hasheq 'conjAssoc conj 'disjAssoc disj 'delayPlacement delay)))

(define (nodes picture)
  (cons picture (append-map nodes (hash-ref picture 'children '()))))

;; Validate the public tree vocabulary and focus links, not a second search
;; semantics. In particular the candidate/answer roles must remain distinct.
(define (visible-node-shape? node)
  (define name (hash-ref node 'name #f))
  (define role (hash-ref node 'renderRole #f))
  (define children (hash-ref node 'children '()))
  (and (list? children)
       (member (hash-ref node 'semanticKind #f)
               '("search" "operation" "frontier" "answer" "goal"))
       (or (not (hash-has-key? node 'owners))
           (and (list? (hash-ref node 'owners))
                (for/and ([owner (in-list (hash-ref node 'owners))])
                  (and (hash? owner)
                       (list? (hash-ref owner 'vars #f))
                       (string? (hash-ref owner 'sourceId #f))))))
       (match (list name role (length children))
         [(list "Candidate" "candidate" 0)
          (and (equal? (hash-ref node 'semanticKind) "search")
               (equal? (hash-ref node 'nodeColor #f) "#fff2cc")
               (andmap (lambda (key) (hash-has-key? node key))
                       '(stateId stateKey scope sub disequalities trail reified)))]
         [(list "Answer" "answer-node" 0)
          (and (equal? (hash-ref node 'semanticKind) "answer")
               (equal? (hash-ref node 'nodeColor #f) "green")
               (andmap (lambda (key) (hash-has-key? node key))
                       '(stateId stateKey scope sub disequalities trail reified)))]
         [(list "Solo" "terminal-answer" 0)
          (and (equal? (hash-ref node 'semanticKind) "frontier")
               (equal? (hash-ref node 'nodeColor #f) "green")
               (andmap (lambda (key) (hash-has-key? node key))
                       '(stateId stateKey scope sub disequalities trail reified owners)))]
         [(list (or "Succeed" "Fail" "Unify" "Disequality" "Rel-Call") "goal-leaf" 0) #t]
         [(list "Empty" "search-empty" 0) #t]
         [(list "Done" "completed" 0) #t]
         [(list "Fresh" "goal-fresh" 1) #t]
         [(list "Goal-Delay" "goal-delay" 1) #t]
         [(list (or "Goal-Conj" "Goal-Disj") "goal-branch" 2) #t]
         [(list "Eval" "evaluation" 1) #t]
         [(list "One" "search-value" 1) #t]
         [(list (or "Yield" "YieldR") "search-yield" 2) #t]
         [(list (or "Mplus" "MplusR" "<-+" "+->") "search-merge" 2) #t]
         [(list "Bind" "search-bind" 2) #t]
         [(list "Delay" "delay" 1)
          (and (hash-ref node 'suspended #f) (not (hash-has-key? node 'activeChildIndex)))]
         [(list "More" "paused-frontier" 1) (not (hash-has-key? node 'activeChildIndex))]
         [(list "Emit" "stream-emit" 2) #t]
         [(list "Forced" "forced" 1) #t]
         [(list "Force" "internal-force" 1) #t]
         [(list (or "Commit" "Advance" "Collect" "Render") "observation" 1) #t]
         [_ #f])
       (for/and ([index (in-list (append (if (hash-has-key? node 'activeChildIndex)
                                            (list (hash-ref node 'activeChildIndex)) '())
                                        (hash-ref node 'resolvedChildIndices '())))])
         (and (exact-nonnegative-integer? index) (< index (length children))))))

(define (check-picture picture context)
  (check-true (jsexpr? picture) context)
  (for ([node (in-list (nodes picture))])
    (check-not-false (member (hash-ref node 'name) allowed-names) context)
    (check-true (visible-node-shape? node) (format "~a: ~e" context node))))

(define (check-trace response session remaining context)
  (define payload (string->jsexpr (response-body->string response)))
  (assert-step-payload-shape payload context)
  (check-equal? (hash-ref payload 'executionStatus) (symbol->string (model-session-status session)))
  (define picture (string->jsexpr (hash-ref payload 'program)))
  (check-equal? picture (model-session-current-picture session))
  (check-picture picture context)
  (unless (or (zero? remaining) (model-session-done? session))
    (define-values (next-response next) (step! session))
    (check-trace next-response next (sub1 remaining) context)))

(define (check-source source [profile #f] [context "visible corpus"])
  (define payload
    (if profile (hasheq 'text source 'sourceMode "mini" 'compileProfile profile)
        (hasheq 'text source 'sourceMode "mini")))
  (define-values (response session)
    (init! #f (make-post-init-request source payload) 'visible-contract-id))
  (check-trace response session 12 context))

(define/provide-test-suite VISIBLE-CONTRACTS
  (test-case "empty and nonempty introductions annotate their owning Forced node in source order"
    (define picture
      (cfg->operational-picture
       '(program () (Forced (Owners (Owner () (label "outer"))
                                    (Owner (u:0 u:1) (label "inner")))
                             (Done (Owners)))) '(u:0)))
    (check-picture picture "owner order")
    (check-equal? (map (lambda (node) (hash-ref node 'name)) (nodes picture))
                  '("Forced" "Done"))
    (check-equal? (hash-ref picture 'owners)
                  (list (hasheq 'vars '() 'sourceId "outer")
                        (hasheq 'vars '(0 1) 'sourceId "inner")))
    (check-false (hash-has-key? picture 'id))
    (check-equal? (hash-ref (first (hash-ref picture 'children)) 'owners) '()))

  (test-case "every frontend example emits the explicit strict computation/Frontier tree contract"
    (for ([example (in-list (frontend-example-programs))])
      (match-define (cons label source) example)
      (check-source source #f label)))

  (test-case "all twelve compiler profiles preserve the visible contract on representative examples"
    (for* ([profile (in-list profiles)] [example (in-list (frontend-example-programs))]
           #:when (member (car example) representative-labels))
      (match-define (cons label source) example)
      (check-source source profile (format "~a / ~s" label profile))))

  (test-case "Search, commitment, paused Frontier, internal force and public advance remain visible phases"
    (define state '(state () () () (label "answer")))
    (define one `(One (Owners) ,state))
    (define delayed `(Delay (Owners) (eval (Owners) (succeed (label "later")) ,state)))
    (define pictures
      (for/list ([configuration (in-list (list '(Empty (Owners)) '(Done (Owners))
                                               one `(commit ,one)
                                               `(Solo (Owners) ,state)
                                               `(Emit (Owners) (Answer (Owners) ,state) (Done (Owners)))
                                               delayed `(More ,delayed) `(force ,delayed)
                                               `(advance (More ,delayed))))])
        (define picture (cfg->operational-picture configuration '()))
        (check-picture picture "semantic boundary")
        picture))
    (check-equal? (length (remove-duplicates pictures)) (length pictures))
    (check-equal? (hash-ref (second (nodes (third pictures))) 'name) "Candidate")
    (check-equal? (hash-ref (fifth pictures) 'name) "Solo")
    (check-equal? (hash-ref (fifth pictures) 'children) '())
    (check-equal? (hash-ref (fifth pictures) 'stateId) "answer")))

(module+ test (run-tests VISIBLE-CONTRACTS))
