#lang racket

(require rackunit json racket/runtime-path redex/reduction-semantics
         "../src/search-picture.rkt"
         "../derivations/matrix/full-source.rkt"
         "../derivations/test-support/witnesses.rkt")

(define-runtime-path contract-path "../../contracts/visible-node-contract.json")
(define allowed-names
  (hash-ref (call-with-input-file contract-path read-json) 'visibleNodeNames))
(define empty-state '(state () () () (label "initial")))
(define yes '(succeed (label "yes")))
(define no '(fail (label "no")))

(define (nodes picture)
  (cons picture (append-map nodes (hash-ref picture 'children '()))))

(define (named picture name)
  (filter (lambda (node) (equal? (hash-ref node 'name) name)) (nodes picture)))

(define (check-picture configuration [queries '()])
  (define picture (cfg->operational-picture configuration queries))
  (check-true (jsexpr? picture))
  (for ([part (in-list (nodes picture))])
    (check-not-false (member (hash-ref part 'name) allowed-names))
    (check-not-false (member (hash-ref part 'semanticKind)
                            '("search" "operation" "frontier" "answer" "goal")))
    (check-not-equal? (hash-ref part 'name) "Freshened")
    (when (hash-has-key? part 'activeChildIndex)
      (check-true (< (hash-ref part 'activeChildIndex)
                    (length (hash-ref part 'children))))))
  picture)

(define (trace-pictures configuration [fuel 2000])
  (check-picture configuration)
  (match (apply-reduction-relation strict-s-rel-red configuration)
    ['() (list configuration)]
    [(list next)
     (when (zero? fuel) (error 'trace-pictures "unexpected fixture exhaustion"))
     (cons configuration (trace-pictures next (sub1 fuel)))]))

(module+ test
  (test-case "strict candidates do not become answers while a sibling or bind is pending"
    (define query-owner '(Owners (Owner (u:0) (label "query"))))
    (define state '(state ((u:0 (sym "a"))) () () (label "head")))
    (define candidate `(Yield (Owners) (Answer (Owners) ,state) (One (Owners) ,state)))
    (for ([configuration
           (in-list (list `(commit (One ,query-owner ,state))
                          `(commit (bind ,query-owner ,candidate ,no))
                          `(commit (mplus ,query-owner (One (Owners) ,state)
                                          (eval (Owners) ,yes ,empty-state)))))])
      (define picture (check-picture configuration '(u:0)))
      (check-equal? (committed-answer-nodes configuration '(u:0)) '())
      (check-equal? (named picture "Answer") '())
      (check-true (pair? (named picture "Candidate"))))
    (define picture
      (check-picture `(mplus ,query-owner (One (Owners) ,state)
                            (eval (Owners) ,yes ,empty-state)) '(u:0)))
    (check-equal? (hash-ref (car (named picture "Mplus")) 'activeChildIndex) 1))

  (test-case "a successful candidate that fails pending bind is never returned as an answer"
    (define start
      (s-rel-query-initial
       '(∃ (x:q) ((x:q =? (sym "candidate") (label "head"))
                   ∧ (fail (label "tail")) (label "bind")) (label "query"))))
    (for ([configuration (in-list (trace-pictures start))])
      (check-equal? (committed-answer-nodes configuration '(u:0)) '())))

  (test-case "commit publishes a head before its Frontier tail has finished construction"
    (define state '(state ((u:9 (nat 7))) () () (label "answer")))
    (define owners '(Owners (Owner (u:9) (label "query"))))
    (define partial `(Emit ,owners (Answer (Owners) ,state)
                           (commit (Delay (Owners) (eval (Owners) ,yes ,empty-state)))))
    (for ([configuration (in-list (list partial `(advance ,partial) `(collect ,partial)
                                        `(program () (Forced (Owners) ,partial))))])
      (check-equal? (map (lambda (node) (hash-ref node 'reified))
                         (committed-answer-nodes configuration '(u:9)))
                    (list (hasheq 'num 7)))))

  (test-case "common introductions reach the residual; private and empty groups retain their exact locations"
    (define first-state '(state ((u:9 (sym "a"))) () () (label "first")))
    (define second-state '(state ((u:9 (sym "b"))) () () (label "second")))
    (define frontier
      `(Emit (Owners (Owner () (label "unused")) (Owner (u:9) (label "common")))
             (Answer (Owners (Owner (u:2) (label "private-first"))) ,first-state)
             (Last (Owners)
                   (Answer (Owners (Owner (u:3) (label "private-second"))) ,second-state))))
    (define picture (check-picture frontier '(u:9)))
    (check-equal? (map (lambda (node) (hash-ref node 'scope))
                       (committed-answer-nodes frontier '(u:9)))
                  '((9 2) (9 3)))
    (check-equal? (hash-ref picture 'name) "Emit")
    (check-equal? (hash-ref picture 'owners)
                  (list (hasheq 'vars '() 'sourceId "unused")
                        (hasheq 'vars '(9) 'sourceId "common")))
    (check-equal? (map (lambda (node) (hash-ref node 'owners)) (named picture "Answer"))
                  (list (list (hasheq 'vars '(2) 'sourceId "private-first"))
                        (list (hasheq 'vars '(3) 'sourceId "private-second"))))
    (check-equal? (map (lambda (node) (hash-ref node 'scope)) (named picture "Answer"))
                  '((9 2) (9 3)))
    (check-equal? (hash-ref (car (named picture "Last")) 'owners) '())
    (check-equal? (map (lambda (node) (hash-ref node 'name)) (nodes picture))
                  '("Emit" "Answer" "Last" "Answer")))

  (test-case "owner annotations preserve grouping and source identity without replacing node or state identity"
    (define groups
      '(Owners (Owner () (label "empty-group"))
               (Owner (u:9 u:2) (label "visible-fresh"))
               (Owner (u:30) (label "hidden:inserted"))))
    (define configuration `(eval ,groups ,yes ,empty-state))
    (define picture (check-picture configuration '(u:9)))
    (check-equal? (hash-ref picture 'name) "Eval")
    (check-equal? (hash-ref picture 'id) "yes")
    (check-equal? (hash-ref picture 'stateId) "initial")
    (check-equal? (hash-ref picture 'scope) '(9 2 30))
    (check-equal? (hash-ref picture 'owners)
                  (list (hasheq 'vars '() 'sourceId "empty-group")
                        (hasheq 'vars '(9 2) 'sourceId "visible-fresh")
                        (hasheq 'vars '(30) 'sourceId "hidden:inserted")))
    (check-equal? (map (lambda (node) (hash-ref node 'name)) (nodes picture))
                  '("Eval" "Succeed"))
    (define hidden
      (check-picture `(eval ,groups (succeed (label "hidden:goal")) ,empty-state)))
    (check-false (hash-has-key? hidden 'id))
    (check-equal? (hash-ref hidden 'owners) (hash-ref picture 'owners)))

  (test-case "sibling-owned introductions remain private to their candidate or pending computation"
    (define configuration
      `(mplus (Owners (Owner (u:9) (label "common")))
              (One (Owners (Owner (u:2) (label "left-only"))) ,empty-state)
              (eval (Owners (Owner (u:30) (label "right-only"))) ,yes ,empty-state)))
    (define picture (check-picture configuration))
    (define left (first (hash-ref picture 'children)))
    (define right (second (hash-ref picture 'children)))
    (check-equal? (hash-ref left 'owners) (list (hasheq 'vars '(2) 'sourceId "left-only")))
    (check-equal? (hash-ref (first (hash-ref left 'children)) 'scope) '(9 2))
    (check-equal? (hash-ref right 'owners) (list (hasheq 'vars '(30) 'sourceId "right-only")))
    (check-equal? (hash-ref right 'scope) '(9 30))
    (check-equal? (hash-ref picture 'activeChildIndex) 1))

  (test-case "Done Last and Emit followed by Done remain visibly distinct"
    (define last `(Last (Owners) (Answer (Owners) ,empty-state)))
    (define emitted `(Emit (Owners) (Answer (Owners) ,empty-state) (Done (Owners))))
    (check-equal? (hash-ref (check-picture '(Done (Owners))) 'name) "Done")
    (check-not-equal? (check-picture last) (check-picture emitted))
    (check-equal? (hash-ref (check-picture last) 'name) "Last"))

  (test-case "semantic categories distinguish mature Search, eager-tail work, and committed structure"
    (define one `(One (Owners) ,empty-state))
    (define answer `(Answer (Owners) ,empty-state))
    (define pending `(eval (Owners) ,yes ,empty-state))
    (define delay `(Delay (Owners) ,pending))
    (for ([configuration (in-list (list '(Empty (Owners)) one delay
                                        `(Yield (Owners) ,answer ,one)
                                        `(YieldR (Owners) ,delay ,answer)))])
      (check-equal? (hash-ref (check-picture configuration) 'semanticKind) "search"))
    (for ([configuration (in-list (list pending `(mplus (Owners) ,one ,one)
                                        `(bind (Owners) ,one ,yes)
                                        `(Yield (Owners) ,answer ,pending)
                                        `(YieldR (Owners) ,pending ,answer)
                                        `(force ,delay) `(commit ,one)
                                        `(advance (More ,delay))))])
      (check-equal? (hash-ref (check-picture configuration) 'semanticKind) "operation"))
    (for ([configuration (in-list (list '(Done (Owners)) `(Last (Owners) ,answer)
                                        `(Emit (Owners) ,answer (commit ,delay))
                                        `(Forced (Owners) (More ,delay)) `(More ,delay)))])
      (check-equal? (hash-ref (check-picture configuration) 'semanticKind) "frontier"))
    (define uncommitted (check-picture `(commit ,one)))
    (check-equal? (hash-ref (car (named uncommitted "Candidate")) 'semanticKind) "search")
    (define committed (check-picture `(Last (Owners) ,answer)))
    (check-equal? (hash-ref (car (named committed "Answer")) 'semanticKind) "answer")
    (check-equal? (hash-ref (car (named (check-picture pending) "Succeed")) 'semanticKind) "goal"))

  (test-case "Delay retains its unevaluated resumption and prevents active-path descent"
    (define configuration
      `(program ((r:loop (x:q) (r:loop x:q (label "loop-call"))))
         (More (Delay (Owners (Owner (u:9) (label "old")))
                      (bind (Owners) (eval (Owners) (r:loop u:9 (label "invoke")) ,empty-state)
                            ,yes)))))
    (define picture (check-picture configuration '(u:9)))
    (check-equal? (committed-answer-nodes configuration '(u:9)) '())
    (check-false (hash-has-key? picture 'activeChildIndex))
    (define delayed (car (named picture "Delay")))
    (check-false (hash-has-key? delayed 'activeChildIndex))
    (check-true (hash-ref delayed 'suspended))
    (check-equal? (hash-ref (car (named picture "Rel-Call")) 'id) "invoke")
    (check-equal? (hash-ref (car (named picture "Eval")) 'scope) '(9)))

  (test-case "actual internal and public Delay contractions display their different owner locations"
    (define delay-owners '(Owners (Owner (u:9) (label "before-delay"))))
    (define body-owners '(Owners (Owner (u:2) (label "body-local"))))
    (define delayed `(Delay ,delay-owners (eval ,body-owners ,yes ,empty-state)))
    (define before (check-picture `(More ,delayed)))
    (check-equal? (hash-ref (car (named before "Delay")) 'owners)
                  (list (hasheq 'vars '(9) 'sourceId "before-delay")))
    (match-define (list "force-delay" internally-resumed)
      (s-rel-contract `(force ,delayed)))
    (match-define (list "advance-delay" publicly-resumed)
      (s-rel-contract `(advance (More ,delayed))))
    (define internal-picture (check-picture internally-resumed))
    (define public-picture (check-picture publicly-resumed))
    (define public-body (car (named public-picture "Eval")))
    (check-equal? (hash-ref internal-picture 'name) "Eval")
    (check-equal? (hash-ref internal-picture 'owners)
                  (list (hasheq 'vars '(9) 'sourceId "before-delay")
                        (hasheq 'vars '(2) 'sourceId "body-local")))
    (check-equal? (hash-ref public-picture 'name) "Forced")
    (check-equal? (hash-ref public-picture 'owners)
                  (list (hasheq 'vars '(9) 'sourceId "before-delay")))
    (check-equal? (hash-ref public-body 'owners)
                  (list (hasheq 'vars '(2) 'sourceId "body-local")))
    (check-equal? (hash-ref internal-picture 'scope) '(9 2))
    (check-equal? (hash-ref public-body 'scope) '(9 2))
    (check-equal? (map (lambda (node) (hash-ref node 'name)) (nodes public-picture))
                  '("Forced" "Commit" "Eval" "Succeed")))

  (test-case "sparse query identities reify through aliases without a dense supply assumption"
    (define state
      '(state ((u:9 u:2) (u:2 (u:30 : (u:30 : empty))))
              ((u:30 (str "tea")))
              ((u:9 =? u:2 (label "alias"))) (label "state")))
    (define frontier
      `(Last (Owners) (Answer (Owners (Owner (u:9 u:2 u:30) (label "sparse"))) ,state)))
    (define answer (car (committed-answer-nodes frontier '(u:9 u:30))))
    (check-equal? (hash-ref answer 'reified)
                  (list (hasheq 'pair (list "_.0" (hasheq 'pair (list "_.0" '())))) "_.0"))
    (check-equal? (hash-ref answer 'scope) '(9 2 30))
    (check-equal? (hash-ref answer 'stateId) "state")
    (check-equal? (hash-ref (car (hash-ref answer 'trail)) 'id) "alias")
    (check-equal? (hash-ref (car (hash-ref answer 'disequalities)) 'right)
                  (hasheq 'str "tea")))

  (test-case "literal strings and booleans retain their type in reified query values"
    (define state '(state ((u:9 (str "_.0")) (u:2 #f)) () () (label "typed")))
    (define frontier `(Last (Owners)
                           (Answer (Owners (Owner (u:9 u:2) (label "query"))) ,state)))
    (check-equal? (hash-ref (car (committed-answer-nodes frontier '(u:9 u:2))) 'reified)
                  (list (hasheq 'str "_.0") #f)))

  (test-case "compiler-inserted Delay labels cannot highlight a visible explicit Delay"
    (define configuration
      `(commit (eval (Owners)
                     (suspend (suspend ,yes (label "y0")) (label "hidden:y0"))
                     ,empty-state)))
    (define picture (check-picture configuration))
    (define delays (named picture "Goal-Delay"))
    (check-false (hash-has-key? (first delays) 'id))
    (check-equal? (hash-ref (second delays) 'id) "y0")
    (check-false (hash-has-key? (car (named picture "Eval")) 'id)))

  (test-case "state inspection distinguishes substitutions that preserve the same semantic state tag"
    (define (answer value)
      (car (committed-answer-nodes
            `(Last (Owners)
                   (Answer (Owners (Owner (u:9) (label "query")))
                           (state ((u:9 (sym ,value))) () () (label "initial"))))
            '(u:9))))
    (define left (answer "a"))
    (define right (answer "b"))
    (check-equal? (hash-ref left 'stateId) (hash-ref right 'stateId))
    (check-not-equal? (hash-ref left 'stateKey) (hash-ref right 'stateKey))
    (check-equal? (hash-ref left 'stateKey) (hash-ref (answer "a") 'stateKey)))

  (test-case "strict Railroad orientation keeps slots while focus follows operand maturation"
    (define right `(One (Owners) ,empty-state))
    (define pending `(eval (Owners) ,yes ,empty-state))
    (define configuration `(program () (commit (mplusR (Owners) ,pending ,right))))
    (define picture (cfg->operational-picture configuration '() #t))
    (define merge (car (named picture "+->")))
    ;; Right has already matured; the remaining eager work is on the left.
    (check-equal? (map (lambda (child) (hash-ref child 'name)) (hash-ref merge 'children)) '("Eval" "One"))
    (check-equal? (hash-ref merge 'activeChildIndex) 0)
    (check-equal? (committed-answer-nodes configuration '()) '())
    (define both `(program () (commit (mplusR (Owners) ,pending ,pending))))
    (check-equal? (hash-ref (car (named (cfg->operational-picture both '() #t) "+->")) 'activeChildIndex) 1)
    (define tail `(program () (commit (YieldR (Owners) ,pending (Answer (Owners) ,empty-state)))))
    (define yielded (car (named (check-picture tail) "YieldR")))
    (check-equal? (hash-ref yielded 'activeChildIndex) 0)
    (check-equal? (committed-answer-nodes tail '()) '()))

  (test-case "relation bodies and resumed recursive calls render their actual source tags"
    (define definitions
      '((r:repeat (x:q)
          ((x:q =? (sym "tea") (label "relation-answer"))
           ∨ (suspend (r:repeat x:q (label "recursive-call")) (label "pause"))
           (label "relation-choice")))))
    (define initial
      (s-rel-query-initial
       '(∃ (x:q) (r:repeat x:q (label "query-call")) (label "query"))
       #:relations definitions))
    (define first-trace (trace-pictures initial))
    (check-true
     (for/or ([configuration (in-list first-trace)])
       (for/or ([part (in-list (named (cfg->operational-picture configuration '(u:0)) "Unify"))])
         (equal? (hash-ref part 'id) "relation-answer"))))
    (define fourth-frontier
      (for/fold ([frontier (last first-trace)]) ([answer-count (in-range 1 4)])
        (check-equal? (length (committed-answer-nodes frontier '(u:0))) answer-count)
        (match-define `(program ,relations ,body) frontier)
        (last (trace-pictures `(program ,relations (advance ,body))))))
    (check-equal? (length (committed-answer-nodes fourth-frontier '(u:0))) 4))

  (for ([sample (in-list validation-witnesses)])
    (test-case (format "strict witness trace renders without changing its configuration: ~a" (witness-name sample))
      (define initial `(program () (commit ,(witness-initial sample))))
      (define frontier (last (trace-pictures initial)))
      (check-true (s-rel-frontier? frontier))
      (match-define `(program ,definitions ,body) frontier)
      (define completed (last (trace-pictures `(program ,definitions (collect ,body)))))
      (check-true (s-rel-observation? completed)))))
