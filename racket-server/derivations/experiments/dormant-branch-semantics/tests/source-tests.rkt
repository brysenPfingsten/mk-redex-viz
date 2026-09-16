#lang racket

(require rackunit
         (prefix-in redex: redex/reduction-semantics)
         (prefix-in source: "../derivation/source.rkt")
         (prefix-in map: "../derivation/lattice-map.rkt")
         (prefix-in direct: "../derivation/interpreter.rkt")
         (prefix-in dfs: "../source/reduction-relations/search-dfs-relcall-red.rkt")
         (prefix-in flip: "../source/reduction-relations/search-flip-relcall-red.rkt")
         (prefix-in rail: "../source/reduction-relations/rail-relcall-red.rkt")
         (prefix-in wf: "../source/wf/all.rkt")
         (only-in "../../../shared/wf.rkt" wf-s-rel?)
         (only-in "../../../shared/kernel.rkt" owners-append)
         (only-in "../../../s-reference/relations.rkt" goal-body)
         "../../../shared/kernel-equations.rkt"
         "../../../test-support/witnesses.rkt")

(define empty-state '(state () () () (label "initial")))
(define (atom name) `((sym ,name) =? (sym ,name) (label ,name)))
(define (or-goal left right) `(,left ∨ ,right (label "choice")))
(define (and-goal left right) `(,left ∧ ,right (label "bind")))
(define (suspend-goal goal) `(suspend ,goal (label "delay")))

;; Static scope abstraction for reusing the established S WF predicate ONLY.
;; Yield's two worlds become mplus's two worlds; their exact Owner paths and
;; states are retained. This never evaluates or translates an executing state.
;; The historical terminal's two Owner fields form one path with no residual;
;; its scope-only image uses the current terminal grammar for this WF check.
(define (scope-shape term)
  (match term
    [`(program ,_ ,definitions ,body) `(program ,definitions ,(scope-shape body))]
    [`(Yield ,owners ,(and answer `(Answer ,private ,state)) ,tail)
     `(mplus ,owners (One ,private ,state) ,(scope-shape tail))]
    [`(YieldR ,owners ,tail (Answer ,private ,state))
     `(mplus ,owners ,(scope-shape tail) (One ,private ,state))]
    [(or `(mplus ,owners ,left ,right) `(mplusR ,owners ,left ,right))
     `(mplus ,owners ,(scope-shape left) ,(scope-shape right))]
    [`(bind ,owners ,body ,goal) `(bind ,owners ,(scope-shape body) ,goal)]
    [`(Last ,owners (Answer ,private ,state))
     `(Solo ,(owners-append owners private) ,state)]
    [`(Emit ,owners ,answer ,tail) `(Emit ,owners ,answer ,(scope-shape tail))]
    [`(,(and constructor (or 'Delay 'Forced)) ,owners ,body)
     `(,constructor ,owners ,(scope-shape body))]
    [`(,(and constructor (or 'commit 'advance 'More)) ,body)
     `(,constructor ,(scope-shape body))]
    [_ term]))

(define (native-step policy configuration)
  ((match policy ['dfs dfs:step-once] ['flip flip:step-once] ['rail rail:step-once]) configuration))
(define (native-wf? policy configuration)
  (match policy
    ['rail (redex:judgment-holds (wf:wf-config/rail-relcall? ,configuration))]
    [_ (redex:judgment-holds (wf:wf-config/search-relcall? ,configuration))]))

(define covered-native-labels '())
(define covered-administrative-labels '())
(define (record-native-label! label)
  (unless (member label covered-native-labels)
    (set! covered-native-labels (cons label covered-native-labels))))
(define (record-administrative-label! label)
  (unless (member label covered-administrative-labels)
    (set! covered-administrative-labels (cons label covered-administrative-labels))))

(define (check-native-trace current fuel [labels '()] #:prefix? [prefix? #f])
  (check-true (source:source-in-domain? current))
  (check-true (wf-s-rel? (scope-shape current)))
  (match-define `(program ,policy ,_ ,body) current)
  (define image (map:source->lattice current))
  (check-true (native-wf? policy image))
  (check-equal? (source:plug (source:focus-term (source:decompose body))
                            (source:focus-frames (source:decompose body))) body)
  (cond
    [(zero? fuel)
     (unless prefix? (check-true (source:complete? body) "finite trace must complete within its budget"))
     labels]
    [else
     (match (source:step current)
       [(list label next)
        (define target (map:source->lattice next))
        (cond [(map:administrative? label)
               (record-administrative-label! label)
               (check-equal? target image)
               (check-true (< (map:administrative-weight next)
                              (map:administrative-weight current)))]
              [else
               (record-native-label! (map:native-label current label))
               (check-equal? (native-step policy image)
                             (list (list (map:native-label current label) target)))])
        (check-native-trace next (sub1 fuel) (cons label labels) #:prefix? prefix?)]
       [#f
        (cond
          [(source:complete? body)
           (check-equal? (native-step policy image) '())
           labels]
          [else
           (check-true (source:frontier? body))
           (define span (map:public-span current))
           (record-native-label! "force-delay")
           (for ([before (in-list (drop-right span 1))])
             (match-define (list crossed-label crossed-next) (source:step before))
             (when (map:administrative? crossed-label)
               (record-administrative-label! crossed-label)
               (check-equal? (map:source->lattice crossed-next) (map:source->lattice before))
               (check-true (< (map:administrative-weight crossed-next)
                              (map:administrative-weight before)))))
           (check-equal? (length span) (+ 2 (map:spine-length body)))
           (check-equal? (map:source->lattice (car span)) image)
           (check-equal? (native-step policy image)
                         (list (list "force-delay" (map:source->lattice (last span)))))
           (check-native-trace (last span) (sub1 fuel) (cons "advance-delay" labels)
                               #:prefix? prefix?)])])]))

(define (reify-resume resume descriptions)
  (match (hash-ref descriptions resume)
    [(list 'eval (list _ goal state)) `(eval (Owners) ,(goal-body goal) ,state)]
    [(list 'scope (list owners body)) (source:lift owners (reify-resume body descriptions))]
    [(list 'merge (list _ side left right))
     `(,(if (eq? side 'left) 'mplus 'mplusR) (Owners)
       ,(reify-resume left descriptions) ,(reify-resume right descriptions))]
    [(list 'bind (list _ body continue))
     `(bind (Owners) ,(reify-resume body descriptions) ,(reify-goal continue descriptions))]
    [(list 'continue (list continue state local))
     `(eval ,local ,(reify-goal continue descriptions) ,state)]))
(define (reify-goal continue descriptions)
  (match (hash-ref descriptions continue)
    [(list 'goal (list _ goal)) (goal-body goal)]))
(define (reify-frontier frontier descriptions)
  (match frontier
    [`(program ,definitions ,body) `(program ,definitions ,(reify-frontier body descriptions))]
    [`(More (Delay ,owners ,resume))
     `(More (Delay ,owners ,(reify-resume resume descriptions)))]
    [`(Emit ,owners ,answer ,rest)
     `(Emit ,owners ,answer ,(reify-frontier rest descriptions))]
    [`(Forced ,owners ,rest) `(Forced ,owners ,(reify-frontier rest descriptions))]
    [_ frontier]))

(define (check-direct-rounds policy example [rounds 12])
  (define descriptions (make-hasheq))
  (define direct-work '())
  (define source-work '())
  (define (observe-direct thunk)
    (parameterize ([direct:current-closure-observer
                    (lambda (family procedure captures)
                      (hash-set! descriptions procedure (list family captures)))]
                   [current-atomic-observer
                    (lambda (goal state) (set! direct-work (cons (list goal state) direct-work)))])
      (thunk)))
  (define (observe-source thunk)
    (parameterize ([current-atomic-observer
                    (lambda (goal state) (set! source-work (cons (list goal state) source-work)))])
      (thunk)))
  (define (drive-checked current [fuel 2000])
    (check-true (source:source-in-domain? current))
    (check-true (wf-s-rel? (scope-shape current)))
    (match (source:step current)
      [#f current]
      [(list _ next)
       (check-true (positive? fuel))
       (drive-checked next (sub1 fuel))]))
  (define start
    (source:initial (witness-goal example) #:policy policy #:owners (witness-owners example)
                    #:state (witness-state example)))
  (define (compare functional source remaining)
    (match-define `(program ,_ ,definitions ,body) source)
    (check-equal? (reify-frontier functional descriptions) `(program ,definitions ,body))
    (check-equal? direct-work source-work)
    (unless (source:complete? body)
      (check-true (positive? remaining))
      (compare (observe-direct (lambda () (direct:resume-once functional)))
               (observe-source (lambda () (drive-checked (source:advance source))))
               (sub1 remaining))))
  (compare
   (observe-direct
    (lambda () (direct:run (witness-goal example) #:policy policy #:relations '()
                           #:owners (witness-owners example) #:state (witness-state example))))
   (observe-source (lambda () (drive-checked start))) rounds))

(define (check-family-orientation current [fuel 500])
  (define image (source:erase-orientation current))
  (check-true (source:source-in-domain? current))
  (check-true (source:source-in-domain? image))
  (check-true (wf-s-rel? (scope-shape current)))
  (check-true (wf-s-rel? (scope-shape image)))
  (when (zero? fuel)
    (check-true (source:complete? (fourth current)) "finite orientation witness must complete"))
  (when (positive? fuel)
    (match (source:step current)
      [(list label next)
       (check-equal? (source:step image)
                     (list (source:erase-orientation-label label) (source:erase-orientation next)))
       (check-family-orientation next (sub1 fuel))]
      [#f
       (check-false (source:step image))
       (unless (source:complete? (fourth current))
         (check-equal? (source:erase-orientation (source:advance current)) (source:advance image))
         (check-family-orientation (source:advance current) (sub1 fuel)))])))

(module+ test
  (test-case "base grammar has computational tails but excludes right-active Search"
    (define right `(program rail () (commit (mplusR (Owners) (One (Owners) ,empty-state)
                                                   (One (Owners) ,empty-state)))))
    (check-true (source:source-in-domain? right))
    (check-false (source:source-in-domain? `(program flip () ,(fourth right))))
    (check-false (source:source-in-domain? `(program dfs () ,(fourth right))))
    (check-true (source:source-in-domain?
                 `(program flip () (commit (Yield (Owners) (Answer (Owners) ,empty-state)
                                                  (eval (Owners) (fail (label "f")) ,empty-state)))))))
  (test-case "direct demand interpreter and source agree at every public frontier and on actual work"
    (for* ([policy '(dfs flip rail)] [example (in-list validation-witnesses)])
      (with-check-info (['policy policy] ['witness (witness-name example)])
        (check-direct-rounds policy example))))
  (test-case "extended interpreter erases to Flip exactly, including retained ownership"
    (for ([example (in-list validation-witnesses)])
      (check-family-orientation
       (source:initial (witness-goal example) #:policy 'rail #:owners (witness-owners example)
                       #:state (witness-state example)))))
  (test-case "raw native map deliberately fails when scope crosses a scheduler Delay"
    (define owners '(Owners (Owner (u:0) (label "common"))))
    (define current
      `(program flip () (commit (mplus ,owners (Delay (Owners) (eval (Owners) ,(atom "A") ,empty-state))
                                      (eval (Owners) ,(atom "B") ,empty-state)))))
    (check-true (source:source-in-domain? current))
    (check-true (wf-s-rel? (scope-shape current)))
    (match-define (list "mplus-delay" next) (source:step current))
    (match-define (list (list "flip-delay-left" native-next))
      (native-step 'flip (map:source->lattice current)))
    (check-not-equal? native-next (map:source->lattice next))
    (check-true (native-wf? 'flip native-next))
    (check-true (native-wf? 'flip (map:source->lattice next))))
  (test-case "prescribed native squares for finite no-allocation fragment"
    (define seeds (list (atom "A") (atom "B") '(succeed (label "yes")) '(fail (label "no"))))
    (define goals
      (append seeds (map suspend-goal seeds)
              (list '((sym "A") != (sym "B") (label "disequality-success"))
                    '((sym "A") != (sym "A") (label "disequality-fail"))
                    '((sym "A") =? (sym "B") (label "unify-fail")))
              (for*/list ([left (in-list (append seeds (map suspend-goal seeds)))]
                          [right (in-list (append seeds (map suspend-goal seeds)))])
                (or-goal left right))
              (for*/list ([left (in-list seeds)] [right (in-list seeds)])
                (and-goal (or-goal (suspend-goal left) right) (suspend-goal (atom "then"))))
              (for*/list ([a (in-list seeds)] [b (in-list seeds)] [c (in-list seeds)])
                (or-goal (suspend-goal a) (or-goal (suspend-goal b) (suspend-goal c))))))
    (for* ([policy '(dfs flip rail)] [goal (in-list goals)])
      (with-check-info (['policy policy] ['goal goal])
        (check-native-trace (source:initial goal #:policy policy) 250))))
  (test-case "native relation calls and guarded infinite prefixes use the same squares"
    (define definitions
      `((r:loop () (suspend (r:loop (label "again")) (label "guard")))
        (r:left (x:q) ((x:q =? (sym "A") (label "left")) ∨
                      (r:right x:q (label "call-right")) (label "mutual-left")))
        (r:right (x:q) (x:q =? (sym "B") (label "right")))))
    (for ([policy '(dfs flip rail)])
      (check-native-trace
       (source:initial (or-goal '(r:loop (label "loop")) (atom "B"))
                        #:policy policy #:relations definitions) 150 #:prefix? #t)
       (check-native-trace
       (source:initial '(r:left (sym "B") (label "finite-call"))
                        #:policy policy #:relations definitions) 150)))
  (test-case "all four nested candidate orientations have native reassociation squares"
    ;; These are mature, well-formed source configurations: the inner search
    ;; has returned a candidate while an enclosing choice still owns it.
    ;; Expected native configurations come from independent source contraction,
    ;; not a copied native-rule right-hand side. All Owners remain empty.
    (for* ([policy '(dfs flip rail)] [outer '(left right)] [inner '(left right)]
           #:when (or (eq? policy 'rail) (and (eq? outer 'left) (eq? inner 'left))))
      (define passive `(eval (Owners) ,(atom "outer-passive") ,empty-state))
      (define residual `(eval (Owners) ,(atom "inner-residual") ,empty-state))
      (define candidate `(Answer (Owners) ,empty-state))
      (define active
        (if (eq? inner 'left) `(Yield (Owners) ,candidate ,residual)
            `(YieldR (Owners) ,residual ,candidate)))
      (define body
        (if (eq? outer 'left) `(mplus (Owners) ,active ,passive)
            `(mplusR (Owners) ,passive ,active)))
      (check-native-trace `(program ,policy () (commit ,body)) 80)))
  (test-case "local allocation and constrained-unification squares do not claim full scope transport"
    ;; These one-step checks deliberately leave the empty-Owners fragment.
    ;; They establish the primitive label/map cases only. In particular, they
    ;; do not extend the native trace theorem across subsequent scheduler scope
    ;; movement; the earlier owner-transport counterexample remains intact.
    (define owners '(Owners (Owner (u:0) (label "existing"))))
    (define constrained-state
      '(state () ((u:0 (sym "A"))) () (label "initial")))
    (for* ([policy '(dfs flip rail)]
           [fixture (in-list
                     (list (list '(∃ (x:q) (succeed (label "body")) (label "local-fresh"))
                                 owners empty-state "eval-fresh" "allocate-fresh")
                           (list '(u:0 =? (sym "A") (label "violates"))
                                 owners constrained-state "eval-atom" "unify-violates-disequality")))])
      (match-define (list goal incoming state source-label native-label) fixture)
      (define current (source:initial goal #:policy policy #:owners incoming #:state state))
      (check-true (source:source-in-domain? current))
      (check-true (wf-s-rel? (scope-shape current)))
      (check-true (native-wf? policy (map:source->lattice current)))
      (match-define (list actual-label next) (source:step current))
      (check-equal? actual-label source-label)
      (check-equal? (map:native-label current actual-label) native-label)
      (check-equal? (native-step policy (map:source->lattice current))
                    (list (list native-label (map:source->lattice next))))
      (check-true (source:source-in-domain? next))
      (check-true (wf-s-rel? (scope-shape next)))
      (check-true (native-wf? policy (map:source->lattice next)))
      (record-native-label! native-label)))
  (test-case "explicit advancement of terminal Frontiers is administrative"
    (for* ([policy '(dfs flip rail)]
           [terminal (in-list (list '(Done (Owners))
                                    `(Last (Owners) (Answer (Owners) ,empty-state))))])
      (check-native-trace `(program ,policy () (advance ,terminal)) 10)))
  (test-case "named native-map and administrative cases have explicit coverage"
    (define native-labels
      (remove-duplicates
       (for*/list ([relation (in-list (list dfs:search-dfs-relcall-red
                                            flip:search-flip-relcall-red
                                            rail:rail-relcall-red))]
                    [label (in-list (redex:reduction-relation->rule-names relation))])
         (format "~a" label))))
    (check-equal? (sort covered-native-labels string<?) (sort native-labels string<?))
    (check-equal? (sort covered-administrative-labels string<?)
                  (sort map:administrative-labels string<?))))
