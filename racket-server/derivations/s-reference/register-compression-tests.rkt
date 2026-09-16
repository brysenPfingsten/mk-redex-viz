#lang racket

(require rackunit redex/reduction-semantics
         "data.rkt" "readback.rkt" "source.rkt" "machine-correspondence.rkt"
         "compression-correspondence.rkt"
         (prefix-in f: "machine.rkt")
         (prefix-in r: "registers.rkt")
         (prefix-in c: "compressed.rkt")
         (prefix-in derive: "derive.rkt")
         (prefix-in compress: "compression-derive.rkt")
         (prefix-in a: "../shared/stages/schema.rkt")
         (prefix-in maps: "../shared/stages/maps.rkt")
         (prefix-in q: "../shared/maps.rkt")
         "../shared/wf.rkt" "../test-support/corpus.rkt"
         "../test-support/witnesses.rkt"
         "../shared/kernel-equations.rkt"
         "../shared/runtime.rkt")

;; Harness closures and observers belong to the test, never to machine data.
(struct Engine (name from-machine decode step! drive! run advance collect
                     pc slots steps signatures) #:transparent)
(struct Observation (value work) #:transparent)
(struct Trial (value work dispatches original-steps) #:transparent)

(define register-engine
  (Engine 'registers r:from-machine r:decode r:step! r:drive! r:run
          r:resume-once r:collect-all r:Registers-pc r:register-values
          r:Registers-steps r:signatures))
(define compressed-engine
  (Engine 'compressed c:from-machine c:decode c:step! c:drive! c:run
          c:resume-once c:collect-all c:Registers-pc c:register-values
          c:Registers-steps c:signatures))
(define engines (list register-engine compressed-engine))
(define seen-controls (make-hash))
(define seen-constructors (make-hash))

(define (observe compute)
  (define reversed '())
  (define value
    (parameterize ([current-atomic-observer
                    (lambda (goal state)
                      (set! reversed (cons (list goal state) reversed)))])
      (compute)))
  (Observation value (reverse reversed)))

(define (quiet compute)
  (define observed (observe compute))
  (check-equal? (Observation-work observed) '()
                "structural representation operations do no atomic work")
  (Observation-value observed))

(define (inspect-data! datum)
  (check-false (procedure? datum) "runtime registers and captured data contain no closures")
  (match datum
    [(cons first rest) (inspect-data! first) (inspect-data! rest)]
    [(? struct?)
     (define fields (struct->vector datum))
     (define constructor (vector-ref fields 0))
     (hash-set! seen-constructors constructor #t)
     (check-not-equal? constructor 'struct:KPrefix)
     (for ([field (in-vector fields 1)]) (inspect-data! field))]
    [_
     (check-true (or (null? datum) (symbol? datum) (string? datum)
                     (number? datum) (boolean? datum)))]))

(define (snapshot engine bank)
  (list ((Engine-pc engine) bank)
        ((Engine-slots engine) bank)
        ((Engine-steps engine) bank)))

(define (decode-quiet engine bank)
  (define before (snapshot engine bank))
  (define result (quiet (lambda () ((Engine-decode engine) bank))))
  (check-equal? (snapshot engine bank) before "decoding preserves all registers and the counter")
  result)

(define (whole-readback original)
  (match original
    [(f:Call pc operands) (readback-call pc operands)]
    [(f:Halted value) (readback-halted value)]))

(define (check-original original)
  (inspect-data! original)
  (define native (quiet (lambda () (functional->M original))))
  (define source (quiet (lambda () (whole-readback original))))
  (check-equal? (a:readback-M native) source)
  (check-true (redex-match? ScopeS q source))
  (check-true (wf-s? source))
  (define native-e (maps:M-SE native))
  (define native-n (maps:M-SN native))
  (check-equal? (maps:M-EN native-e) native-n)
  (check-equal? (a:readback-M native-e) (q:Q-SE source))
  (check-equal? (a:readback-M native-n) (q:Q-SN source))
  (check-true (wf-e? (a:readback-M native-e)))
  (check-true (wf-n? (a:readback-M native-n))))

(define (check-bank engine bank)
  (inspect-data! bank)
  (define pc ((Engine-pc engine) bank))
  (hash-set! seen-controls (list (Engine-name engine) pc) #t)
  (define arity
    (match pc
      ['halt 1]
      [_ (length (rest (assq pc (Engine-signatures engine))))]))
  (check-equal? (drop ((Engine-slots engine) bank) arity)
                (make-list (- 5 arity) #f)
                "unused registers are cleared after every dispatch")
  (check-true (exact-nonnegative-integer? ((Engine-steps engine) bank)))
  (define original (decode-quiet engine bank))
  (check-original original)
  (when (eq? (Engine-name engine) 'compressed)
    (check-equal? (quiet (lambda () (compressed->functional bank))) original)
    (define uncompressed (quiet (lambda () (compressed->registers bank))))
    (check-equal? (decode-quiet register-engine uncompressed) original))
  original)

;; This classifier is deliberately independent of compressed-step-span.
;; Only the atomic arm has a length-three original transition sequence.
(define (atomic-input original)
  (match original
    [(f:Call 'eval/d (list goal state _ _ _))
     (match goal
       [`(∃ ,_ ,_ ,_) #f]
       [`(,_ ∧ ,_ ,_) #f]
       [`(,_ ∨ ,_ ,_) #f]
       [`(suspend ,_ ,_) #f]
       [_ (list goal state)])]
    [_ #f]))

(define (prescription engine original)
  (match original
    [(f:Halted _) '()]
    [(f:Call pc _)
     (cond
       [(and (eq? (Engine-name engine) 'compressed) (atomic-input original))
        (list (OriginalStep '(eval/d) "eval-atom")
              (OriginalStep '(outcome/d) #f)
              (OriginalStep '(failure/d success/d) #f))]
       [else (list (OriginalStep (list pc) (functional-step-label original)))])]))

(define (check-source-edge before after label)
  (define source (whole-readback before))
  (define target (whole-readback after))
  (match label
    [#f (check-equal? target source)]
    [_
     (check-equal? (apply-reduction-relation/tag-with-names retained-red source)
                   (list (list label target)))]))

;; Consume exactly the stated number of old edges. A target is never used to
;; decide whether to stop, and the atom is evaluated only by the real machines.
(define (original-span original steps [work '()])
  (match steps
    ['() (values original work)]
    [(cons descriptor rest)
     (check-original original)
     (match-define (f:Call pc _) original)
     (check-not-false (member pc (OriginalStep-pcs descriptor)))
     (check-equal? (functional-step-label original) (OriginalStep-label descriptor))
     (define observed (observe (lambda () (f:step original))))
     (define next (Observation-value observed))
     (check-source-edge original next (OriginalStep-label descriptor))
     (original-span next rest (append work (Observation-work observed)))]))

(define (check-dispatch engine bank)
  (define original (check-bank engine bank))
  (define prescribed (prescription engine original))
  (when (eq? (Engine-name engine) 'compressed)
    (check-equal? (quiet (lambda () (compressed-step-span bank))) prescribed))
  (define-values (expected expected-work) (original-span original prescribed))
  (define before-count ((Engine-steps engine) bank))
  (define observed (observe (lambda () ((Engine-step! engine) bank))))
  (check-equal? (Observation-value observed) (not (null? prescribed)))
  (check-equal? ((Engine-steps engine) bank)
                (+ before-count (if (null? prescribed) 0 1)))
  (check-equal? (check-bank engine bank) expected
                "dispatch endpoint equals the independently prescribed original span")
  (check-equal? (Observation-work observed) expected-work)
  (check-equal? expected-work
                (match (atomic-input original) [#f '()] [input (list input)])
                "a dispatch does exactly its one atomic operation, if any")
  (when (and (eq? (Engine-name engine) 'compressed) (atomic-input original))
    ;; This compression stops before continuation dispatch, including KCommit.
    (match-define (f:Call 'eval/d (list _ _ _ _ k)) original)
    (check-match expected (f:Call 'return/d (list _ (== k)))))
  (values expected-work (length prescribed)))

(define (checked-drive engine bank [fuel 100000] [reversed-work '()] [old-steps 0])
  (define current (check-bank engine bank))
  (match current
    [(f:Halted value)
     (define before (snapshot engine bank))
     (define-values (work count) (check-dispatch engine bank))
     (check-equal? (snapshot engine bank) before "halted dispatch is a complete no-op")
     (check-equal? work '())
     (check-equal? count 0)
     (Trial value (reverse reversed-work) ((Engine-steps engine) bank) old-steps)]
    [_
     (when (zero? fuel) (error 'checked-drive "fixture exhausted dispatch budget"))
     (define-values (work count) (check-dispatch engine bank))
     (checked-drive engine bank (sub1 fuel)
                    (append (reverse work) reversed-work) (+ old-steps count))]))

(define (check-operation original public-computations)
  (define reference (observe (first public-computations)))
  (define trials
    (for/list ([engine (in-list engines)]
               [public-compute (in-list (rest public-computations))])
      (define bank (quiet (lambda () ((Engine-from-machine engine) original))))
      (check-equal? ((Engine-steps engine) bank) 0)
      (check-equal? (decode-quiet engine bank) original)
      (define trial (checked-drive engine bank))
      (define actual (observe public-compute))
      (check-equal? (Observation-value actual) (Observation-value reference))
      (check-equal? (Observation-work actual) (Observation-work reference))
      (check-equal? (Trial-value trial) (Observation-value actual))
      (check-equal? (Trial-work trial) (Observation-work actual))
      ;; Reify complete data, including every still-unforced resumption body.
      (check-equal? (reify-frontier (Trial-value trial))
                    (reify-frontier (Observation-value reference)))
      trial))
  (check-equal? (Trial-dispatches (first trials)) (Trial-original-steps (first trials)))
  (check-equal? (Trial-original-steps (first trials)) (Trial-original-steps (second trials)))
  (check-equal? (- (Trial-dispatches (first trials)) (Trial-dispatches (second trials)))
                (* 2 (length (Observation-work reference))))
  (Observation-value reference))

(define (check-boundaries frontier [fuel 100])
  (when (zero? fuel) (error 'check-boundaries "unexpected unproductive fixture"))
  (define collected
    (check-operation
     (f:Call 'collect/d (list frontier '() (KDone)))
     (list (lambda () (f:collect-all frontier))
           (lambda () (r:collect-all frontier))
           (lambda () (c:collect-all frontier)))))
  (check-true (retained-observation? (reify-frontier collected)))
  (define advanced
    (check-operation
     (f:Call 'advance/d (list frontier '() (KDone)))
     (list (lambda () (f:resume-once frontier))
           (lambda () (r:resume-once frontier))
           (lambda () (c:resume-once frontier)))))
  (cond
    [(retained-observation? (reify-frontier frontier))
     (check-equal? advanced frontier)
     (check-equal? collected frontier)]
    [else (check-boundaries advanced (sub1 fuel))]))

(define initial-state '(state () () () (label "initial")))
(define yes '(succeed (label "yes")))
(define no '(fail (label "no")))
(define fresh '(∃ (x:y) (x:y =? (sym "new") (label "new")) (label "fresh-y")))
(define (owned-resumption body [binders '(x:x)])
  `((∃ ,binders (suspend ,body (label "pause")) (label "fresh-x"))
    ∨ ,no (label "choice")))
(define focused
  (append
   (for/list ([body (in-list
                    (list yes no `(,yes ∨ ,yes (label "more"))
                          `(,no ∨ ,yes (label "reuse-right"))
                          `(suspend ,yes (label "nested")) fresh
                          '(x:x =? (sym "old") (label "old"))
                          '(suspend (x:x =? (sym "old") (label "old"))
                                    (label "nested-old"))))])
     (owned-resumption body))
   (list (owned-resumption yes '())
         `(,(owned-resumption yes) ∧ ,fresh (label "fresh-after-return"))
         `(,(owned-resumption '(x:x =? (sym "old") (label "old")))
           ∧ ,fresh (label "delayed-bind"))
         `(,no ∧ (suspend ,yes (label "unreachable-delay")) (label "failed-bind")))))

(define (same-input? left right)
  (and (equal? (witness-goal left) (witness-goal right))
       (equal? (witness-owners left) (witness-owners right))
       (equal? (witness-state left) (witness-state right))))
(define cases
  (remove-duplicates
   (append validation-witnesses
           (for/list ([goal (in-list (append search-corpus focused))]
                      [index (in-naturals)])
             (witness (string->symbol (format "register-~a" index)) goal '(Owners)
                      initial-state "register and compression regression")))
   same-input?))

(define (replace-datum datum needle replacement)
  (cond
    [(equal? datum needle) replacement]
    [else
     (match datum
       [(cons first rest)
        (cons (replace-datum first needle replacement)
              (replace-datum rest needle replacement))]
       [_ datum])]))

(module+ test
  (for ([sample (in-list cases)])
    (test-case (format "register/compression correspondence: ~a" (witness-name sample))
      (define goal (witness-goal sample))
      (define owners (witness-owners sample))
      (define state (witness-state sample))
      (define initial (f:initial goal #:owners owners #:state state))
      (check-equal? (r:decode (r:initial goal #:owners owners #:state state)) initial)
      (check-equal? (c:decode (c:initial goal #:owners owners #:state state)) initial)
      (define frontier
        (check-operation
         initial
         (list (lambda () (f:run goal #:owners owners #:state state))
               (lambda () (r:run goal #:owners owners #:state state))
               (lambda () (c:run goal #:owners owners #:state state)))))
      (check-boundaries frontier)))

  (test-case "every admitted PC is dispatched, including the original outcome handlers"
    (for ([engine (in-list engines)])
      (check-equal?
       (sort (for/list ([(key _) (in-hash seen-controls)]
                        #:when (eq? (first key) (Engine-name engine)))
               (second key)) symbol<?)
       (sort (cons 'halt (map first (Engine-signatures engine))) symbol<?)))
    (for ([pc (in-list '(outcome/d failure/d success/d))])
      (check-false (hash-has-key? seen-controls (list 'compressed pc))))
    (for ([constructor (in-list '(struct:KCommit struct:KDisjLeft struct:KDisjRight
                                  struct:KBindHead struct:KBindTail struct:KMergeForced
                                  struct:KBindForced struct:REval struct:RMerge struct:RBind))])
      (check-true (hash-has-key? seen-constructors constructor))))

  (test-case "register writes retain old operands needed by later target slots"
    (define answer `(One (Owners) ,initial-state))
    (define original
      (f:Call 'return/d
              (list answer (KDisjLeft no initial-state '(Owners) '() (KCommit (KDone))))))
    (define expected
      (f:Call 'eval/d
              (list no initial-state '(Owners) '()
                    (KDisjRight answer '(Owners) '() (KCommit (KDone))))))
    (for ([engine (in-list engines)])
      (define bank ((Engine-from-machine engine) original))
      (define-values (work count) (check-dispatch engine bank))
      (check-equal? ((Engine-decode engine) bank) expected)
      (check-equal? work '())
      (check-equal? count 1)))

  (test-case "conversion preserves pending allocation and never forces a halted Delay"
    (define owned '(Owners (Owner (u:9 u:2) (label "existing"))))
    (define initial (f:initial fresh #:owners owned))
    (define partial `(More (Delay ,owned ,(REval fresh initial-state))))
    (for ([engine (in-list engines)])
      (define pending ((Engine-from-machine engine) initial))
      (check-equal? (decode-quiet engine pending) initial)
      (check-equal? ((Engine-steps engine) pending) 0)
      (define halted ((Engine-from-machine engine) (f:Halted partial)))
      (check-equal? (decode-quiet engine halted) (f:Halted partial))
      (check-equal? (quiet (lambda () ((Engine-drive! engine) halted #:fuel 0))) partial)
      (define-values (work count) (check-dispatch engine halted))
      (check-equal? work '())
      (check-equal? count 0)))

  (test-case "compressed decoding rejects the three removed handler PCs"
    (define k (KCommit (KDone)))
    (for ([original
           (in-list (list (f:Call 'outcome/d (list (Failure) (FEmpty '(Owners) k)
                                                  (SOne '(Owners) k)))
                          (f:Call 'failure/d (list (FEmpty '(Owners) k)))
                          (f:Call 'success/d (list (SOne '(Owners) k) initial-state))))])
      (check-equal? (r:decode (r:from-machine original)) original)
      (check-exn exn:fail? (lambda () (c:from-machine original)))))

  (test-case "fuel counts actual dispatches and preserves halted values at zero"
    (for ([engine (in-list engines)])
      (define initial (f:initial yes))
      (define trial (checked-drive engine ((Engine-from-machine engine) initial)))
      (define exact-budget (Trial-dispatches trial))
      (check-equal? ((Engine-drive! engine) ((Engine-from-machine engine) initial)
                     #:fuel exact-budget)
                    (Trial-value trial))
      (check-exn exn:fail:budget?
                 (lambda () ((Engine-drive! engine) ((Engine-from-machine engine) initial)
                              #:fuel (sub1 exact-budget))))
      (check-exn exn:fail:budget?
                 (lambda () ((Engine-drive! engine) ((Engine-from-machine engine) initial)
                              #:fuel 0)))
      (for ([bad (in-list '(-1 1/2 invalid))])
        (check-exn exn:fail?
                   (lambda () ((Engine-drive! engine) ((Engine-from-machine engine) initial)
                                #:fuel bad))))
      (check-equal? ((Engine-drive! engine)
                     ((Engine-from-machine engine) (f:Halted '(Done (Owners)))) #:fuel 0)
                    '(Done (Owners)))))

  (test-case "canonical unused slots and counters are enforced"
    (define bad-register (r:from-machine (f:Halted '(Done (Owners)))))
    (r:set-Registers-r4! bad-register 'stale)
    (check-exn exn:fail? (lambda () (r:decode bad-register)))
    (define bad-compressed (c:from-machine (f:Halted '(Done (Owners)))))
    (c:set-Registers-r2! bad-compressed 'stale)
    (check-exn exn:fail? (lambda () (c:decode bad-compressed)))
    (r:set-Registers-r4! bad-register #f)
    (c:set-Registers-r2! bad-compressed #f)
    (r:set-Registers-steps! bad-register -1)
    (c:set-Registers-steps! bad-compressed 1/2)
    (check-exn exn:fail? (lambda () (r:decode bad-register)))
    (check-exn exn:fail? (lambda () (c:decode bad-compressed))))

  (test-case "compression generation is fresh and rejects drift at each fused source site"
    (check-true (compress:check-generated!))
    (check-equal? (length r:signatures) 13)
    (check-equal? (length c:signatures) 10)
    (define original (derive:control-definitions))
    (define transformed (compress:compress-control-definitions original))
    (for ([definition (in-list original)])
      (match-define `(define (,name ,_ ...) ,_ ...) definition)
      (unless (member name '(eval/d outcome/d failure/d success/d))
        (check-not-false (member definition transformed)
                        "every other control definition is literally preserved")))
    (for ([mutation
           (in-list
            '((eval/d (FEmpty owners k) (FEmpty (quote (Owners)) k))
              (outcome/d (failure/d failure) (success/d failure state))
              (failure/d Empty Done)
              (success/d One Solo)))])
      (match-define (list target needle replacement) mutation)
      (define mutated
        (for/list ([definition (in-list original)])
          (match-define `(define (,name ,_ ...) ,_ ...) definition)
          (if (eq? name target)
              (replace-datum definition needle replacement)
              definition)))
      (check-not-equal? mutated original)
      (check-exn #rx"needs review"
                 (lambda () (compress:compress-control-definitions mutated))))))
