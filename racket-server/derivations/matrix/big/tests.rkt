#lang racket

(require rackunit redex/reduction-semantics
         "s.rkt" "e.rkt" "n.rkt" "features.rkt" "../features.rkt" "maps.rkt"
         "../source-s.rkt" "../source-e.rkt" "../source-n.rkt"
         "../../shared/maps.rkt" "../../test-support/corpus.rkt"
         (prefix-in w: "../../test-support/witnesses.rkt")
         "../../shared/stages/schema.rkt" "../stages/instances.rkt")

(define (check-row stage computation evaluate promote raw source-trace)
  (match-define (list value labels) (evaluate computation))
  ;; Raw proof multiplicity matters, including for value/context overlaps.
  ;; judgment-holds alone deduplicates identical conclusions.
  (check-equal? (length (raw computation)) 1)
  (check-equal? (promote computation) value)
  (define source-edges (source-trace computation))
  (check-equal? labels (map first source-edges))
  (check-equal? value (if (null? source-edges) computation (second (last source-edges))))
  (define initial (initial-B stage computation))
  (define b-edges (b-trace stage initial))
  (check-equal? labels (append-map (lambda (edge) (semantic-labels (first edge))) b-edges))
  (check-equal? value (readback-B (if (null? b-edges) initial (second (last b-edges)))))
  ;; Every finite B edge carries its independently replayable exact M span.
  (for ([before (in-list (cons initial (map second b-edges)))]
        [edge (in-list b-edges)])
    (match-define (list span after) edge)
    (check-equal? (replay-span stage (decode-BM before) span) (decode-BM after)))
  value)

(define (relation-trace relation computation [reversed '()])
  (match (apply-reduction-relation/tag-with-names relation computation)
    ['() (reverse reversed)]
    [(list (and edge (list _ next))) (relation-trace relation next (cons edge reversed))]
    [other (error 'relation-trace "nonunique source proof: ~e" other)]))

(define search-rows
  (list (list S evaluate/s promote/s raw-derivations/s strict-s-red)
        (list E evaluate/e promote/e raw-derivations/e strict-e-red)
        (list N evaluate/n promote/n raw-derivations/n strict-n-red)))
(define core-rows
  (list (list SCore evaluate/s-core promote/s-core raw-derivations/s-core strict-s-core-red)
        (list ECore evaluate/e-core promote/e-core raw-derivations/e-core strict-e-core-red)
        (list NCore evaluate/n-core promote/n-core raw-derivations/n-core strict-n-core-red)))
(define delay-rows
  (list (list SDelay evaluate/s-delay promote/s-delay raw-derivations/s-delay strict-s-delay-red)
        (list EDelay evaluate/e-delay promote/e-delay raw-derivations/e-delay strict-e-delay-red)
        (list NDelay evaluate/n-delay promote/n-delay raw-derivations/n-delay strict-n-delay-red)))
(define disjunction-rows
  (list (list SDisjunction evaluate/s-disjunction promote/s-disjunction raw-derivations/s-disjunction strict-s-disjunction-red)
        (list EDisjunction evaluate/e-disjunction promote/e-disjunction raw-derivations/e-disjunction strict-e-disjunction-red)
        (list NDisjunction evaluate/n-disjunction promote/n-disjunction raw-derivations/n-disjunction strict-n-disjunction-red)))

(define (check-three computation [rows search-rows])
  (define e-computation (Q-SE computation))
  (define n-computation (Q-SN computation))
  (match-define
    (list s-final e-final n-final)
    (for/list ([row (in-list rows)] [input (in-list (list computation e-computation n-computation))])
      (match-define (list stage evaluate promote raw relation) row)
      (check-row stage input evaluate promote raw (lambda (c) (relation-trace relation c)))))
  (check-equal? (Q-SE s-final) e-final)
  (check-equal? (Q-SN s-final) n-final)
  (check-equal? (Q-EN e-final) n-final)
  ;; Structural Q_Big maps visit every premise query and mature result,
  ;; including S inherited owner prefixes and strict saved operand chunks.
  (match-define
    (list s-proof e-proof n-proof)
    (for/list ([row (in-list rows)] [input (in-list (list computation e-computation n-computation))])
      (certify/raw (fourth row) input)))
  (check-equal? (QBig-SE s-proof) e-proof)
  (check-equal? (QBig-EN e-proof) n-proof)
  (check-equal? (QBig-SN s-proof) n-proof)
  (check-equal? (QBig-SN s-proof) (QBig-EN (QBig-SE s-proof)))
  n-final)

(module+ test
  (for ([coordinate
         (in-list (list (list 'core core-corpus core-rows) (list 'delay delay-corpus delay-rows)
                        (list 'disjunction disjunction-corpus disjunction-rows)
                        (list 'search search-corpus search-rows)))])
    (match-define (list feature goals rows) coordinate)
    (for ([goal (in-list goals)] [index (in-naturals)])
      (test-case (format "~a S/E/N Big, fixed point, finite B witness ~a" feature index)
        (void (check-three `(render ,(s-initial goal)) rows))
        (void (check-three (s-initial goal) rows)))))

  (test-case "lower-coordinate Big domains reject absent goals and control constructors"
    (define delay-goal '(suspend (succeed (label "body")) (label "delay")))
    (define choice-goal '((succeed (label "left")) ∨ (succeed (label "right")) (label "choice")))
    (for ([rows (in-list (list core-rows delay-rows disjunction-rows))]
          [forbidden (in-list (list (list delay-goal choice-goal) (list choice-goal) (list delay-goal)))])
      (for ([goal (in-list forbidden)])
        (define s-input `(render ,(s-initial goal)))
        (for ([row (in-list rows)] [input (in-list (list s-input (Q-SE s-input) (Q-SN s-input)))])
          (match-define (list _ evaluate promote raw _) row)
          (for ([run (in-list (list evaluate promote raw))])
            (check-exn exn:fail? (lambda () (run input))))))))

  (test-case "exact Big premises at intermediate nested rail source states"
    (define initial `(render ,(s-initial nested-rail-goal)))
    (for ([state (in-list (cons initial (map second (s-trace initial))))])
      (void (check-three state))))

  (define state '(state () () () (label "initial")))
  (define fresh
    '(∃ (x:q) (x:q =? (sym "fresh") (label "use")) (label "fresh")))
  (define shared '(Owners (Owner (u:0 u:9) (label "shared"))))
  (define head-only '(Owners (Owner (u:1) (label "head-only"))))
  (define pending `(eval (Owners) ,fresh ,state))
  (define mature `(One (Owners) ,state))

  (test-case "S ancestor support affects allocation; sibling/head support does not"
    (define computations
      (list
       `(render (Yield ,shared (Answer ,head-only ,state) ,pending))
       `(render (mplus ,shared (Delay ,head-only ,pending) ,pending))
       `(render (bind ,shared
                      (Yield (Owners) (Answer (Owners) ,state) ,mature) ,fresh))
       `(Emit ,shared (Answer ,head-only ,state) (Forced (Owners) (render ,pending)))
       `(render (Delay ,shared ,pending))))
    (for ([computation (in-list computations)])
      (void (check-three computation)))
    ;; The same focused computation gets a different first fresh name when
    ;; its inherited world already owns u:0. This context is explicit input.
    (match-define (list result labels) (evaluate/s pending '(u:0)))
    (check-equal? result
                  '(One (Owners (Owner (u:1) (label "fresh")))
                        (state ((u:1 (sym "fresh"))) ()
                               ((u:1 =? (sym "fresh") (label "use"))) (label "initial"))))
    (check-equal? labels '("allocate-fresh" "eval-atom"))
    (check-equal? (promote/s pending '(u:0)) result)
    (check-equal? (length (raw-derivations/s pending '(u:0))) 1))

  (test-case "mature versus pending Yield, Emit and Forced have one raw proof"
    (for ([computation
           (in-list
            (list `(Yield ,shared (Answer ,head-only ,state) ,mature)
                  `(Yield ,shared (Answer ,head-only ,state) ,pending)
                  `(Emit ,shared (Answer ,head-only ,state) (Done (Owners)))
                  `(Emit ,shared (Answer ,head-only ,state) (render ,pending))
                  `(Forced ,shared (Done (Owners)))
                  `(Forced ,shared (render ,pending))
                  `(Delay ,shared ,pending)))])
      (void (check-three computation))))

  (test-case "sparse E support is preserved until explicit positional addressing"
    (define computation
      `(render (eval ,fresh (state (Support u:9 u:2 u:7) () () () (label "sparse")))))
    (define e-result (check-row E computation evaluate/e promote/e raw-derivations/e e-trace))
    (define n-result
      (check-row N (Q-EN computation) evaluate/n promote/n raw-derivations/n n-trace))
    (check-equal? (Q-EN e-result) n-result)))

(define (check-frontier-rounds frontier [rows search-rows] [fuel 1000])
  ;; A partial frontier is itself a finite Big endpoint: obtaining its proof
  ;; must not enter a suspended tip. Test the full certificate, not just shape.
  (void (check-three frontier rows))
  (void (check-three `(collect ,frontier) rows))
  (define next (s-run `(advance ,frontier)))
  (void (check-three `(advance ,frontier) rows))
  (if (s-observation? frontier)
      (check-equal? next frontier)
      (begin
        (when (zero? fuel) (error 'check-frontier-rounds "frontier boundary fuel exhausted"))
        (check-frontier-rounds next rows (sub1 fuel)))))

(module+ test
  (for ([w (in-list w:validation-witnesses)])
    (test-case (format "native commit/advance/collect Big and S/E/N certificates: ~a"
                       (w:witness-name w))
      (define query `(commit ,(w:witness-initial w)))
      (void (check-three query))
      (define frontier (s-run query))
      (check-true (s-frontier? frontier))
      (check-frontier-rounds frontier)
      (check-equal? (s-run `(collect ,frontier))
                    (s-run `(render ,(w:witness-initial w))))))

  (test-case "commit stops without forcing; advance and collect have distinct exact labels"
    (define query
      '(commit (eval (Owners) (suspend (succeed (label "body")) (label "delay"))
                     (state () () () (label "initial")))))
    (match-define (list frontier labels) (evaluate/s query))
    (check-equal? labels '("eval-suspend" "commit-delay"))
    (check-equal? frontier
                  '(More (Delay (Owners) (eval (Owners) (succeed (label "body"))
                                               (state () () () (label "initial"))))))
    (match-define (list completed advance-labels) (evaluate/s `(advance ,frontier)))
    (check-equal? advance-labels '("advance-delay" "eval-atom" "commit-one"))
    (check-true (s-observation? completed))
    (check-equal? (second (evaluate/s `(advance ,completed))) '("advance-forced" "advance-solo"))
    (check-equal? (second (evaluate/s `(collect ,frontier)))
                  '("collect-delay" "eval-atom" "commit-one" "collect-solo"))
    (void (check-three `(collect (advance ,query)))))

  (test-case "internal force retains saved owners on the active root before allocation"
    (define owners '(Owners (Owner (u:0 u:9) (label "saved"))))
    (define body
      '(eval (Owners) (∃ (x:q) (suspend (x:q =? (sym "fresh") (label "use"))
                                         (label "inner-delay")) (label "fresh"))
                     (state () () () (label "initial"))))
    (match-define `(eval (Owners) ,goal ,state) body)
    (define pending `(eval ,owners ,goal ,state))
    (define forced `(force (Delay ,owners ,body)))
    (for ([rows (in-list (list delay-rows search-rows))])
      (void (check-three pending rows))
      (void (check-three forced rows))
      (void (check-three `(collect (commit ,forced)) rows)))
    (match-define (list result labels) (evaluate/s forced))
    (check-equal? labels '("force-delay" "allocate-fresh" "eval-suspend"))
    ;; Saved and newly allocated Owners remain on the running eval and then
    ;; its suspension. Returning that suspension never enters its body.
    (check-equal? result
                  '(Delay (Owners (Owner (u:0 u:9) (label "saved"))
                                  (Owner (u:1) (label "fresh")))
                          (eval (Owners) (u:1 =? (sym "fresh") (label "use"))
                                (state () () () (label "initial")))))
    (define proof (certify/raw raw-derivations/s forced))
    (match-define (list operand-proof body-proof) (BigCertificate-premises proof))
    (check-equal? (BigCertificate-labels operand-proof) '())
    (check-equal? (BigCertificate-prefix body-proof) '())
    (check-equal? (BigCertificate-input body-proof) pending)
    (check-equal? (BigCertificate-labels body-proof) '("allocate-fresh" "eval-suspend")))

  (test-case "force certificate retains common scope across bind, choice, and nested force roots"
    (define saved '(Owners (Owner (u:0 u:9) (label "saved"))))
    (define local '(Owners (Owner (u:2) (label "local"))))
    (define combined
      '(Owners (Owner (u:0 u:9) (label "saved")) (Owner (u:2) (label "local"))))
    (define state '(state () () () (label "initial")))
    (define goal '(∃ (x:q) (succeed (label "unused")) (label "fresh")))
    (define pending `(eval (Owners) ,goal ,state))
    (define roots
      (list
       (list `(bind ,local (One (Owners) ,state) ,goal)
             `(bind ,combined (One (Owners) ,state) ,goal))
       (list `(mplus ,local ,pending ,pending)
             `(mplus ,combined ,pending ,pending))
       (list `(force (Delay ,local ,pending))
             `(force (Delay ,combined ,pending)))))
    (for ([pair (in-list roots)])
      (match-define (list body retained) pair)
      (define query `(force (Delay ,saved ,body)))
      (void (check-three query))
      (void (check-three `(collect (commit ,query))))
      (define proof (certify/raw raw-derivations/s query))
      (match-define (list _ body-proof) (BigCertificate-premises proof))
      (check-equal? (BigCertificate-input body-proof) retained)
      (check-equal? (BigCertificate-prefix body-proof) '())
      (check-equal? (apply-reduction-relation/tag-with-names strict-s-red query)
                    (list (list "force-delay" retained)))))

  (test-case "delayed bind and public render resume stored computation directly"
    (define state '(state () () () (label "initial")))
    (define body `(eval (Owners) (succeed (label "body")) ,state))
    (define continuation '(succeed (label "continuation")))
    (define query `(bind (Owners) (Delay (Owners) ,body) ,continuation))
    (match-define (list result labels) (evaluate/s query))
    (check-equal? result `(Delay (Owners) (bind (Owners) ,body ,continuation)))
    (check-equal? labels '("bind-delay"))
    (for ([rows (in-list (list delay-rows search-rows))])
      (void (check-three query rows))
      (void (check-three `(render ,query) rows)))
    (check-equal? (second (evaluate/s `(render ,query)))
                  '("bind-delay" "render-delay" "eval-atom" "bind-one"
                                  "eval-atom" "render-one")))

  (test-case "obsolete prefix control is absent from every Big feature instance"
    (define inputs
      '((prefix (Owners) (One (Owners) (state () () () (label "initial"))))
        (prefix (One (state (Support) () () () (label "initial"))))
        (prefix (One (state 0 () () () (label "initial"))))))
    (for ([rows (in-list (list core-rows delay-rows disjunction-rows search-rows))])
      (for ([row (in-list rows)]
            [query (in-list inputs)])
        (match-define (list _ evaluate promote raw _) row)
        (for ([run (in-list (list evaluate promote raw))])
          (check-exn exn:fail? (lambda () (run query)))))))

  (test-case "all twelve Big feature instances admit native frontier operations"
    (for ([rows (in-list (list core-rows delay-rows disjunction-rows search-rows))]
          [name (in-list '(unused-binder allocation-across-delay eager-bind-residual nested-rail))])
      (define w
        (findf (lambda (w) (eq? (w:witness-name w) name)) w:validation-witnesses))
      (define query `(commit ,(w:witness-initial w)))
      (void (check-three query rows))
      (check-frontier-rounds (s-run query) rows))))
