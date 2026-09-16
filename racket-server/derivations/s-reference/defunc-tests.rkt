#lang racket

(require rackunit
         "data.rkt"
         (prefix-in direct: "interpreter.rkt")
         (prefix-in cps: "cps.rkt")
         (prefix-in defunc: "defunc.rkt")
         (prefix-in machine: "machine.rkt")
         (prefix-in data: "readback.rkt")
         (prefix-in inspection: "inspection.rkt")
         (only-in "derive.rkt" check-generated!)
         "source.rkt"
         "../shared/kernel-equations.rkt"
         "../test-support/witnesses.rkt"
         "../test-support/corpus.rkt")

;; This compares the functional stages using only their public operations.
;; The separate machine-correspondence suite checks each control transition.
(struct Runner (name run advance collect reify) #:transparent)
(struct Result (value image work) #:transparent)

(define (observe runner compute)
  (define reversed '())
  (define value
    (parameterize ([current-atomic-observer
                    (lambda (goal state)
                      (set! reversed (cons (list goal state) reversed)))])
      (compute)))
  (Result value ((Runner-reify runner) value) (reverse reversed)))

(define (check-results results)
  (match-define (cons expected rest) results)
  (for ([actual (in-list rest)])
    (check-equal? (Result-image actual) (Result-image expected)
                  "exact Frontier including every unforced resumption body")
    (check-equal? (Result-work actual) (Result-work expected)
                  "actual atomic work and incoming logical states")))

(define (check-boundaries runners results [fuel 100])
  (when (zero? fuel) (error 'check-boundaries "fixture exceeded public boundary budget"))
  (check-results results)
  (define advanced
    (for/list ([runner (in-list runners)] [result (in-list results)])
      (observe runner (lambda () ((Runner-advance runner) (Result-value result))))))
  (check-results advanced)
  (cond
    [(retained-observation? (Result-image (first results)))
     (for ([before (in-list results)] [after (in-list advanced)])
       (check-equal? (Result-image after) (Result-image before))
       (check-equal? (Result-work after) '()))]
    [else (check-boundaries runners advanced (sub1 fuel))]))

(define state '(state () () () (label "initial")))
(define yes '(succeed (label "yes")))
(define no '(fail (label "no")))
(define fresh '(∃ (x:y) (x:y =? (sym "new") (label "new")) (label "fresh-y")))
(define (owned body [binders '(x:x)])
  `((∃ ,binders (suspend ,body (label "pause")) (label "fresh-x"))
    ∨ ,no (label "choice")))
(define focused
  (append
   (for/list ([body (in-list (list no yes `(,yes ∨ ,yes (label "two"))
                                   `(,no ∨ ,yes (label "saved-right"))
                                   `(suspend ,yes (label "nested")) fresh
                                   '(x:x =? (sym "old") (label "old"))
                                   '(suspend (x:x =? (sym "old") (label "old"))
                                             (label "nested-old"))))])
     (owned body))
   (list (owned yes '())
         `(,(owned yes) ∧ ,fresh (label "fresh-after"))
         `(,(owned '(x:x =? (sym "old") (label "old")))
           ∧ ,fresh (label "bind-old")))))

(module+ test
  (define samples
    (append validation-witnesses
            (for/list ([goal (in-list (remove-duplicates (append search-corpus focused)))]
                       [index (in-naturals)])
              (witness (string->symbol (format "defunc-~a" index)) goal '(Owners) state
                       "defunctionalization boundary comparison"))))
  (for ([sample (in-list samples)])
    (test-case (format "direct/CPS/data/machine: ~a" (witness-name sample))
      (define descriptions (make-weak-hasheq))
      (define (functional-image value) (inspection:reify-frontier descriptions value))
      (define runners
        (list (Runner 'direct direct:run direct:resume-once direct:collect-all functional-image)
              (Runner 'cps cps:run cps:resume-once cps:collect-all functional-image)
              (Runner 'defunc defunc:run defunc:resume-once defunc:collect-all data:reify-frontier)
              (Runner 'machine machine:run machine:resume-once machine:collect-all data:reify-frontier)))
      (parameterize ([direct:current-closure-observer
                      (lambda (family procedure captures)
                        (inspection:record-closure! descriptions family procedure captures))])
        (define initial-results
          (for/list ([runner (in-list runners)])
            (observe runner
                     (lambda () ((Runner-run runner) (witness-goal sample)
                                 #:owners (witness-owners sample)
                                 #:state (witness-state sample))))))
        (check-boundaries runners initial-results)
        (define collected
          (for/list ([runner (in-list runners)] [result (in-list initial-results)])
            (observe runner (lambda () ((Runner-collect runner) (Result-value result))))))
        (check-results collected)
        (check-true (retained-observation? (Result-image (first collected)))))))

  (test-case "data outcomes are produced directly and settled answers cannot enter bind"
    (check-true (Failure? (atomic/data no state)))
    (check-true (Success? (atomic/data yes state)))
    (for ([frontier (in-list
                    (list `(Solo (Owners) ,state)
                          `(Emit (Owners) (Answer (Owners) ,state) (Done (Owners)))))])
      (check-exn exn:fail?
                 (lambda ()
                   (defunc:bind/d frontier (GRight yes) '(Owners) '() (KDone))))))

  (test-case "generated trampoline has an explicit transition budget"
    (check-true (check-generated!))
    (check-exn exn:fail?
               (lambda () (machine:drive (machine:initial yes) #:fuel 0)))
    (check-exn exn:fail?
               (lambda () (machine:drive (machine:initial yes) #:fuel -1)))
    (check-equal? (machine:drive (machine:Halted '(Done (Owners))) #:fuel 0)
                  '(Done (Owners)))))
