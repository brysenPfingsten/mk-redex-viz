#lang racket

(require rackunit
         rackunit/text-ui
         (prefix-in premachine: "../premachine/main.rkt")
         (prefix-in cfree: "../cfree/main.rkt")
         (prefix-in zipper: "../zipper/main.rkt")
         "../bridge/main.rkt"
         "../corpus.rkt")

(provide refocusing-tests)

(define (trace/final stepper cfg [limit 128])
  (define-values (_steps final-cfg status)
    (stepper cfg limit))
  (check-equal? status 'value)
  final-cfg)

(define (premachine-final label)
  (trace/final premachine:trace (premachine:parse-example label)))

(define (cfree-final label)
  (trace/final cfree:trace (cfree:parse-example label)))

(define (zipper-final label)
  (trace/final zipper:trace (zipper:parse-example label)))

(define (current-trace cfg [limit 128] [steps '()] [cfgs (list cfg)])
  (match (current-step cfg)
    [#f
     (values (reverse steps) cfg 'value (reverse cfgs))]
    [_ #:when (zero? limit)
       (values (reverse steps) cfg 'cap (reverse cfgs))]
    [(list name next)
     (current-trace next
                    (sub1 limit)
                    (cons name steps)
                    (cons next cfgs))]))

(define (current-final-from-cfree label)
  (define initial-current
    (cfree->current-c-machine (cfree:parse-example label)))
  (define-values (_steps final-cfg status _cfgs)
    (current-trace initial-current))
  (check-equal? status 'value)
  final-cfg)

(define (observables-by-artifact label)
  (define premachine-final-cfg
    (premachine-final label))
  (define zipper-final-machine
    (zipper-final label))
  (define cfree-final-cfg
    (cfree-final label))
  (define current-final-cfg
    (current-final-from-cfree label))
  (list (premachine:answers premachine-final-cfg)
        (zipper:answers zipper-final-machine)
        (cfree:answers cfree-final-cfg)
        (project-observable current-final-cfg)))

(define (deterministic-step? stepper cfg)
  (<= (length (stepper cfg)) 1))

(define (normalize-answers answers*)
  (sort (remove-duplicates answers*)
        string<?
        #:key ~s))

(define refocusing-tests
  (test-suite
   "refocusing"

   (test-case
    "artifact boundaries use real structs"
    (define premachine-cfg
      (premachine:parse-example "simple unify"))
    (define cfree-cfg
      (cfree:parse-example "simple unify"))
    (define zipper-machine
      (zipper:parse-example "simple unify"))
    (check-true (premachine:premachine-config? premachine-cfg))
    (check-true (cfree:cfree-config? cfree-cfg))
    (check-true (zipper:machine? zipper-machine))
    (check-equal? (zipper:machine->cfg (zipper:cfg->machine premachine-cfg))
                  premachine-cfg)
    (check-true (cfree:cfree-config? (premachine->cfree premachine-cfg)))
    (check-true (zipper:machine? (premachine->zipper premachine-cfg)))
    (check-true (cfree:cfree-config? (zipper->cfree zipper-machine))))

   (test-case
    "each artifact is deterministic on the shared corpus"
    (for ([label (in-list example-labels)])
      (check-true
       (deterministic-step? premachine:step
                            (premachine:parse-example label))
       label)
      (check-true
       (deterministic-step? cfree:step
                            (cfree:parse-example label))
       label)
      (check-true
       (deterministic-step? zipper:step
                            (zipper:parse-example label))
       label)
      (match (current-step (cfree->current-c-machine (cfree:parse-example label)))
        [#f (void)]
        [(list _name _next) (void)])))

   (test-case
    "all artifacts agree on final answer sets"
    (for ([label (in-list example-labels)])
      (match-define (list premachine-ans zipper-ans cfree-ans current-ans)
        (observables-by-artifact label))
      (check-equal? (normalize-answers premachine-ans)
                    (normalize-answers zipper-ans)
                    label)
      (check-equal? (normalize-answers premachine-ans)
                    (normalize-answers cfree-ans)
                    label)
      (check-equal? (normalize-answers premachine-ans)
                    (normalize-answers current-ans)
                    label)))

   (test-case
    "zipper steps agree with premachine decompose-contract-plug"
    (for ([label (in-list example-labels)])
      (let check ([cfg (premachine:parse-example label)]
                  [machine (premachine->zipper (premachine:parse-example label))])
        (match (premachine:step cfg)
          ['()
           (check-equal? (zipper:machine->cfg machine) cfg)]
          [(list (list name next-cfg))
           (match-define (list (list zipper-name next-machine))
             (zipper:step machine))
           (check-equal? zipper-name name label)
           (check-equal? (zipper:machine->cfg machine) cfg label)
           (check-equal? (zipper:machine->cfg next-machine) next-cfg label)
           (check-equal? (zipper->cfree next-machine)
                         (premachine->cfree next-cfg)
                         label)
           (check next-cfg next-machine)]))))

   (test-case
    "erase-c and restore-c round-trip on well-formed current traces"
    (for ([label (in-list example-labels)])
      (define initial-current
        (cfree->current-c-machine (cfree:parse-example label)))
      (define-values (_steps _final _status cfgs)
        (current-trace initial-current))
      (for ([cfg (in-list cfgs)])
        (check-true (current-c-scope-agrees? cfg) label)
        (check-equal? (cfree->current-c-machine (erase-c cfg))
                      cfg
                      label))))

   (test-case
    "c-free and current endpoints agree on each final observable"
    (for ([label (in-list example-labels)])
      (define cfree-final-cfg
        (cfree-final label))
      (define current-final-cfg
        (current-final-from-cfree label))
      (check-true
       (same-observable? (cfree->current-c-machine cfree-final-cfg)
                         current-final-cfg)
       label)))

   (test-case
    "current machine c matches ambient scope at every bridged step"
    (for ([label (in-list example-labels)])
      (define initial-current
        (cfree->current-c-machine (cfree:parse-example label)))
      (define-values (_steps _final _status cfgs)
        (current-trace initial-current))
      (for ([cfg (in-list cfgs)])
        (check-true (current-c-scope-agrees? cfg) label))))))

(module+ test
  (run-tests refocusing-tests))
