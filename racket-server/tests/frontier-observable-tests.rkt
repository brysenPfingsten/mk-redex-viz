#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         (prefix-in red:
                    "../src/search-lattice/reduction-relations/all.rkt")
         "./frontier-observable-support.rkt"
         "./search-lattice-support.rkt")

(provide FRONTIER-OBSERVABLES)

(define TRACE-CAP 48)

(define (count-step-name steps expected [count 0])
  (match steps
    ['() count]
    [(cons step-name rest)
     (count-step-name rest
                      expected
                      (if (or (string=? step-name expected)
                              (and (string=? expected "fresh-substitute")
                                   (string-prefix? "fresh-substitute"
                                                   step-name)))
                          (add1 count)
                          count))]))

(define (named-step succ*)
  (match (remove-duplicates succ*)
    [(list (list name cfg))
     (values (~a name) cfg)]
    [_ (error 'named-step "expected exactly one tagged successor, got ~e" succ*)]))

(define scoped-delay-fresh
  (term ((∃ (x:0)
            (suspend (x:0 =? (sym "cat") (label "eq"))
                     (label "zz"))
            (label "ex"))
         (state () () () () (label "s")))))

(define/provide-test-suite FRONTIER-OBSERVABLES
  (test-case "Freshened scope stays exact when fresh and delay interact"
    (for ([entry (in-list (list red:search-early-red
                                red:search-late-red))])
      (define-values (steps final-cfg status)
        (trace-deterministic entry scoped-delay-fresh TRACE-CAP))
      (check-equal? status 'done)
      (check-true (config-c-scope-agreement? final-cfg))
      (check-true (config-exact-scope? final-cfg))
      (check-equal? (count-step-name steps "fresh-substitute") 1)
      (check-true (>= (count-freshened final-cfg) 1))
      (check-equal? (count-step-name steps "invoke-delay") 1)
      (check-equal? (count-bounced final-cfg) 1)))

  (test-case "delay pops do not escape their Freshened wrapper"
    (define scoped-delay
      (term (ScopedTree
             (u:0)
             (delay ((succeed (label "ok"))
                     (state () () (u:0) () (label "s"))))
             (label "fresh"))))
    (for ([rel (in-list (list red:delay-red
                              red:search-early-red
                              red:search-late-red))])
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel scoped-delay)))
      (check-equal? step-name "invoke-delay")
      (check-equal?
       next
       (term (ScopedShell
              (u:0)
              (Deferred
               ((succeed (label "ok"))
                (state () () (u:0) () (label "s"))))
              (label "fresh"))))
      (check-true (config-c-scope-agreement? next))
      (check-true (config-exact-scope? next))))

  (test-case "Deferred accounting matches invoke-delay steps across representative machines"
    (for ([entry
           (in-list
            (list (list "delay" red:delay-red cfg-delay-goal)
                  (list "search-early" red:search-early-red cfg-delay-goal)
                  (list "search-late" red:search-late-red cfg-delay-goal)
                  (list "relcall" red:relcall-red cfg-call)
                  (list "search-dfs-early-relcall" red:search-dfs-early-relcall-red cfg-call)
                  (list "search-dfs-late-relcall" red:search-dfs-late-relcall-red cfg-call)
                  (list "search-flip-early-relcall" red:search-flip-early-relcall-red cfg-call)
                  (list "search-flip-late-relcall" red:search-flip-late-relcall-red cfg-call)
                  (list "search-dfs-early" red:search-dfs-early-red cfg-flip)
                  (list "search-dfs-late" red:search-dfs-late-red cfg-flip)
                  (list "search-flip-early" red:search-flip-early-red cfg-flip)
                  (list "search-flip-late" red:search-flip-late-red cfg-flip)
                  (list "rail-early" red:rail-early-red cfg-rail)
                  (list "rail-late" red:rail-late-red cfg-rail)
                  (list "rail-early-relcall" red:rail-early-relcall-red cfg-call-rail)
                  (list "rail-late-relcall" red:rail-late-relcall-red cfg-call-rail)))])
      (match-define (list label rel cfg) entry)
      (define-values (steps final-cfg status)
        (trace-deterministic rel cfg TRACE-CAP))
      (check-true (or (eq? status 'done)
                      (eq? status 'cap)))
      (check-true (config-c-scope-agreement? final-cfg))
      (check-true (config-exact-scope? final-cfg))
      (check-equal? (count-bounced final-cfg)
                    (count-step-name steps "invoke-delay")
                    label)
      (check-true (>= (count-bounced final-cfg) 1)
                  label))))

(module+ test
  (run-tests FRONTIER-OBSERVABLES))
