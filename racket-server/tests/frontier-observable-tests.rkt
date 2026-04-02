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
                              (and (string=? expected "core/fresh-substitute")
                                   (string-prefix? "core/fresh-substitute"
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
    (for ([entry (in-list (list red:search-base-seq-red
                                red:search-base-fused-red))])
      (define-values (steps final-cfg status)
        (trace-deterministic entry scoped-delay-fresh TRACE-CAP))
      (check-equal? status 'done)
      (check-true (config-c-scope-agreement? final-cfg))
      (check-true (config-exact-scope? final-cfg))
      (check-equal? (count-step-name steps "core/fresh-substitute") 1)
      (check-true (>= (count-freshened final-cfg) 1))
      (check-equal? (count-step-name steps "delay/invoke-delay") 1)
      (check-equal? (count-bounced final-cfg) 1)))

  (test-case "delay pops do not escape their Freshened wrapper"
    (define scoped-delay
      (term (FreshenedTree
             (u:0)
             (delay ((succeed (label "ok"))
                     (state () () (u:0) () (label "s"))))
             (label "fresh"))))
    (for ([rel (in-list (list red:delay-red
                              red:search-base-seq-red
                              red:search-base-fused-red))])
      (define-values (step-name next)
        (named-step (apply-reduction-relation/tag-with-names rel scoped-delay)))
      (check-equal? step-name "delay/invoke-delay")
      (check-equal?
       next
       (term (FreshenedShell
              (u:0)
              (Bounced
               ((succeed (label "ok"))
                (state () () (u:0) () (label "s"))))
              (label "fresh"))))
      (check-true (config-c-scope-agreement? next))
      (check-true (config-exact-scope? next))))

  (test-case "Bounced accounting matches invoke-delay steps across representative machines"
    (for ([entry
           (in-list
            (list (list "delay" red:delay-red cfg-delay-goal)
                  (list "search-base-seq" red:search-base-seq-red cfg-delay-goal)
                  (list "search-base-fused" red:search-base-fused-red cfg-delay-goal)
                  (list "calls" red:calls-red cfg-call)
                  (list "search-dfs-seq-calls" red:search-dfs-seq-calls-red cfg-call)
                  (list "search-dfs-fused-calls" red:search-dfs-fused-calls-red cfg-call)
                  (list "search-flip-seq-calls" red:search-flip-seq-calls-red cfg-call)
                  (list "search-flip-fused-calls" red:search-flip-fused-calls-red cfg-call)
                  (list "search-dfs-seq" red:search-dfs-seq-red cfg-flip)
                  (list "search-dfs-fused" red:search-dfs-fused-red cfg-flip)
                  (list "search-flip-seq" red:search-flip-seq-red cfg-flip)
                  (list "search-flip-fused" red:search-flip-fused-red cfg-flip)
                  (list "rail-seq" red:rail-seq-red cfg-rail)
                  (list "rail-fused" red:rail-fused-red cfg-rail)
                  (list "rail-seq-calls" red:rail-seq-calls-red cfg-call-rail)
                  (list "rail-fused-calls" red:rail-fused-calls-red cfg-call-rail)))])
      (match-define (list label rel cfg) entry)
      (define-values (steps final-cfg status)
        (trace-deterministic rel cfg TRACE-CAP))
      (check-true (or (eq? status 'done)
                      (eq? status 'cap)))
      (check-true (config-c-scope-agreement? final-cfg))
      (check-true (config-exact-scope? final-cfg))
      (check-equal? (count-bounced final-cfg)
                    (count-step-name steps "delay/invoke-delay")
                    label)
      (check-true (>= (count-bounced final-cfg) 1)
                  label))))

(module+ test
  (run-tests FRONTIER-OBSERVABLES))
