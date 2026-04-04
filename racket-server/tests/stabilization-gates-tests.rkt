#lang racket

(require racket/list
         rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "../src/search-lattice/languages/all.rkt"
         (prefix-in red: "../src/search-lattice/reduction-relations/all.rkt")
         (prefix-in wf: "../src/search-lattice/wf/all.rkt")
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         "./example-compat-tests.rkt"
         "./frontier-observable-support.rkt"
         "./search-lattice-support.rkt")

(provide STABILIZATION-GATES)

(define TRACE-CAP 64)

(define (example-src label)
  (for/first ([pr (in-list (frontend-example-programs))]
              #:do [(match-define (cons example-label src) pr)]
              #:when (equal? example-label label))
    src))

(define (example-frontier label)
  (define src (example-src label))
  (unless src
    (error 'example-frontier "missing example label: ~a" label))
  (define-values (cfg _html)
    (parse-prog/canonical (read-all-sexprs (open-input-string src))))
  (match cfg
    [`(() ,frontier) frontier]
    [_ (error 'example-frontier
              "expected an empty-Γ config for ~a, got ~s"
              label
              cfg)]))

(define (named-step rel cfg)
  (match (remove-duplicates
          (apply-reduction-relation/tag-with-names rel cfg))
    [(list (list name cfg^))
     (values (~a name) cfg^)]
    [other
     (error 'named-step "expected one tagged successor, got ~e" other)]))

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

(define (trace-locked? rel wf? matcher cfg [remaining TRACE-CAP])
  (cond
    [(negative? remaining) #f]
    [(not (and (wf? cfg)
               (matcher cfg)
               (produced-answer-spine-only? cfg)
               (config-exact-scope? cfg)))
     #f]
    [else
     (match (remove-duplicates
             (apply-reduction-relation/tag-with-names rel cfg))
       ['() (final-program? cfg)]
       [(list (list _ cfg^))
        (trace-locked? rel wf? matcher cfg^ (sub1 remaining))]
       [_ #f])]))

(define (wf-core? cfg)
  (judgment-holds (wf:wf-cfg/core? ,cfg)))

(define (wf-delay? cfg)
  (judgment-holds (wf:wf-cfg/delay? ,cfg)))

(define (wf-disj? cfg)
  (judgment-holds (wf:wf-cfg/disj? ,cfg)))

(define (wf-search-base? cfg)
  (judgment-holds (wf:wf-cfg/search-base? ,cfg)))

(define (core-shape? cfg)
  (redex-match? core-lang cfg cfg))

(define (delay-shape? cfg)
  (redex-match? delay-lang cfg cfg))

(define (disj-shape? cfg)
  (redex-match? disj-lang cfg cfg))

(define (search-base-shape? cfg)
  (redex-match? search-base-lang cfg cfg))

(define cfg-core-succeed
  (term ((succeed (label "ok")) ,sigma-a)))

(define cfg-core-fail
  (term ((fail (label "no")) ,sigma-a)))

(define cfg-core-conj-success
  (term ((⊤ ,sigma-a)
         × (succeed (label "k"))
         ())))

(define cfg-core-conj-fail
  (term ((empty-tree)
         × (succeed (label "k"))
         ())))

(define cfg-core-fresh-fail
  (term ((∃ (x:0)
            ((x:0 =? (sym "cat") (label "eq-left"))
             ∧
             (x:0 =? (sym "dog") (label "eq-right"))
             (label "and"))
            (label "fresh"))
         ,sigma-s)))

(define cfg-core-empty-fresh
  (term ((∃ ()
            (succeed (label "inner"))
            (label "fresh-empty"))
         ,sigma-s)))

(define cfg-delay-through-conj
  (term ((delay ((succeed (label "inner")) ,sigma-s))
         × (succeed (label "k"))
         ())))

(define cfg-double-delay
  (term ((suspend
          (suspend (succeed (label "inner"))
                   (label "zz-inner"))
          (label "zz-outer"))
         ,sigma-s)))

(define cfg-fresh-inside-delay
  (term ((suspend
          (∃ (x:0)
             (x:0 =? (sym "nap") (label "eq"))
             (label "fresh"))
          (label "zz"))
         ,sigma-s)))

(define cfg-delay-inside-fresh
  (term ((∃ (x:0)
            (suspend (x:0 =? (sym "nap") (label "eq"))
                     (label "zz"))
            (label "fresh"))
         ,sigma-s)))

(define cfg-disj-goal
  (term (((succeed (label "left"))
          ∨
          (fail (label "right"))
          (label "split"))
         ,sigma-s)))

(define/provide-test-suite STABILIZATION-GATES
  (test-case "L0/core lock gates"
    (check-true (redex-match? core-lang QFresh (term (FreshenedTree (u:0) hole (label "fresh")))))
    (check-false (redex-match? core-lang search (term (delay ((succeed (label "late")) ,sigma-s)))))
    (check-false (redex-match? core-lang search (term (Bounced (⊤ ,sigma-a)))))
    (check-false (redex-match? core-lang search (term ((⊤ ,sigma-a) + (empty-tree)))))
    (define-values (succeed-name succeed-next)
      (named-step red:core-red cfg-core-succeed))
    (define-values (fail-name fail-next)
      (named-step red:core-red cfg-core-fail))
    (define-values (conj-success-name conj-success-next)
      (named-step red:core-red cfg-core-conj-success))
    (define-values (conj-fail-name conj-fail-next)
      (named-step red:core-red cfg-core-conj-fail))
    (check-equal? succeed-name "succeed")
    (check-equal? fail-name "fail")
    (check-equal? conj-success-name "conj-bring-scoped-success")
    (check-equal? conj-fail-name "conj-preserve-scoped-fail")
    (check-equal? succeed-next (term (⊤ ,sigma-a)))
    (check-equal? fail-next (term (empty-tree)))
    (check-equal? conj-success-next
                  (term ((succeed (label "k")) ,sigma-a)))
    (check-equal? conj-fail-next (term (empty-tree)))
    (define-values (empty-fresh-name empty-fresh-next)
      (named-step red:core-red cfg-core-empty-fresh))
    (check-equal? empty-fresh-name "fresh-substitute")
    (check-equal? empty-fresh-next
                  (term (FreshenedTree ()
                                       ((succeed (label "inner")) ,sigma-s)
                                       (label "fresh-empty"))))
    (check-true (trace-locked? red:core-red
                               wf-core?
                               core-shape?
                               (example-frontier "fresh witness")))
    (check-true (trace-locked? red:core-red
                               wf-core?
                               core-shape?
                               (example-frontier "core/fresh+conj+unify")))
    (check-true (trace-locked? red:core-red
                               wf-core?
                               core-shape?
                               cfg-core-fresh-fail)))

  (test-case "L1/delay lock gates"
    (check-false (redex-match? delay-lang cfg '(delay (empty-tree))))
    (check-false
     (redex-match?
      delay-lang
      cfg
      (term (delay (FreshenedTree (u:0) (⊤ ,sigma-a) (label "fresh"))))))
    (check-true (redex-match? delay-lang cfg (term ,delayed-left-search)))
    (define-values (delay-step-1 delay-next-1)
      (named-step red:delay-red cfg-delay-goal))
    (define-values (delay-step-2 _delay-next-2)
      (named-step red:delay-red delay-next-1))
    (define-values (delay-conj-name _delay-conj-next)
      (named-step red:delay-red cfg-delay-through-conj))
    (check-equal? delay-step-1 "suspend-goal")
    (check-equal? delay-step-2 "invoke-delay")
    (check-equal? delay-conj-name "delay-through-conj")
    (define-values (double-delay-steps double-delay-final double-delay-status)
      (trace-deterministic red:delay-red cfg-double-delay))
    (check-equal? double-delay-status 'done)
    (check-true (trace-locked? red:delay-red wf-delay? delay-shape? cfg-double-delay))
    (check-equal? (take double-delay-steps 4)
                  '("suspend-goal"
                    "invoke-delay"
                    "suspend-goal"
                    "invoke-delay"))
    (check-equal? (count-bounced double-delay-final) 2)
    (define-values (fresh-outside-step fresh-outside-next)
      (named-step red:delay-red cfg-delay-inside-fresh))
    (check-equal? fresh-outside-step "fresh-substitute")
    (check-true (config-exact-scope? fresh-outside-next))
    (check-true (wf-delay? fresh-outside-next))
    (check-true (trace-locked? red:delay-red
                               wf-delay?
                               delay-shape?
                               cfg-fresh-inside-delay))
    (check-true (trace-locked? red:delay-red
                               wf-delay?
                               delay-shape?
                               cfg-delay-inside-fresh)))

  (test-case "L2/shared disjunction lock gates"
    (check-true (redex-match? disj-lang KBranch (term (hole <-+ (empty-tree)))))
    (check-true
     (redex-match?
      disj-lang
      KLate
      (term (hole × (succeed (label "k")) ()))))
    (define pending-disj
      (term ((((succeed (label "left")) ,sigma-s)
              <-+
              ((succeed (label "right")) ,sigma-s))
             × (succeed (label "k"))
             ())))
    (define-values (goal-seq-name _goal-seq-next)
      (named-step red:disj-seq-red cfg-disj-goal))
    (define-values (goal-fused-name _goal-fused-next)
      (named-step red:disj-fused-red cfg-disj-goal))
    (define-values (seq-name _seq-next)
      (named-step red:disj-seq-red pending-disj))
    (define-values (fused-pending-name _fused-pending-next)
      (named-step red:disj-fused-red pending-disj))
    (define-values (fused-answer-name _fused-answer-next)
      (named-step red:disj-fused-red cfg-mixed-answer))
    (define-values (fused-fail-name _fused-fail-next)
      (named-step red:disj-fused-red cfg-mixed-fail))
    (check-equal? goal-seq-name "expand-disjunction")
    (check-equal? goal-fused-name "expand-disjunction")
    (check-equal? seq-name "distribute-over-conj")
    (check-equal? fused-pending-name "succeed")
    (check-equal? fused-answer-name "continue-left-answer")
    (check-equal? fused-fail-name "continue-left-fail")
    (define nested-answer
      (term (((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))
             <-+
             (empty-tree))))
    (define nested-fail
      (term (((empty-tree) <-+ (⊤ ,sigma-b))
             <-+
             (empty-tree))))
    (define freshened-answer
      (term (((FreshenedTree (u:0) (⊤ ,sigma-a) (label "fresh")) <-+ (⊤ ,sigma-b))
             × (succeed (label "k"))
             ())))
    (define-values (fused-fresh-name fused-fresh-next)
      (named-step red:disj-fused-red freshened-answer))
    (for ([rel (in-list (list red:disj-seq-red red:disj-fused-red))])
      (define-values (reassoc-answer-name reassoc-answer-next)
        (named-step rel nested-answer))
      (define-values (consume-answer-name consume-answer-next)
        (named-step rel reassoc-answer-next))
      (define-values (reassoc-fail-name reassoc-fail-next)
        (named-step rel nested-fail))
      (define-values (consume-fail-name consume-fail-next)
        (named-step rel reassoc-fail-next))
      (check-equal? reassoc-answer-name "reassociate-left-answer")
      (check-equal? consume-answer-name "promote-left-answer")
      (check-equal? reassoc-fail-name "erase-left-fail")
      (check-equal? consume-fail-name "promote-left-answer")
      (check-equal? reassoc-answer-next
                    (term ((⊤ ,sigma-a) <-+ ((⊤ ,sigma-b) <-+ (empty-tree)))))
      (check-equal? consume-answer-next
                    (term ((⊤ ,sigma-a) + ((⊤ ,sigma-b) <-+ (empty-tree)))))
      (check-equal? reassoc-fail-next
                    (term ((⊤ ,sigma-b) <-+ (empty-tree))))
      (check-equal? consume-fail-next
                    (term ((⊤ ,sigma-b) + (empty-tree)))))
    (check-equal? fused-fresh-name "continue-left-answer")
    (check-equal? fused-fresh-next
                  (term ((FreshenedTree (u:0)
                                    ((succeed (label "k")) ,sigma-a)
                                    (label "fresh"))
                         <-+
                         ((⊤ ,sigma-b) × (succeed (label "k")) ()))))
    (for ([rel (in-list (list red:disj-seq-red red:disj-fused-red))])
      (define-values (shared-steps shared-final shared-status)
        (trace-deterministic rel (example-frontier "fresh shared disj")))
      (define-values (branch-steps branch-final branch-status)
        (trace-deterministic rel (example-frontier "fresh branch disj")))
      (check-equal? shared-status 'done)
      (check-equal? branch-status 'done)
      (check-true (wf-disj? shared-final))
      (check-true (wf-disj? branch-final))
      (check-true (disj-shape? shared-final))
      (check-true (disj-shape? branch-final))
      (check-true (config-exact-scope? shared-final))
      (check-true (config-exact-scope? branch-final))
      (check-true (final-program? shared-final))
      (check-true (final-program? branch-final))
      (check-equal? (count-step-name shared-steps "fresh-substitute") 2)
      (check-equal? (count-step-name branch-steps "fresh-substitute") 3)
      (check-equal? (count-answers shared-final) 2)
      (check-equal? (count-answers branch-final) 2)
      (check-true (config-exact-scope? shared-final))
      (check-true (config-exact-scope? branch-final)))))

  (test-case "L3/search-base reopen gates"
    (define bounced-branch
      (term (Bounced (((⊤ ,sigma-a) <-+ (empty-tree))
                      <-+
                      (⊤ ,sigma-b)))))
    (define bad-bounced-promotion
      (term (Bounced ((((⊤ ,sigma-a) + (empty-tree))
                       <-+
                       (⊤ ,sigma-b))))))
    (define prefixed-bounced
      (term (Bounced ((⊤ ,sigma-a)
                      +
                      ((⊤ ,sigma-b) <-+ (empty-tree))))))
    (check-true (redex-match? search-base-lang cfg cfg-disj))
    (check-true (redex-match? search-base-lang cfg bounced-branch))
    (check-false
     (redex-match?
      search-base-lang
      cfg
      (term (((⊤ ,sigma-a) + (empty-tree)) <-+ (⊤ ,sigma-b)))))
    (for ([rel (in-list (list red:search-base-seq-red
                              red:search-base-fused-red))])
      (define-values (plain-name plain-next)
        (named-step rel cfg-disj))
      (check-equal? plain-name "promote-left-answer")
      (check-equal? plain-next
                    (term ((⊤ ,sigma-a) + (⊤ ,sigma-b))))
      (define-values (bounce-step-1-name bounce-step-1)
        (named-step rel bounced-branch))
      (define-values (bounce-step-2-name bounce-step-2)
        (named-step rel bounce-step-1))
      (check-equal? bounce-step-1-name "reassociate-left-answer")
      (check-equal? bounce-step-2-name "promote-left-answer")
      (check-equal? bounce-step-1
                    (term (Bounced ((⊤ ,sigma-a)
                                    <-+
                                    ((empty-tree) <-+ (⊤ ,sigma-b))))))
      (check-false
       (member bad-bounced-promotion
               (map tagged-successor-cfg
                    (apply-reduction-relation/tag-with-names rel bounced-branch))))
      (check-equal? bounce-step-2
                    (term (Bounced ((⊤ ,sigma-a)
                                    +
                                    ((empty-tree) <-+ (⊤ ,sigma-b))))))
      (define-values (prefixed-name prefixed-next)
        (named-step rel prefixed-bounced))
      (check-equal? prefixed-name "promote-left-answer")
      (check-equal? prefixed-next
                    (term (Bounced ((⊤ ,sigma-a)
                                    +
                                    ((⊤ ,sigma-b) + (empty-tree))))))
      (check-true (wf-search-base? bounce-step-1))
      (check-true (wf-search-base? bounce-step-2))
      (check-true (wf-search-base? prefixed-next))
      (check-true (trace-locked? rel
                                 wf-search-base?
                                 search-base-shape?
                                 cfg-delay-goal))
      (check-true (shape-closed? search-base-shape? rel cfg-delay-goal))
      (define-values (seq/fused-name _seq/fused-next)
        (named-step rel cfg-mixed-answer))
      (check-not-false
       (member seq/fused-name
               '("distribute-over-conj"
                 "continue-left-answer")))))

(module+ test
  (run-tests STABILIZATION-GATES))
