#lang racket

(require rackunit
         rackunit/text-ui
         "../src/capability-analysis.rkt"
         "../src/model-registry.rkt")

(define core-source
  "(run* (q)
     (fresh (x y)
       (== x (cons 'ok '()))
       (== y (cons 'ok '()))
       (== q x)
       (== q y)))")

(define appendo-source
  "(defrel (appendo l s out)
     (conde
       [(== l '()) (== s out)]
       [(fresh (a d res)
          (== l (cons a d))
          (== out (cons a res))
          (appendo d s res))]))

   (run* (q) (appendo (list 'mini) (list 'kanren) q))")

(define unify-only-source
  "(run* (q) (== q 'ok))")

(define micro-delay-source
  "(defrel (same x y)
     (Zzz (== x y)))

   (run* (q) (same q 'cat))")

(define/provide-test-suite CAPABILITY-ANALYSIS
  (test-case "core/fresh+conj+unify infers core + fresh"
    (define analysis (analyze-source-capabilities core-source))
    (define reqs (hash-ref analysis 'requirements))
    (check-not-false (member REQ-CORE reqs))
    (check-not-false (member REQ-FRESH reqs))
    (check-false (member REQ-RELCALL reqs))
    (check-false (member REQ-DISJUNCTION reqs)))

  (test-case "appendo infers relcall + disjunction + fresh + compiler delay"
    (define analysis (analyze-source-capabilities appendo-source))
    (define reqs (hash-ref analysis 'requirements))
    (check-not-false (member REQ-CORE reqs))
    (check-not-false (member REQ-RELCALL reqs))
    (check-not-false (member REQ-DISJUNCTION reqs))
    (check-not-false (member REQ-FRESH reqs))
    (check-not-false (member REQ-DELAY reqs)))

  (test-case "simple run with unify infers core only"
    (define analysis (analyze-source-capabilities unify-only-source))
    (define reqs (hash-ref analysis 'requirements))
    (check-not-false (member REQ-CORE reqs))
    (check-false (member REQ-RELCALL reqs))
    (check-false (member REQ-DISJUNCTION reqs))
    (check-false (member REQ-FRESH reqs)))

  (test-case "direct micro source reports explicit delay requirement"
    (define analysis
      (analyze-source-capabilities micro-delay-source
                                   #:source-mode "micro"))
    (define reqs (hash-ref analysis 'requirements))
    (check-not-false (member REQ-CORE reqs))
    (check-not-false (member REQ-RELCALL reqs))
    (check-not-false (member REQ-DELAY reqs))
    (check-false (member REQ-DISJUNCTION reqs))
    (check-false (member REQ-FRESH reqs)))

  (test-case "direct micro source rejects compile profiles"
    (check-exn
     exn:fail?
     (lambda ()
       (analyze-source-capabilities
        micro-delay-source
        #:source-mode "micro"
        #:compile-profile
        (hasheq 'conjAssoc "left"
                'disjAssoc "right"
                'delayPlacement "relbody")))))

  (test-case "all currently surfaced models satisfy hard requirements for appendo"
    (define analysis (analyze-source-capabilities appendo-source))
    (define reqs (hash-ref analysis 'requirements))
    (define compatible-ids (compatible-model-ids reqs all-model-specs))
    (check-true (not (null? compatible-ids)))
    (check-equal? (sort compatible-ids string<?)
                  (sort '("mk-l3-dfs-eager"
                          "mk-l3-dfs-lazy"
                          "mk-l3-flip-eager"
                          "mk-l3-flip-lazy"
                          "mk-l4-rail-eager"
                          "mk-l4-rail-lazy")
                        string<?))))

(module+ test
  (run-tests CAPABILITY-ANALYSIS))
