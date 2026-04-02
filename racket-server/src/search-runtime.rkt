#lang racket

(require redex/reduction-semantics
         "./search-lattice/canonical-adapter.rkt"
         (prefix-in lang:
                    "./search-lattice/languages/all.rkt")
         (rename-in "./search-lattice/reduction-relations/rail-fused-calls-red.rkt"
                    [step-once step-once/rail-fused-calls])
         (rename-in "./search-lattice/reduction-relations/rail-seq-calls-red.rkt"
                    [step-once step-once/rail-seq-calls])
         (rename-in "./search-lattice/reduction-relations/search-dfs-fused-calls-red.rkt"
                    [step-once step-once/search-dfs-fused-calls])
         (rename-in "./search-lattice/reduction-relations/search-dfs-seq-calls-red.rkt"
                    [step-once step-once/search-dfs-seq-calls])
         (rename-in "./search-lattice/reduction-relations/search-flip-fused-calls-red.rkt"
                    [step-once step-once/search-flip-fused-calls])
         (rename-in "./search-lattice/reduction-relations/search-flip-seq-calls-red.rkt"
                    [step-once step-once/search-flip-seq-calls])
         (prefix-in wf:
                    "./search-lattice/wf/all.rkt")
         "search-strategy.rkt")

(provide (struct-out strategy-spec)
         all-strategy-specs
         lookup-strategy-spec
         canonical-flat->calls-config
         calls-config->canonical-flat
         lookup-search-step-once
         search-config-in-domain?
         search-config-well-formed?
         check-search-config)

(struct strategy-spec (strategy step-once in-domain? well-formed?) #:transparent)

(define/match (strategy-key strategy)
  [((search-strategy hoist scheduler))
   (list hoist scheduler)])

(define all-strategy-specs
  (list
   (strategy-spec
    (search-strategy "early" "dfs")
    step-once/search-dfs-seq-calls
    (lambda (cfg)
      (redex-match? lang:search-base-seq-calls-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-base-calls? ,cfg))))
   (strategy-spec
    (search-strategy "late" "dfs")
    step-once/search-dfs-fused-calls
    (lambda (cfg)
      (redex-match? lang:search-base-fused-calls-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-base-calls? ,cfg))))
   (strategy-spec
    (search-strategy "early" "flip")
    step-once/search-flip-seq-calls
    (lambda (cfg)
      (redex-match? lang:search-base-seq-calls-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-base-calls? ,cfg))))
   (strategy-spec
    (search-strategy "late" "flip")
    step-once/search-flip-fused-calls
    (lambda (cfg)
      (redex-match? lang:search-base-fused-calls-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-base-calls? ,cfg))))
   (strategy-spec
    (search-strategy "early" "rail")
    step-once/rail-seq-calls
    (lambda (cfg)
      (redex-match? lang:rail-seq-calls-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/rail-calls? ,cfg))))
   (strategy-spec
    (search-strategy "late" "rail")
    step-once/rail-fused-calls
    (lambda (cfg)
      (redex-match? lang:rail-fused-calls-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/rail-calls? ,cfg))))))

(define spec-by-key
  (for/hash ([spec (in-list all-strategy-specs)])
    (match-define (strategy-spec strategy _ _ _) spec)
    (values (strategy-key strategy)
            spec)))

(define (lookup-strategy-spec strategy)
  (define normalized (normalize-search-strategy strategy))
  (hash-ref spec-by-key
            (strategy-key normalized)
            (lambda ()
              (error 'lookup-strategy-spec
                     "unsupported search strategy ~e"
                     normalized))))

(define (lookup-search-step-once strategy)
  (match-define (strategy-spec normalized step-internal _ _) (lookup-strategy-spec strategy))
  (lambda (cfg)
    (define next* (step-internal (canonical-flat->calls-config cfg)))
    (match next*
      ['() '()]
      [(list (list name cfg^))
       (list (list name (calls-config->canonical-flat cfg^)))]
      [_ (error 'lookup-search-step-once
                "unexpected successor set for ~e under ~e: ~e"
                cfg
                normalized
                next*)])))

(define (search-config-in-domain? strategy cfg)
  ((strategy-spec-in-domain? (lookup-strategy-spec strategy))
   (canonical-flat->calls-config cfg)))

(define (search-config-well-formed? strategy cfg)
  ((strategy-spec-well-formed? (lookup-strategy-spec strategy))
   (canonical-flat->calls-config cfg)))

(define (check-search-config strategy cfg)
  (match-define (strategy-spec normalized _ in-domain? well-formed?)
    (lookup-strategy-spec strategy))
  (unless (in-domain?
           (canonical-flat->calls-config cfg))
    (error 'check-search-config
           "program is outside the internal search target for strategy ~e"
           (search-strategy->jsexpr normalized)))
  (unless (well-formed?
           (canonical-flat->calls-config cfg))
    (error 'check-search-config
           "program failed internal search wf check for strategy ~e"
           (search-strategy->jsexpr normalized))))
