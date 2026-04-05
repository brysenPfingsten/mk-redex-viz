#lang racket

(require redex/reduction-semantics
         (prefix-in lang:
                    "./search-lattice/languages/all.rkt")
         (rename-in "./search-lattice/reduction-relations/rail-late-relcall-red.rkt"
                    [step-once step-once/rail-late-relcall])
         (rename-in "./search-lattice/reduction-relations/rail-early-relcall-red.rkt"
                    [step-once step-once/rail-early-relcall])
         (rename-in "./search-lattice/reduction-relations/search-dfs-late-relcall-red.rkt"
                    [step-once step-once/search-dfs-late-relcall])
         (rename-in "./search-lattice/reduction-relations/search-dfs-early-relcall-red.rkt"
                    [step-once step-once/search-dfs-early-relcall])
         (rename-in "./search-lattice/reduction-relations/search-flip-late-relcall-red.rkt"
                    [step-once step-once/search-flip-late-relcall])
         (rename-in "./search-lattice/reduction-relations/search-flip-early-relcall-red.rkt"
                    [step-once step-once/search-flip-early-relcall])
         (prefix-in wf:
                    "./search-lattice/wf/all.rkt")
         "search-strategy.rkt")

(provide (struct-out strategy-spec)
         all-strategy-specs
         lookup-strategy-spec
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
    step-once/search-dfs-early-relcall
    (lambda (cfg)
      (redex-match? lang:search-relcall-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-relcall? ,cfg))))
   (strategy-spec
   (search-strategy "late" "dfs")
    step-once/search-dfs-late-relcall
    (lambda (cfg)
      (redex-match? lang:search-relcall-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-relcall? ,cfg))))
   (strategy-spec
   (search-strategy "early" "flip")
    step-once/search-flip-early-relcall
    (lambda (cfg)
      (redex-match? lang:search-relcall-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-relcall? ,cfg))))
   (strategy-spec
   (search-strategy "late" "flip")
    step-once/search-flip-late-relcall
    (lambda (cfg)
      (redex-match? lang:search-relcall-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/search-relcall? ,cfg))))
   (strategy-spec
   (search-strategy "early" "rail")
    step-once/rail-early-relcall
    (lambda (cfg)
      (redex-match? lang:rail-relcall-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/rail-relcall? ,cfg))))
   (strategy-spec
   (search-strategy "late" "rail")
    step-once/rail-late-relcall
    (lambda (cfg)
      (redex-match? lang:rail-relcall-lang config cfg))
    (lambda (cfg)
      (judgment-holds (wf:wf-config/rail-relcall? ,cfg))))))

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
  (match-define (strategy-spec _ step-once _ _) (lookup-strategy-spec strategy))
  step-once)

(define (search-config-in-domain? strategy cfg)
  ((strategy-spec-in-domain? (lookup-strategy-spec strategy))
   cfg))

(define (search-config-well-formed? strategy cfg)
  ((strategy-spec-well-formed? (lookup-strategy-spec strategy))
   cfg))

(define (check-search-config strategy cfg)
  (match-define (strategy-spec normalized _ in-domain? well-formed?)
    (lookup-strategy-spec strategy))
  (unless (in-domain? cfg)
    (error 'check-search-config
           "program is outside the internal search target for strategy ~e"
           (search-strategy->jsexpr normalized)))
  (unless (well-formed? cfg)
    (error 'check-search-config
           "program failed internal search wf check for strategy ~e"
           (search-strategy->jsexpr normalized))))
