#lang racket

(require (prefix-in core: "reduction-relations/core-reduction-relations.rkt")
         (prefix-in var: "reduction-relations/extensions/variant-relations.rkt")
         "transpiler.rkt")

(provide model-spec?
         model-spec-id
         model-spec-label
         model-spec-parser-profile
         model-spec-parser-target
         model-spec-capabilities
         model-spec-step-once
         all-model-specs
         default-model-id
         lookup-model-spec
         lookup-model-step-once
         model-spec->jsexpr)

(struct model-spec (id label parser-profile parser-target capabilities step-once) #:transparent)

;; This is intentionally a backend-only source of truth for model dispatch.
;; Frontend option wiring can consume this later without changing stepping code.
(define all-model-specs
  (list (model-spec "mk-l0-core"
                    "µKanren Core (No RelCall/No Disjunction)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/fresh")
                    core:step-once)
        (model-spec "mk-l1-call-lazy"
                    "µKanren L1 Calls (Lazy, No Disjunction)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/fresh")
                    var:step-once/Rl1-call-lazy)
        (model-spec "mk-l1-call-eager"
                    "µKanren L1 Calls (Eager, No Disjunction)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/fresh")
                    var:step-once/Rl1-call-eager)
        (model-spec "mk-l2-disj-left"
                    "µKanren L2 Disjunction (No RelCall)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/disjunction" "cap/fresh")
                    var:step-once/Rl2-disj-left)
        (model-spec "mk-l4-rail-lazy"
                    "µKanren (Interleave + Railroad, Lazy)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh")
                    var:step-once/Rl4-rail-lazy)
        (model-spec "mk-l3-dfs-lazy"
                    "µKanren (No Interleave, Lazy)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh")
                    var:step-once/Rl3-dfs-lazy)
        (model-spec "mk-l3-flip-lazy"
                    "µKanren (Interleave + Flip-Flop, Lazy)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh")
                    var:step-once/Rl3-flip-lazy)
        (model-spec "mk-l4-rail-eager"
                    "µKanren (Interleave + Railroad, Eager)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh")
                    var:step-once/Rl4-rail-eager)
        (model-spec "mk-l3-dfs-eager"
                    "µKanren (No Interleave, Eager)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh")
                    var:step-once/Rl3-dfs-eager)
        (model-spec "mk-l3-flip-eager"
                    "µKanren (Interleave + Flip-Flop, Eager)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh")
                    var:step-once/Rl3-flip-eager)))

(define default-model-id "mk-l4-rail-lazy")

(define spec-by-id
  (for/hash ([spec (in-list all-model-specs)])
    (values (model-spec-id spec) spec)))

(define (lookup-model-spec model-id)
  (define canonical-id
    (cond
      ;; Legacy id aliases for compatibility with older URLs/saved state.
      [(equal? model-id "microKanren") "mk-l4-rail-lazy"]
      [(equal? model-id "microKanren-rail") "mk-l4-rail-lazy"]
      [(equal? model-id "microKanren-noi-flip") "mk-l3-dfs-lazy"]
      [(equal? model-id "microKanren-flip") "mk-l3-flip-lazy"]
      [(equal? model-id "microKanren-rail-eager") "mk-l4-rail-eager"]
      [(equal? model-id "microKanren-flip-eager") "mk-l3-flip-eager"]
      [(equal? model-id "dfs") "mk-l3-dfs-lazy"]
      [(equal? model-id "mk-l4-dfs-lazy") "mk-l3-dfs-lazy"]
      [(equal? model-id "core") "mk-l0-core"]
      [(equal? model-id "l1-lazy") "mk-l1-call-lazy"]
      [(equal? model-id "l1-eager") "mk-l1-call-eager"]
      [(equal? model-id "l2") "mk-l2-disj-left"]
      [else model-id]))
  (and (string? canonical-id)
       (hash-ref spec-by-id canonical-id #f)))

(define (lookup-model-step-once model-id)
  (define maybe-spec (lookup-model-spec model-id))
  (and maybe-spec (model-spec-step-once maybe-spec)))

(define (model-spec->jsexpr spec)
  (hasheq 'id (model-spec-id spec)
          'label (model-spec-label spec)
          'parserProfile (model-spec-parser-profile spec)
          'parserTarget (model-spec-parser-target spec)
          'capabilities (model-spec-capabilities spec)))
