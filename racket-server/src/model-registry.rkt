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
(define (stepper rel)
  (lambda (prog) (var:step-once/by rel prog)))

(define all-model-specs
  (list (model-spec "mk-l0-core"
                    "Core (No RelCall/No Disjunction)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/fresh")
                    core:step-once)
        (model-spec "mk-l1-call-lazy"
                    "L1 Calls (Lazy, No Disjunction)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/fresh" "cap/delay")
                    (stepper var:Rl1-call-lazy))
        (model-spec "mk-l1-call-eager"
                    "L1 Calls (Eager, No Disjunction)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/fresh" "cap/delay")
                    (stepper var:Rl1-call-eager))
        (model-spec "mk-l2-disj-left"
                    "L2 Disjunction (No RelCall)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/disjunction" "cap/fresh")
                    (stepper var:Rl2-disj-left))
        (model-spec "mk-l4-rail-lazy"
                    "(Interleave + Railroad, Lazy)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh" "cap/delay")
                    (stepper var:Rl4-rail-lazy))
        (model-spec "mk-l3-dfs-lazy"
                    "(No Interleave, Lazy)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh" "cap/delay")
                    (stepper var:Rl3-dfs-lazy))
        (model-spec "mk-l3-flip-lazy"
                    "(Interleave + Flip-Flop, Lazy)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh" "cap/delay")
                    (stepper var:Rl3-flip-lazy))
        (model-spec "mk-l4-rail-eager"
                    "(Interleave + Railroad, Eager)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh" "cap/delay")
                    (stepper var:Rl4-rail-eager))
        (model-spec "mk-l3-dfs-eager"
                    "(No Interleave, Eager)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh" "cap/delay")
                    (stepper var:Rl3-dfs-eager))
        (model-spec "mk-l3-flip-eager"
                    "(Interleave + Flip-Flop, Eager)"
                    canonical-parser-profile
                    canonical-parser-target-id
                    '("cap/core" "cap/relcall" "cap/disjunction" "cap/fresh" "cap/delay")
                    (stepper var:Rl3-flip-eager))))

(define default-model-id "mk-l4-rail-lazy")

(define spec-by-id
  (for/hash ([spec (in-list all-model-specs)])
    (values (model-spec-id spec) spec)))

(define (lookup-model-spec model-id)
  (and (string? model-id)
       (hash-ref spec-by-id model-id #f)))

(define (lookup-model-step-once model-id)
  (define maybe-spec (lookup-model-spec model-id))
  (and maybe-spec (model-spec-step-once maybe-spec)))

(define (model-spec->jsexpr spec)
  (hasheq 'id (model-spec-id spec)
          'label (model-spec-label spec)
          'parserProfile (model-spec-parser-profile spec)
          'parserTarget (model-spec-parser-target spec)
          'capabilities (model-spec-capabilities spec)))
