#lang racket

(require racket/port
         racket/sandbox
         redex/reduction-semantics
         "transpiler/ir/canonical-core-lang.rkt"
         "transpiler/ir/canonical-wf.rkt"
         "transpiler/ir/canonical-core-wf.rkt"
         "sexpr-read.rkt"
         "transpiler.rkt")

(provide canonical-core-shape?
         canonical-well-formed?
         canonical-target-well-formed?
         canonical-target-in-domain?
         check-canonical-well-formed
         check-syntax-capture-error)

;; Canonical-config -> boolean
;; Purpose: True when config is in the core judgment fragment shape.
(define (canonical-core-shape? canonical-config)
  (and (redex-match? canonical-core-lang config canonical-config)
       (judgment-holds (core-shape?/canonical ,canonical-config))))

;; Canonical-config -> boolean
;; Purpose: True when canonical config satisfies core wf-config? judgment.
(define (canonical-well-formed? canonical-config)
  (and (redex-match? canonical-core-lang config canonical-config)
       (judgment-holds (wf-config/canonical-core? ,canonical-config))))

;; Canonical-config String -> boolean
;; Purpose: True when canonical config is in the selected target language domain.
(define (canonical-target-in-domain? canonical-config [target-id canonical-parser-target-id])
  (config-in-target-domain? target-id canonical-config))

;; Canonical-config String -> boolean
;; Purpose: True when canonical config is wf under the selected target judgment.
(define (canonical-target-well-formed? canonical-config [target-id canonical-parser-target-id])
  (wf-config/target? target-id canonical-config))

;; Canonical-config String -> String or Error
;; Purpose: Canonical target-specific wf gate used by runtime and tests.
(define (check-canonical-well-formed canonical-config [target-id canonical-parser-target-id])
  (if (and (canonical-target-in-domain? canonical-config target-id)
           (canonical-target-well-formed? canonical-config target-id))
      ""
      (error (format "Program failed canonical ~a wf check." target-id))))


;; String -> String or Error
;; Purpose: Uses syntax-spec to throw static errors in the given program.
(define (check-syntax-capture-error program-str)
    (parameterize ([current-namespace (make-base-namespace)])
      (expand (datum->syntax #f
                             `(module syntax-checker racket/base
                                (require hosted-minikanren)
                                ,@(read-all-sexprs (open-input-string program-str)))))))
