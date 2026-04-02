#lang racket

(require racket/set
         racket/list
         "transpiler.rkt"
         "sexpr-read.rkt"
         "syntax-checking.rkt"
         "model-registry.rkt")

(provide ANALYSIS-VERSION
         REQ-CORE
         REQ-RELCALL
         REQ-DISJUNCTION
         REQ-FRESH
         REQ-DELAY
         requirement->capability
         ast->requirements
         analyze-source-capabilities
         incompatible-reasons
         compatible-model-ids
         incompatible-model-ids)

(define ANALYSIS-VERSION "v2")

(define (requirement->capability req)
  (cond
    [(equal? req REQ-CORE) "cap/core"]
    [(equal? req REQ-RELCALL) "cap/relcall"]
    [(equal? req REQ-DISJUNCTION) "cap/disjunction"]
    [(equal? req REQ-FRESH) "cap/fresh"]
    [(equal? req REQ-DELAY) "cap/delay"]
    [else #f]))

(define (analyze-source-capabilities source
                                     #:source-mode [source-mode default-source-mode]
                                     #:compile-profile [compile-profile #f])
  (define source-mode* (normalize-source-mode source-mode))
  (when (equal? source-mode* "mini")
    (check-syntax-capture-error source))
  (define sexprs (read-all-sexprs (open-input-string source)))
  (define ast (parse-prog->ast sexprs
                               #:source-mode source-mode*
                               #:compile-profile compile-profile))
  (hasheq 'validSyntax #t
          'requirements (ast->requirements ast)
          'analysisVersion ANALYSIS-VERSION))

(define (missing-capabilities requirements capabilities)
  (define caps (list->set capabilities))
  (for/list ([req (in-list requirements)]
             #:do [(define cap (requirement->capability req))]
             #:when (and cap (not (set-member? caps cap))))
    (list req cap)))

(define (incompatible-reasons requirements capabilities)
  (for/list ([entry (in-list (missing-capabilities requirements capabilities))])
    (match-define (list req cap) entry)
    (format "missing ~a (required by ~a)" cap req)))

(define (compatible-model-ids requirements specs)
  (for/list ([spec (in-list specs)]
             #:when (null? (incompatible-reasons requirements
                                                 (model-spec-capabilities spec))))
    (model-spec-id spec)))

(define (incompatible-model-ids requirements specs)
  (for/list ([spec (in-list specs)]
             #:when (not (null? (incompatible-reasons requirements
                                                      (model-spec-capabilities spec)))))
    (model-spec-id spec)))
