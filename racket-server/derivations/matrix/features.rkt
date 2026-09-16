#lang racket

(require redex/reduction-semantics
         (only-in "../shared/core/e/language.rkt" core-e-oracle-lang)
         (only-in "../shared/core/n/language.rkt" core-n-oracle-lang)
         "../shared/kernel.rkt" "../shared/feature-schema.rkt" "ownerless-source.rkt" "source-s.rkt"
         (for-syntax racket/base racket/syntax syntax/parse))

;; The strict helper-computation forms are common. Feature admission is in the
;; recursive source goal grammar and the literal rule set, independently for
;; each row. Search is the Delay/Disjunction union plus the explicitly selected
;; rail interaction mplus-delay; neither child contains that interaction.
(define-syntax (define-s-feature stx)
  (syntax-parse stx
    [(_ prefix:id language:id feature:id)
     #:with (feature-goal ...) (feature-goal-productions (syntax-e #'feature) #'language)
     #:with (feature-value ...) (feature-extra-productions (syntax-e #'feature) 'SV #t #'language)
     #:with (feature-computation ...) (feature-extra-productions (syntax-e #'feature) 'c #t #'language)
     #:with (feature-context ...) (feature-extra-productions (syntax-e #'feature) 'E #t #'language)
     #:with (feature-observation ...) (feature-extra-productions (syntax-e #'feature) 'O #t #'language)
     #:with (feature-frontier ...) (feature-extra-productions (syntax-e #'feature) 'F #t #'language)
     #:with (feature-observer ...) (feature-extra-productions (syntax-e #'feature) 'o #t #'language)
     #:with (feature-observer-context ...) (feature-extra-productions (syntax-e #'feature) 'C #t #'language)
     #:with control (format-id #'prefix "~a-control-raw" #'prefix)
     #:with red (format-id #'prefix "strict-~a-red" #'prefix)
     #:with allocation-red (format-id #'prefix "~a-allocation-red" #'prefix)
     #:with contract (format-id #'prefix "~a-contract" #'prefix)
     #:with value? (format-id #'prefix "~a-value?" #'prefix)
     #:with observation? (format-id #'prefix "~a-observation?" #'prefix)
     #:with frontier? (format-id #'prefix "~a-frontier?" #'prefix)
     #:with initial (format-id #'prefix "~a-initial" #'prefix)
     #:with query-initial (format-id #'prefix "~a-query-initial" #'prefix)
     #'(begin
         (provide language red control contract value? observation? frontier?
                  (rename-out [s-initial initial] [s-query-initial query-initial]))
         (define-extended-language language StrictS
           [g eq (t != t tag) (succeed tag) (fail tag)
              (∃ d g tag) (g ∧ g tag) feature-goal ...]
           [SV (Empty owners) (One owners σ) feature-value ...]
           [c SV (eval owners g σ) (bind owners c g) feature-computation ...]
           [E hole (bind owners E g) feature-context ...]
           [O (Done owners) (Solo owners σ) feature-observation ...]
           [F (Done owners) (Solo owners σ) feature-frontier ...]
           [o F (render c) (commit c) (advance o) (collect o) feature-observer ...]
           [C E (render E) (commit E) (advance C) (collect C)
              feature-observer-context ...])
         (define-s-control control language feature)
         (define allocation-red
           (reduction-relation
            language #:domain q
            [--> (in-hole C (eval owners (∃ (x (... ...)) g tag) σ))
                 (in-hole C ,(allocate/s
                              (term owners) (term (x (... ...))) (term g)
                              (term tag) (term σ) (context-support/s (term C))))
                 allocate-fresh]))
         (define red
           (union-reduction-relations
            (context-closure control language C) allocation-red))
         (define (contract computation [prefix '()])
           (define raw
             (extend-reduction-relation
              control language
              [--> (eval owners (∃ (x (... ...)) g tag) σ)
                   ,(allocate/s (term owners) (term (x (... ...))) (term g)
                                (term tag) (term σ) prefix) allocate-fresh]))
           (match (apply-reduction-relation/tag-with-names raw computation)
             ['() #f] [(list result) result]
             [results (error 'contract "nonunique raw proof: ~e" results)]))
         (define (value? value) (redex-match? language SV value))
         (define (observation? value) (redex-match? language O value))
         (define (frontier? value) (redex-match? language F value)))]))

(define-s-feature s-core StrictSCore core)
(define-s-feature s-delay StrictSDelay delay)
(define-s-feature s-disjunction StrictSDisjunction disjunction)

(define-ownerless-strict-source e-core StrictECore core-e-oracle-lang support
  atomic/e allocate/e '(Support) #:feature core)
(define-ownerless-strict-source e-delay StrictEDelay core-e-oracle-lang support
  atomic/e allocate/e '(Support) #:feature delay)
(define-ownerless-strict-source e-disjunction StrictEDisjunction core-e-oracle-lang support
  atomic/e allocate/e '(Support) #:feature disjunction)

(define-ownerless-strict-source n-core StrictNCore core-n-oracle-lang next
  atomic/n allocate/n 0 #:feature core)
(define-ownerless-strict-source n-delay StrictNDelay core-n-oracle-lang next
  atomic/n allocate/n 0 #:feature delay)
(define-ownerless-strict-source n-disjunction StrictNDisjunction core-n-oracle-lang next
  atomic/n allocate/n 0 #:feature disjunction)
