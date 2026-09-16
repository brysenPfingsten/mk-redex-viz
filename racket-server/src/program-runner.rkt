#lang racket

(require racket/string
         "search-picture.rkt"
         "search-runtime.rkt"
         "search-strategy.rkt"
         "sexpr-read.rkt"
         "syntax-checking.rkt"
         "transpiler.rkt"
         "zipper.rkt")

(provide open-source
         open-forms
         open-compiled
         answer-json->host-value
         (struct-out model-step)
         (struct-out model-session)
         model-session-current-step
         model-session-current-step-name
         model-session-current-step-kind
         model-session-current-config
         model-session-current-picture
         model-session-current-answer-nodes
         model-session-current-answers
         model-session-current-host-answers
         model-session-step-index
         model-session-operation-counts
         model-session-status
         model-session-done?
         model-session-step
         model-session-back
         model-session-reset
         (struct-out search-strategy)
         (struct-out strict-search)
         default-search-strategy
         all-surfaced-search-strategies
         search-strategy->jsexpr
         normalize-search-strategy
         default-source-mode
         normalize-source-mode
         (struct-out compile-profile)
         canonical-compile-profile
         canonical-compile-profile-jsexpr
         normalize-compile-profile
         compile-profile->jsexpr)

(struct model-step (name config) #:transparent)

(struct model-session (zipper step-once query search-strategy) #:transparent)

(define reified-var-rx #px"^_\\.[0-9]+$")

(define (normalize-form-datum form)
  (cond
    [(syntax? form) (syntax->datum form)]
    [else form]))

(define (forms->source-string forms)
  (string-join (map (lambda (form)
                      (format "~s" (normalize-form-datum form)))
                    forms)
               "\n"))

(define (prepare-source raw-prog source-mode compile-profile strategy)
  (define source-mode*
    (normalize-source-mode source-mode))
  (define compile-profile*
    (normalize-compile-profile compile-profile source-mode*))
  (define strategy*
    (normalize-search-strategy strategy))
  (when (equal? source-mode* "mini")
    (void (check-syntax-capture-error raw-prog)))
  (define sexpr-prog
    (read-all-sexprs (open-input-string raw-prog)))
  (define-values (initial-config _html query)
    (parse-prog/canonical sexpr-prog
                          #:source-mode source-mode*
                          #:compile-profile compile-profile*
                          #:search-strategy strategy*))
  (values initial-config
          query
          strategy*))

(define (answer-json->host-value datum)
  (match datum
    [(? hash? h)
     (cond
       [(hash-has-key? h 'sym)
        (string->symbol (hash-ref h 'sym))]
       [(hash-has-key? h 'num)
        (hash-ref h 'num)]
       [(hash-has-key? h 'str)
        (hash-ref h 'str)]
       [(hash-has-key? h 'var)
        (string->symbol (hash-ref h 'var))]
       [(hash-has-key? h 'pair)
        (match (hash-ref h 'pair)
          [(list left right)
           (cons (answer-json->host-value left)
                 (answer-json->host-value right))]
          [_ h])]
       [else h])]
    [(? string? s)
     (if (regexp-match? reified-var-rx s)
         (string->symbol s)
         s)]
    [(? list? elems)
     (map answer-json->host-value elems)]
    [_ datum]))

(define (answer-nodes->reified answer-nodes)
  (for/list ([answer-node (in-list answer-nodes)])
    (hash-ref answer-node 'reified '())))

(define (answer-nodes->host-values answer-nodes)
  (for/list ([answer-node (in-list answer-nodes)])
    (answer-json->host-value (hash-ref answer-node 'reified '()))))

(define (open-compiled initial-config query [strategy default-search-strategy])
  (define normalized (normalize-search-strategy strategy))
  (check-search-config normalized initial-config)
  (unless (query-info? query)
    (raise-argument-error 'open-compiled "query-info?" query))
  (model-session
   (zipper-add (make-empty-zipper)
               (model-step "Initialize Program" initial-config))
   (lookup-search-step-once normalized)
   query
   normalized))

(define (open-source raw-prog
                     #:source-mode [source-mode default-source-mode]
                     #:compile-profile [compile-profile #f]
                     #:search-strategy [strategy default-search-strategy])
  (define-values (initial-config query strategy*)
    (prepare-source raw-prog source-mode compile-profile strategy))
  (open-compiled initial-config query strategy*))

(define (open-forms forms
                    #:source-mode [source-mode default-source-mode]
                    #:compile-profile [compile-profile #f]
                    #:search-strategy [strategy default-search-strategy])
  (open-source (forms->source-string forms)
               #:source-mode source-mode
               #:compile-profile compile-profile
               #:search-strategy strategy))

(define (model-session-current-step session)
  (match-define (model-session (zipper _ curr _ _) _ _ _) session)
  curr)

(define (model-session-current-step-name session)
  (model-step-name (model-session-current-step session)))

(define (model-step-kind step)
  (match (model-step-name step)
    ["Initialize Program" 'initialization]
    ["advance" 'public-operation]
    [_ 'reduction]))

(define (model-session-current-step-kind session)
  (model-step-kind (model-session-current-step session)))

(define (model-session-current-config session)
  (model-step-config (model-session-current-step session)))

(define (model-session-current-picture session)
  (cfg->operational-picture (model-session-current-config session)
                            (query-info-variables (model-session-query session))
                            (search-strategy? (model-session-search-strategy session))))

(define (model-session-current-answer-nodes session)
  (committed-answer-nodes (model-session-current-config session)
                         (query-info-variables (model-session-query session))))

(define (model-session-current-answers session)
  (answer-nodes->reified (model-session-current-answer-nodes session)))

(define (model-session-current-host-answers session)
  (answer-nodes->host-values (model-session-current-answer-nodes session)))

(define (model-session-step-index session)
  (zipper-idx (model-session-zipper session)))

;; Count only the history prefix ending at the displayed configuration. Moving
;; back excludes future entries; replaying them restores the same counts.
(define (model-session-operation-counts session)
  (match-define (zipper previous current _ _) (model-session-zipper session))
  (for/fold ([reductions 0] [advances 0]) ([step (in-list (cons current previous))])
    (match (model-step-kind step)
      ['initialization (values reductions advances)]
      ['reduction (values (add1 reductions) advances)]
      ['public-operation (values reductions (add1 advances))])))

(define (model-session-status session)
  (configuration-status (model-session-current-config session)))

(define (model-session-done? session)
  (match-define (model-session (zipper _ _ next _) _ _ _) session)
  (and (null? next) (eq? (model-session-status session) 'complete)))

(define (model-session-step session)
  (match-define (model-session zipper step-once _ strategy) session)
  (define-values (maybe-next zipper^)
    (zipper-forward zipper))
  (cond
    [(model-step? maybe-next)
     (struct-copy model-session session [zipper zipper^])]
    [else
     (define current (model-step-config (zipper-curr zipper)))
     (define status (configuration-status current))
     (define successors
       (match status
         ['paused
          (list (list "advance" (advance-configuration current)))]
         ['complete '()]
         ['running (step-once current)]
         ['stuck (error 'model-session-step "stuck ~a configuration: ~e"
                        (if (strict-search? strategy) "matrix" "lattice") current)]))
     (match successors
       ['() #:when (eq? status 'complete) session]
       ['() (error 'model-session-step "no successor for ~a configuration: ~e" status current)]
       [(list (list name new-config))
        (struct-copy model-session session
                     [zipper (zipper-add zipper
                                         (model-step name new-config))])]
       [_ (error 'model-session-step
                 "expected a deterministic successor under search strategy ~e"
                 (search-strategy->jsexpr
                  (model-session-search-strategy session)))])]))

(define (model-session-back session)
  (define-values (_maybe-back zipper^)
    (zipper-back (model-session-zipper session)))
  (struct-copy model-session session [zipper zipper^]))

(define (model-session-reset session)
  (match-define (model-session (zipper prev curr _ _) _ _ _) session)
  (define init-step
    (cond
      [(pair? prev)
       (for/first ([entry (in-list (reverse prev))]
                   #:when (model-step? entry))
         entry)]
      [(model-step? curr) curr]
      [else #f]))
  (unless (model-step? init-step)
    (error 'model-session-reset
           "session has no initial program to reset to"))
  (struct-copy model-session session
               [zipper (zipper-add (make-empty-zipper) init-step)]))
