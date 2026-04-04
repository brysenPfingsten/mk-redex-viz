#lang racket

(require racket/list
         redex/reduction-semantics
         (prefix-in red:
                    "../../../src/search-lattice/reduction-relations/all.rkt")
         "../shared/kernel.rkt")

(provide erase-c
         restore-c
         cfree->current-c-machine
         current-step
         project-observable
         same-observable?
         current-c-scope-agrees?)

(define (config-query cfg)
  (match cfg
    [`(config ,query-u* ,_root-scope ,_term)
     query-u*]
    [_ (error 'config-query "unsupported config: ~e" cfg)]))

(define (config-root-scope cfg)
  (match cfg
    [`(config ,_query-u* ,root-scope ,_term)
     root-scope]
    [_ (error 'config-root-scope "unsupported config: ~e" cfg)]))

(define (config-term cfg)
  (match cfg
    [`(config ,_query-u* ,_root-scope ,term)
     term]
    [_ (error 'config-term "unsupported config: ~e" cfg)]))

(define (restore-state state ambient)
  (match state
    [`(state ,sub ,dis ,trail ,tag)
     `(state ,sub ,dis ,ambient ,trail ,tag)]
    [_ (error 'restore-state "unsupported c-free state: ~e" state)]))

(define (restore-search term ambient)
  (match term
    ['empty-tree
     'empty-tree]
    [`(⊤ ,state)
     `(⊤ ,(restore-state state ambient))]
    [`(,g (state ,sub ,dis ,trail ,tag))
     `(,g ,(restore-state `(state ,sub ,dis ,trail ,tag) ambient))]
    [`(FreshenedTree ,intro ,inner ,tag)
     `(FreshenedTree ,intro
                     ,(restore-search inner (scope-append intro ambient))
                     ,tag)]
    [`(delay ,inner)
     `(delay ,(restore-search inner ambient))]
    [`(,left × ,g)
     `(,(restore-search left ambient) × ,g ,ambient)]
    [`(,left <-+ ,right)
     `(,(restore-search left ambient) <-+ ,(restore-search right ambient))]
    [`(,left +-> ,right)
     `(,(restore-search left ambient) +-> ,(restore-search right ambient))]
    [`(,left + ,right)
     `(,(restore-promoted left ambient) + ,(restore-frontier right ambient))]
    [_ (error 'restore-search "unsupported c-free search term: ~e" term)]))

(define (restore-promoted term ambient)
  (match term
    [`(⊤ ,state)
     `(⊤ ,(restore-state state ambient))]
    [`(FreshenedShell ,intro ,inner ,tag)
     `(FreshenedShell ,intro
                      ,(restore-promoted inner (scope-append intro ambient))
                      ,tag)]
    [_ (restore-search term ambient)]))

(define (restore-frontier term ambient)
  (match term
    [`(FreshenedShell ,intro ,inner ,tag)
     `(FreshenedShell ,intro
                      ,(restore-frontier inner (scope-append intro ambient))
                      ,tag)]
    [`(Bounced ,inner)
     `(Bounced ,(restore-frontier inner ambient))]
    [`(,left + ,right)
     `(,(restore-promoted left ambient) + ,(restore-frontier right ambient))]
    [_ (restore-search term ambient)]))

(define (restore-c term ambient)
  (restore-frontier term ambient))

(define (cfree->current-c-machine cfg)
  (match cfg
    [`(config ,query-u* ,root-scope ,term)
     `(config ,query-u* ,root-scope ,(restore-c term root-scope))]
    [_ (error 'cfree->current-c-machine
              "unsupported c-free config: ~e"
              cfg)]))

(define (erase-state state)
  (match state
    [`(state ,sub ,dis ,_c ,trail ,tag)
     `(state ,sub ,dis ,trail ,tag)]
    [_ (error 'erase-state "unsupported current state: ~e" state)]))

(define (erase-term term)
  (match term
    ['empty-tree
     'empty-tree]
    [`(⊤ ,state)
     `(⊤ ,(erase-state state))]
    [`(,g (state ,sub ,dis ,_c ,trail ,tag))
     `(,g (state ,sub ,dis ,trail ,tag))]
    [`(FreshenedTree ,intro ,inner ,tag)
     `(FreshenedTree ,intro ,(erase-term inner) ,tag)]
    [`(FreshenedShell ,intro ,inner ,tag)
     `(FreshenedShell ,intro ,(erase-term inner) ,tag)]
    [`(Bounced ,inner)
     `(Bounced ,(erase-term inner))]
    [`(delay ,inner)
     `(delay ,(erase-term inner))]
    [`(,left × ,g ,_c)
     `(,(erase-term left) × ,g)]
    [`(,left <-+ ,right)
     `(,(erase-term left) <-+ ,(erase-term right))]
    [`(,left +-> ,right)
     `(,(erase-term left) +-> ,(erase-term right))]
    [`(,left + ,right)
     `(,(erase-term left) + ,(erase-term right))]
    [_ (error 'erase-term "unsupported current term: ~e" term)]))

(define (erase-c datum)
  (match datum
    [`(config ,query-u* ,root-scope ,term)
     `(config ,query-u* ,root-scope ,(erase-term term))]
    [_ (erase-term datum)]))

(define (current-step current-cfg)
  (match current-cfg
    [`(config ,query-u* ,root-scope ,term)
     (match (remove-duplicates
             (apply-reduction-relation/tag-with-names red:rail-fused-red term))
       [(list (list name next-term))
        (list (~a name)
              `(config ,query-u* ,root-scope ,next-term))]
       ['()
        #f]
       [_ (error 'current-step
                 "expected deterministic current step, got ~e"
                 current-cfg)])]
    [_ (error 'current-step "unsupported current config: ~e" current-cfg)]))

(define (collect-current-answer-states term [acc '()])
  (match term
    [`(⊤ ,state)
     (cons state acc)]
    [`(FreshenedTree ,_intro ,inner ,_tag)
     (collect-current-answer-states inner acc)]
    [`(FreshenedShell ,_intro ,inner ,_tag)
     (collect-current-answer-states inner acc)]
    [`(Bounced ,inner)
     (collect-current-answer-states inner acc)]
    [`(delay ,inner)
     (collect-current-answer-states inner acc)]
    [`(,left × ,_g ,_c)
     (collect-current-answer-states left acc)]
    [`(,left <-+ ,right)
     (collect-current-answer-states left
                                    (collect-current-answer-states right acc))]
    [`(,left +-> ,right)
     (collect-current-answer-states left
                                    (collect-current-answer-states right acc))]
    [`(,left + ,right)
     (collect-current-answer-states left
                                    (collect-current-answer-states right acc))]
    [_ acc]))

(define (normalize-observable answers*)
  (sort (remove-duplicates answers*)
        string<?
        #:key ~s))

(define (project-observable cfg)
  (match cfg
    [`(config ,query-u* ,_root-scope ,term)
     (normalize-observable
      (for/list ([state (in-list (reverse (collect-current-answer-states term)))])
        (match-define `(state ,sub ,_dis ,_c ,_trail ,_tag) state)
        (reify-query-values query-u* sub)))]
    [_ (error 'project-observable "unsupported config: ~e" cfg)]))

(define (same-observable? cfg-a cfg-b)
  (equal? (project-observable cfg-a)
          (project-observable cfg-b)))

(define (current-c-scope-agrees?/term term ambient)
  (match term
    ['empty-tree
     #t]
    [`(⊤ (state ,_sub ,_dis ,c ,_trail ,_tag))
     (equal? c ambient)]
    [`(,g (state ,_sub ,_dis ,c ,_trail ,_tag))
     #:when (not (equal? g '⊤))
     (equal? c ambient)]
    [`(FreshenedTree ,intro ,inner ,_tag)
     (current-c-scope-agrees?/term inner (scope-append intro ambient))]
    [`(FreshenedShell ,intro ,inner ,_tag)
     (current-c-scope-agrees?/term inner (scope-append intro ambient))]
    [`(Bounced ,inner)
     (current-c-scope-agrees?/term inner ambient)]
    [`(delay ,inner)
     (current-c-scope-agrees?/term inner ambient)]
    [`(,left × ,_g ,c)
     (and (equal? c ambient)
          (current-c-scope-agrees?/term left ambient))]
    [`(,left <-+ ,right)
     (and (current-c-scope-agrees?/term left ambient)
          (current-c-scope-agrees?/term right ambient))]
    [`(,left +-> ,right)
     (and (current-c-scope-agrees?/term left ambient)
          (current-c-scope-agrees?/term right ambient))]
    [`(,left + ,right)
     (and (current-c-scope-agrees?/term left ambient)
          (current-c-scope-agrees?/term right ambient))]
    [_ #f]))

(define (current-c-scope-agrees? cfg)
  (match cfg
    [`(config ,_query-u* ,root-scope ,term)
     (current-c-scope-agrees?/term term root-scope)]
    [_ (error 'current-c-scope-agrees?
              "unsupported config: ~e"
              cfg)]))
