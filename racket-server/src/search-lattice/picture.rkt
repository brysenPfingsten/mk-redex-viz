#lang racket

(require racket/hash
         "./answer-node.rkt")

(provide cfg->operational-picture
         cfg->extensional-picture
         program-query-var-count)

(define (normalize-config cfg)
  (match cfg
    [`(,_gamma ,_f) cfg]
    [f f]))

(define (project-config-tree cfg)
  (match (normalize-config cfg)
    [`(,_gamma ,f) f]
    [f f]
    [_ '(empty-tree)]))

(define (empty-node)
  (hasheq 'name "Empty"
          'renderRole "terminal"))

(define (freshened-node c-intro child tag)
  (hasheq 'name "Freshened"
          'renderRole "freshened"
          'id (label->visible-id tag)
          'vars (map term->visible-json c-intro)
          'activeChildIndex 0
          'children (list child)))

(define (bounced-node child)
  (hasheq 'name "Bounced"
          'renderRole "bounced"
          'activeChildIndex 0
          'children (list child)))

(define (emit-node left right)
  (hasheq 'name "Emit"
          'renderRole "stream-emit"
          'resolvedChildIndices '(0)
          'resolvedColor "green"
          'activeChildIndex 1
          'children (list left right)))

(define (answer-state-fields σ num-query-variables)
  (for/hasheq ([(k v) (in-hash (state->answer-node σ num-query-variables))]
               #:when (member k '(stateId sub disequalities trail reified)))
    (values k v)))

(define (goal-query-vars g)
  (match g
    [`(∃ ,d ,_ ,_) (length d)]
    [`(suspend ,g_1 ,_) (goal-query-vars g_1)]
    [`(,g_1 ∧ ,g_2 ,_) (max (goal-query-vars g_1)
                            (goal-query-vars g_2))]
    [`(,g_1 ∨ ,g_2 ,_) (max (goal-query-vars g_1)
                            (goal-query-vars g_2))]
    [_ 0]))

(define (num-query-vars/work s)
  (match s
    [(or `(FreshenedTree ,_ ,s_1 ,_)
         `(FreshenedShell ,_ ,s_1 ,_))
     (num-query-vars/work s_1)]
    [`(Bounced ,s_1)
     (num-query-vars/work s_1)]
    [`(,s_1 + ,s_2)
     (max (num-query-vars/work s_1)
          (num-query-vars/work s_2))]
    [`(,g ,_σ)
     (goal-query-vars g)]
    [`(,s_1 × ,g ,_c)
     (max (num-query-vars/work s_1)
          (goal-query-vars g))]
    [`(,s_1 <-+ ,s_2)
     (max (num-query-vars/work s_1)
          (num-query-vars/work s_2))]
    [`(,s_1 +-> ,s_2)
     (max (num-query-vars/work s_1)
          (num-query-vars/work s_2))]
    [`(delay ,s_1)
     (num-query-vars/work s_1)]
    [_ 0]))

(define (num-query-vars cfg)
  (match cfg
    [`(,_gamma ,s)
     (num-query-vars/work s)]
    [_ 0]))

(define (program-query-var-count cfg)
  (num-query-vars cfg))

(define (tree->picture s num-query-variables #:extensional? [extensional? #f])
  (match s
    ['(empty-tree)
     (empty-node)]
    [(or `(FreshenedTree ,c-intro ,s_1 ,tag)
         `(FreshenedShell ,c-intro ,s_1 ,tag))
     (freshened-node
      c-intro
      (tree->picture s_1 num-query-variables #:extensional? extensional?)
      tag)]
    [`(Bounced ,s_1)
     (if extensional?
         (tree->picture s_1 num-query-variables #:extensional? #t)
         (bounced-node
          (tree->picture s_1 num-query-variables #:extensional? #f)))]
    [`(,s_1 + ,s_2)
     (emit-node
      (tree->picture s_1 num-query-variables #:extensional? extensional?)
      (tree->picture s_2 num-query-variables #:extensional? extensional?))]
    [`(,g (state ,sub ,dis ,c ,trail ,tag))
     #:when (not (equal? g '⊤))
     (hash-union (goal->visible-node g)
                 (answer-state-fields
                  `(state ,sub ,dis ,c ,trail ,tag)
                  num-query-variables))]
    [`(,s_1 <-+ ,s_2)
     (hasheq 'name "<-+"
             'renderRole "search-branch"
             'focusColor "#ff8000"
             'activeChildIndex 0
             'children (list (tree->picture s_1 num-query-variables #:extensional? extensional?)
                             (tree->picture s_2 num-query-variables #:extensional? extensional?)))]
    [`(,s_1 +-> ,s_2)
     (hasheq 'name "+->"
             'renderRole "search-branch"
             'focusColor "#ff8000"
             'activeChildIndex 1
             'children (list (tree->picture s_1 num-query-variables #:extensional? extensional?)
                             (tree->picture s_2 num-query-variables #:extensional? extensional?)))]
    [`(,s_1 × ,g ,_c)
     (hasheq 'name "Conjunction"
             'renderRole "search-conjunction"
             'focusColor "blue"
             'activeChildIndex 0
             'children (list (tree->picture s_1 num-query-variables #:extensional? extensional?)
                             (goal->visible-node g)))]
    [`(delay ,s_1)
     (hasheq 'name "Delay"
             'renderRole "delay"
             'activeChildIndex 0
             'children (list (tree->picture s_1 num-query-variables #:extensional? extensional?)))]
    [`(state ,sub ,dis ,c ,trail ,tag)
     (state->answer-node `(state ,sub ,dis ,c ,trail ,tag) num-query-variables)]
    [`(⊤ ,σ)
     (state->answer-node σ num-query-variables)]
    [_ (error 'tree->picture
              "unknown tree/frontier shape: ~e"
              s)]))

(define (cfg->operational-picture cfg [num-query-variables (num-query-vars cfg)])
  (tree->picture (project-config-tree cfg)
                 num-query-variables
                 #:extensional? #f))

(define (cfg->extensional-picture cfg [num-query-variables (num-query-vars cfg)])
  (tree->picture (project-config-tree cfg)
                 num-query-variables
                 #:extensional? #t))
