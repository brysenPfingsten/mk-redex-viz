#lang racket

(require redex/reduction-semantics
         "../languages/disj-lang.rkt"
         (only-in "../languages/core-lang.rkt" fresh-tree-prefix->shell-prefix)
         (prefix-in core: "./core-red.rkt"))

(provide local/base
         shell/base)

(check-redundancy #t)

(define frontier/base
  (let ([frontier/local-base
         (reduction-relation
           disj-lang
           #:domain cfg
           [--> (in-hole FreshCtx_1 ((settled_1 <-+ search_mid) <-+ search_right))
                (fresh-tree-prefix->shell-prefix
                 (in-hole FreshCtx_1 (settled_1 <-+ (search_mid <-+ search_right))))
                "reassociate-left-result"]
           [--> (in-hole FreshCtx_1 ((in-hole FreshCtx_2 (⊤ σ)) <-+ search_right))
                (fresh-tree-prefix->shell-prefix
                 (in-hole FreshCtx_1 ((in-hole FreshCtx_2 (⊤ σ)) + search_right)))
                "promote-left-answer"]
           [--> (in-hole FreshCtx_1 ((in-hole FreshCtx_2 (empty-tree)) <-+ search_right))
                (fresh-tree-prefix->shell-prefix (in-hole FreshCtx_1 search_right))
                "skip-left-fail"])])
    (context-closure frontier/local-base disj-lang ShellCtx)))

(define local/base
  (let ([goal-local/base
         (reduction-relation
          disj-lang
          #:domain cfg
          [--> (in-hole LocalCtx ((g_1 ∨ g_2 tag) σ))
               (in-hole LocalCtx ((g_1 σ) <-+ (g_2 σ)))
               "expand-disjunction"])])
    (union-reduction-relations
     (context-closure
      (extend-reduction-relation core:local/base disj-lang)
      disj-lang
      LocalCtx)
     goal-local/base)))

(define shell/base
 (union-reduction-relations
   (extend-reduction-relation core:shell/base disj-lang)
   frontier/base))
