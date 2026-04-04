#lang racket

(require redex/reduction-semantics
         "../languages/rail-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-red.rkt")

(provide rail-early-red
         step-once)

(check-redundancy #t)

(define lifted-search-early-red
  (extend-reduction-relation
   search-early-red
   rail-lang))

(define rail-early-frontier/base
  (reduction-relation
   rail-lang
   #:domain cfg
   [--> (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> ((answers_i <-+ search_mid) <-+ search_right))))
        (in-hole ShellCtx (answers_i + (in-hole RailTailCtx (search_left +-> (search_mid <-+ search_right)))))
        "bubble-right-left-answer"]
   [--> (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (answers_i <-+ search_right))))
        (in-hole ShellCtx (answers_i + (in-hole RailTailCtx (search_left +-> search_right))))
        "promote-right-left-answer"]
   [--> (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (((in-hole FreshCtx (empty-tree)) <-+ search_mid) <-+ search_right))))
        (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (search_mid <-+ search_right))))
        "bubble-right-left-fail"]
   [--> (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> ((in-hole FreshCtx (empty-tree)) <-+ search_right))))
        (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> search_right)))
        "skip-right-left-fail"]
   [--> (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> answers_i)))
        (in-hole ShellCtx (answers_i + (in-hole RailTailCtx search_left)))
        "promote-right-answer"]
   [--> (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (in-hole FreshCtx (empty-tree)))))
        (in-hole ShellCtx (in-hole RailTailCtx search_left))
        "skip-right-fail"]))

(define rail-early-local/under-ShellCtx
  (let ([rail-early-local/base
         (reduction-relation
          rail-lang
          #:domain cfg
          [--> (in-hole RailTailCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))
               (in-hole RailTailCtx
                        (delay ((in-hole FreshCtx runnable-search_1) +-> search_2)))
               "enter-right"]
          [--> (in-hole RailTailCtx (search_2 +-> (in-hole FreshCtx (delay runnable-search_1))))
               (in-hole RailTailCtx
                        (delay (search_2 <-+ (in-hole FreshCtx runnable-search_1))))
               "return-left"])])
    (context-closure rail-early-local/base rail-lang ShellCtx)))

(define rail-early-red
  (union-reduction-relations
   lifted-search-early-red
   rail-early-local/under-ShellCtx
   rail-early-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-early-red prog))
