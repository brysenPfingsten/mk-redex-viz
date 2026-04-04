#lang racket

(require redex/reduction-semantics
         "../languages/rail-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-red.rkt")

(provide rail-late-frontier/base
         rail-late-local/under-ShellCtx
         rail-late-red
         step-once)

(check-redundancy #t)

(define lifted-search-late-red
  (extend-reduction-relation
   search-late-red
   rail-lang))

(define rail-late-frontier/base
  (reduction-relation
   rail-lang
   #:domain cfg
   [--> (in-hole ShellCtx (in-hole LateCtx (search_left +-> ((answers_i <-+ search_mid) <-+ search_right))))
        (in-hole ShellCtx (answers_i + (in-hole LateCtx (search_left +-> (search_mid <-+ search_right)))))
        "bubble-right-left-answer"]
   [--> (in-hole ShellCtx (in-hole LateCtx (search_left +-> (answers_i <-+ search_right))))
        (in-hole ShellCtx (answers_i + (in-hole LateCtx (search_left +-> search_right))))
        "promote-right-left-answer"]
   [--> (in-hole ShellCtx (in-hole LateCtx (search_left +-> (((in-hole FreshCtx (empty-tree)) <-+ search_mid) <-+ search_right))))
        (in-hole ShellCtx (in-hole LateCtx (search_left +-> (search_mid <-+ search_right))))
        "bubble-right-left-fail"]
   [--> (in-hole ShellCtx (in-hole LateCtx (search_left +-> ((in-hole FreshCtx (empty-tree)) <-+ search_right))))
        (in-hole ShellCtx (in-hole LateCtx (search_left +-> search_right)))
        "skip-right-left-fail"]
   [--> (in-hole ShellCtx (in-hole LateCtx (search_left +-> answers_i)))
        (in-hole ShellCtx (answers_i + (in-hole LateCtx search_left)))
        "promote-right-answer"]
   [--> (in-hole ShellCtx (in-hole LateCtx (search_left +-> (in-hole FreshCtx (empty-tree)))))
        (in-hole ShellCtx (in-hole LateCtx search_left))
        "skip-right-fail"]))

(define rail-late-local/under-ShellCtx
  (let ([rail-late-local/base
         (reduction-relation
          rail-lang
          #:domain cfg
          [--> (in-hole LateCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))
               (in-hole LateCtx
                        (delay ((in-hole FreshCtx runnable-search_1) +-> search_2)))
               "enter-right"]
          [--> (in-hole LateCtx (search_2 +-> (in-hole FreshCtx (delay runnable-search_1))))
               (in-hole LateCtx
                        (delay (search_2 <-+ (in-hole FreshCtx runnable-search_1))))
               "return-left"])])
    (context-closure rail-late-local/base rail-lang ShellCtx)))

(define rail-late-red
  (union-reduction-relations
   lifted-search-late-red
   rail-late-local/under-ShellCtx
   rail-late-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-late-red prog))
