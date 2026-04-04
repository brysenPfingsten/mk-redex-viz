#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         "./private/context-pipeline.rkt"
         "./private/step-utils.rkt"
         "./search-base-fused-calls-red.rkt")

(provide rail-late-relcall-local/base
         rail-late-relcall-frontier/base
         rail-late-relcall-red
         step-once)

(check-redundancy #t)

(define lifted-search-late-relcall-red
  (extend-reduction-relation
   search-late-relcall-red
   rail-relcall-lang))

(define rail-late-relcall-local/base
  (reduction-relation
   rail-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole ShellCtx
                      (in-hole LateCtx
                               (delay ((in-hole FreshCtx runnable-search_1)
                                       +->
                                       search_2)))))
        "enter-right"]
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (search_2 +-> (in-hole FreshCtx (delay runnable-search_1))))))
        (Γ (in-hole ShellCtx
                      (in-hole LateCtx
                               (delay (search_2
                                       <-+
                                       (in-hole FreshCtx runnable-search_1))))))
        "return-left"]))

(define rail-late-relcall-frontier/base
  (reduction-relation
   rail-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> ((answers_i <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole ShellCtx (answers_i + (in-hole LateCtx (search_left +-> (search_mid <-+ search_right))))))
        "bubble-right-left-answer"]
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> (answers_i <-+ search_right)))))
        (Γ (in-hole ShellCtx (answers_i + (in-hole LateCtx (search_left +-> search_right)))))
        "promote-right-left-answer"]
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> (((in-hole FreshCtx (empty-tree)) <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> (search_mid <-+ search_right)))))
        "bubble-right-left-fail"]
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> ((in-hole FreshCtx (empty-tree)) <-+ search_right)))))
        (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> search_right))))
        "skip-right-left-fail"]
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> answers_i))))
        (Γ (in-hole ShellCtx (answers_i + (in-hole LateCtx search_left))))
        "promote-right-answer"]
   [--> (Γ (in-hole ShellCtx (in-hole LateCtx (search_left +-> (in-hole FreshCtx (empty-tree))))))
        (Γ (in-hole ShellCtx (in-hole LateCtx search_left)))
        "skip-right-fail"]))

(define rail-late-relcall-red
  (union-reduction-relations
   lifted-search-late-relcall-red
   rail-late-relcall-local/base
   rail-late-relcall-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-late-relcall-red prog))
