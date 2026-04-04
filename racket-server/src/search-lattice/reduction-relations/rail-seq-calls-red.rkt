#lang racket

(require redex/reduction-semantics
         "../languages/rail-calls-lang.rkt"
         "./private/step-utils.rkt"
         "./search-base-seq-calls-red.rkt")

(provide rail-early-relcall-local/base
         rail-early-relcall-frontier/base
         rail-early-relcall-red
         step-once)

(check-redundancy #t)

(define lifted-search-early-relcall-red
  (extend-reduction-relation search-early-relcall-red rail-relcall-lang))

(define rail-early-relcall-local/base
  (reduction-relation
   rail-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx ((in-hole FreshCtx (delay runnable-search_1)) <-+ search_2))))
        (Γ (in-hole ShellCtx
                      (in-hole RailTailCtx
                               (delay ((in-hole FreshCtx runnable-search_1)
                                       +->
                                       search_2)))))
        "enter-right"]
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_2 +-> (in-hole FreshCtx (delay runnable-search_1))))))
        (Γ (in-hole ShellCtx
                      (in-hole RailTailCtx
                               (delay (search_2
                                       <-+
                                       (in-hole FreshCtx runnable-search_1))))))
        "return-left"]))

(define rail-early-relcall-frontier/base
  (reduction-relation
   rail-relcall-lang
   #:domain config
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> ((answers_i <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole ShellCtx (answers_i + (in-hole RailTailCtx (search_left +-> (search_mid <-+ search_right))))))
        "bubble-right-left-answer"]
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (answers_i <-+ search_right)))))
        (Γ (in-hole ShellCtx (answers_i + (in-hole RailTailCtx (search_left +-> search_right)))))
        "promote-right-left-answer"]
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (((in-hole FreshCtx (empty-tree)) <-+ search_mid) <-+ search_right)))))
        (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (search_mid <-+ search_right)))))
        "bubble-right-left-fail"]
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> ((in-hole FreshCtx (empty-tree)) <-+ search_right)))))
        (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> search_right))))
        "skip-right-left-fail"]
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> answers_i))))
        (Γ (in-hole ShellCtx (answers_i + (in-hole RailTailCtx search_left))))
        "promote-right-answer"]
   [--> (Γ (in-hole ShellCtx (in-hole RailTailCtx (search_left +-> (in-hole FreshCtx (empty-tree))))))
        (Γ (in-hole ShellCtx (in-hole RailTailCtx search_left)))
        "skip-right-fail"]))

(define rail-early-relcall-red
  (union-reduction-relations
   lifted-search-early-relcall-red
   rail-early-relcall-local/base
   rail-early-relcall-frontier/base))

(define (step-once prog)
  (step-once/deterministic rail-early-relcall-red prog))
