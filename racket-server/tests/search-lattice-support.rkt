#lang racket

(require rackunit
         redex/reduction-semantics
         "../src/search-lattice/wf/all.rkt")

(provide final-program?
         progress?
         unique-decomposition?
         states-wf?
         shape-closed?
         tagged-successor-name
         tagged-successor-cfg
         sigma-a
         sigma-b
         sigma-s
         gamma-delay
         cfg-disj
         cfg-delay-goal
         cfg-flip
         cfg-rail
         cfg-mixed-answer
         cfg-mixed-fail
         cfg-call
         cfg-call-branch
         cfg-call-rail)

(define (final-frontier? f)
  (match f
    ['(empty-tree) #t]
    [`(Freshened ,_ ,_ ,inner) (final-frontier? inner)]
    [`((Freshened ,_ ,_ ,_) + ,rest) (final-frontier? rest)]
    [`((⊤ ,_) + ,rest) (final-frontier? rest)]
    [`(Bounced + ,rest) (final-frontier? rest)]
    [_ #f]))

(define (final-program? prog)
  (match prog
    [`(,_gamma ,f)
     (final-frontier? f)]
    [f
     (final-frontier? f)]
    [_ #f]))

(define (progress? rel prog)
  (or (final-program? prog)
      (not (null? (apply-reduction-relation rel prog)))))

(define (unique-decomposition? rel prog)
  (define next* (apply-reduction-relation rel prog))
  (if (final-program? prog)
      (null? next*)
      (= (length next*) 1)))

(define (states-in datum [acc '()])
  (match datum
    [`(state ,_sub ,_dis ,_c ,_trail ,_tag) (cons datum acc)]
    ['() acc]
    [(cons a d) (states-in a (states-in d acc))]
    [_ acc]))

(define (states-wf? prog)
  (for/and ([st (in-list (states-in prog))])
    (judgment-holds (wf-state? ,st))))

(define (shape-closed? matcher rel prog)
  (for/and ([prog^ (in-list (apply-reduction-relation rel prog))])
    (matcher prog^)))

(define (tagged-successor-name succ)
  (match succ
    [(list name _cfg) (~a name)]
    [_ "<unknown>"]))

(define (tagged-successor-cfg succ)
  (match succ
    [(list _name cfg) cfg]
    [_ succ]))

(define sigma-a
  (term (state () () () () (label "a"))))

(define sigma-b
  (term (state () () () () (label "b"))))

(define sigma-s
  (term (state () () () () (label "s"))))

(define gamma-delay
  (term ((r:delay ()
                  (suspend (succeed (label "inner"))
                           (label "zz"))))))

(define cfg-disj
  (term ((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))))

(define cfg-delay-goal
  (term ((suspend (succeed (label "inner")) (label "delay"))
         ,sigma-s)))

(define cfg-flip
  (term ((delay (empty-tree))
         <-+
         (⊤ ,sigma-b))))

(define cfg-rail
  (term ((delay (empty-tree))
         <-+
         (⊤ ,sigma-b))))

(define cfg-mixed-answer
  (term ((((⊤ ,sigma-a) <-+ (⊤ ,sigma-b))
          × (succeed (label "k"))
          ())
         <-+
         (empty-tree))))

(define cfg-mixed-fail
  (term ((((empty-tree) <-+ (⊤ ,sigma-b))
          × (succeed (label "k"))
          ())
         <-+
         (empty-tree))))

(define cfg-call
  (term (,gamma-delay
         ((r:delay (label "call")) ,sigma-a))))

(define cfg-call-branch
  (term (,gamma-delay
         (((r:delay (label "call")) ,sigma-a)
          <-+
          (⊤ ,sigma-b)))))

(define cfg-call-rail
  (term (,gamma-delay
         (((r:delay (label "call")) ,sigma-a)
          <-+
          (⊤ ,sigma-b)))))
