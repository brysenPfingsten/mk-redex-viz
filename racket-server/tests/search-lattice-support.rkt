#lang racket

(require rackunit
         redex/reduction-semantics
         "../src/search-lattice/wf/all.rkt")

(provide final-program?
         progress?
         unique-decomposition?
         states-wf?
         shape-closed?
         produced-answer-spine-only?
         invariant-closed?
         tagged-successor-name
         tagged-successor-cfg
         sigma-a
         sigma-b
         sigma-s
         gamma-delay
         cfg-disj
         cfg-delay-goal
         delayed-left-search
         scoped-delayed-left-search
         cfg-scoped-delay-through-conj
         cfg-flip
         cfg-scoped-flip
         cfg-rail
         cfg-scoped-rail
         cfg-mixed-answer
         cfg-mixed-fail
         cfg-call
         cfg-call-branch
         cfg-call-rail)

(define (final-frontier? f)
  (match f
    ['(empty-tree) #t]
    [`(⊤ ,_) #t]
    [(or (list 'FreshenedTree _ inner _)
         (list 'FreshenedShell _ inner _))
     (final-frontier? inner)]
    [`(Bounced ,inner) (final-frontier? inner)]
    [`(,_ + ,rest) (final-frontier? rest)]
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

(define (state-wf? st)
  (match st
    [`(state ,sub ,dis ,c ,trail ,tag)
     (judgment-holds (wf-state? (state ,sub ,dis ,c ,trail ,tag)))]
    [_ #f]))

(define (states-wf? prog)
  (for/and ([st (in-list (states-in prog))])
    (state-wf? st)))

(define (shape-closed? matcher rel prog)
  (for/and ([prog^ (in-list (apply-reduction-relation rel prog))])
    (matcher prog^)))

(define (produced-answer-spine-only? prog
                                    [inside-branch? #f])
  (match prog
    [`(,_gamma ,cfg)
     (produced-answer-spine-only? cfg inside-branch?)]
    [(or (list 'FreshenedTree _ inner _)
         (list 'FreshenedShell _ inner _))
     (produced-answer-spine-only? inner inside-branch?)]
    [`(Bounced ,inner)
     (produced-answer-spine-only? inner inside-branch?)]
    [`(delay ,inner)
     (produced-answer-spine-only? inner inside-branch?)]
    [`(,cfg_i × ,_g ,_c)
     (produced-answer-spine-only? cfg_i inside-branch?)]
    [`(,left + ,right)
     (and (not inside-branch?)
          (produced-answer-spine-only? left inside-branch?)
          (produced-answer-spine-only? right inside-branch?))]
    [`(,left <-+ ,right)
     (and (produced-answer-spine-only? left #t)
          (produced-answer-spine-only? right #t))]
    [`(,left +-> ,right)
     (and (produced-answer-spine-only? left #t)
          (produced-answer-spine-only? right #t))]
    [_ #t]))

(define (invariant-closed? invariant? rel prog)
  (and (invariant? prog)
       (for/and ([prog^ (in-list (apply-reduction-relation rel prog))])
         (invariant? prog^))))

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

(define delayed-left-search
  (term (delay ((succeed (label "late")) ,sigma-s))))

(define scoped-delayed-left-search
  (term (FreshenedTree (u:0)
                       (delay ((succeed (label "late")) ,sigma-s))
                       (label "fresh"))))

(define cfg-scoped-delay-through-conj
  (term (,scoped-delayed-left-search
         × (succeed (label "k"))
         ())))

(define cfg-flip
  (term (,delayed-left-search
         <-+
         (⊤ ,sigma-b))))

(define cfg-scoped-flip
  (term (,scoped-delayed-left-search
         <-+
         (⊤ ,sigma-b))))

(define cfg-rail
  (term (,delayed-left-search
         <-+
         (⊤ ,sigma-b))))

(define cfg-scoped-rail
  (term (,scoped-delayed-left-search
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
