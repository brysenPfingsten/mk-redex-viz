#lang racket

(require "./ast.rkt"
         "./profile.rkt")

(provide prepare-mini-program)

(define (conj-goals/left goals)
  (match goals
    [(list goal) goal]
    [(cons goal (cons next more))
     (conj-goals/left
      (cons (conj goal next) more))]
    [_ (error 'conj-goals/left "expected a non-empty goal sequence")]))

(define (combine-conj goals assoc)
  (match goals
    [(list goal) goal]
    [(cons goal (cons next more))
     (if (equal? assoc "left")
         (combine-conj (cons (conj goal next) more) assoc)
         (conj goal (combine-conj (cons next more) assoc)))]
    [_ (error 'combine-conj "expected a non-empty goal sequence")]))

(define (combine-disj goals assoc)
  (match goals
    [(list goal) goal]
    [(cons goal (cons next more))
     (if (equal? assoc "left")
         (combine-disj (cons (disj goal next) more) assoc)
         (disj goal (combine-disj (cons next more) assoc)))]
    [_ (error 'combine-disj "expected a non-empty clause sequence")]))

(define (flatten-conj-tree goal [acc '()])
  (match goal
    [(conj g1 g2)
     (flatten-conj-tree g1 (flatten-conj-tree g2 acc))]
    [_ (cons goal acc)]))

(define (contains-delay-goal? goal [seen #f])
  (match goal
    [(delay-goal g) (contains-delay-goal? g #t)]
    [(compiled-delay-goal g) (contains-delay-goal? g #t)]
    [(fresh _ g) (contains-delay-goal? g seen)]
    [(conde clauses)
     (for/or ([clause (in-list clauses)])
       (contains-delay-goal? clause seen))]
    [(conj g1 g2)
     (or (contains-delay-goal? g1 seen)
         (contains-delay-goal? g2 seen))]
    [(disj g1 g2)
     (or (contains-delay-goal? g1 seen)
         (contains-delay-goal? g2 seen))]
    [_ seen]))

(define (wrap-relcalls goal [wrapper delay-goal])
  (match goal
    [(fresh vars g)
     (fresh vars (wrap-relcalls g wrapper))]
    [(conj g1 g2)
     (conj (wrap-relcalls g1 wrapper) (wrap-relcalls g2 wrapper))]
    [(disj g1 g2)
     (disj (wrap-relcalls g1 wrapper) (wrap-relcalls g2 wrapper))]
    [(delay-goal g)
     (delay-goal (wrap-relcalls g wrapper))]
    [(compiled-delay-goal g)
     (compiled-delay-goal (wrap-relcalls g wrapper))]
    [(relcall _ _)
     (wrapper goal)]
    [_ goal]))

(define (wrap-disjs goal [wrapper delay-goal])
  (match goal
    [(fresh vars g)
     (fresh vars (wrap-disjs g wrapper))]
    [(conj g1 g2)
     (conj (wrap-disjs g1 wrapper) (wrap-disjs g2 wrapper))]
    [(disj g1 g2)
     (wrapper (disj (wrap-disjs g1 wrapper) (wrap-disjs g2 wrapper)))]
    [(delay-goal g)
     (delay-goal (wrap-disjs g wrapper))]
    [(compiled-delay-goal g)
     (compiled-delay-goal (wrap-disjs g wrapper))]
    [_ goal]))

(define/match (surface-goal->micro goal profile)
  [(goal (and profile (compile-profile conj-assoc disj-assoc _)))
   (match goal
     [(fresh vars g)
      (fresh vars (surface-goal->micro g profile))]
     [(conde clauses)
      (combine-disj
       (for/list ([clause (in-list clauses)])
         (surface-goal->micro clause profile))
       disj-assoc)]
     [(conj _ _)
      (combine-conj
       (for/list ([piece (in-list (flatten-conj-tree goal))])
         (surface-goal->micro piece profile))
       conj-assoc)]
     [(disj g1 g2)
      (disj (surface-goal->micro g1 profile)
            (surface-goal->micro g2 profile))]
     [(delay-goal g)
      (delay-goal (surface-goal->micro g profile))]
     [_ goal])])

(define (apply-delay-placement goal placement [wrapper delay-goal])
  (case (string->symbol placement)
    [(relcall) (wrap-relcalls goal wrapper)]
    [(disj) (wrap-disjs goal wrapper)]
    [else goal]))

(define/match (mini-ast->normalized-micro ast profile)
  [((prog rels (run n q goal))
    (and profile (compile-profile _ _ delay-placement)))
   (define normalized-rels
     (for/list ([rel (in-list rels)])
       (match-define (defrel name lop rel-goal) rel)
       (define normalized-goal (surface-goal->micro rel-goal profile))
       (defrel name
               lop
               (if (equal? delay-placement "relbody")
                   (compiled-delay-goal normalized-goal)
                   (apply-delay-placement normalized-goal
                                          delay-placement
                                          compiled-delay-goal)))))
   (prog normalized-rels
         (run n
              q
              (apply-delay-placement (surface-goal->micro goal profile)
                                     delay-placement
                                     compiled-delay-goal)))]
  [(ast _)
   (error 'mini-ast->normalized-micro
          "unexpected source AST shape: ~e"
          ast)])

(define (relbody-certifies? goal)
  (match goal
    [(compiled-delay-goal inner)
     (not (contains-delay-goal? inner))]
    [_ #f]))

(define (relcall-certifies? goal)
  (match goal
    [(delay-goal inner)
     (and (relcall? inner) #t)]
    [(compiled-delay-goal inner)
     (and (relcall? inner) #t)]
    [(relcall _ _) #f]
    [(fresh _ g) (relcall-certifies? g)]
    [(conj g1 g2)
     (and (relcall-certifies? g1)
          (relcall-certifies? g2))]
    [(disj g1 g2)
     (and (relcall-certifies? g1)
          (relcall-certifies? g2))]
    [_ #t]))

(define (disj-certifies? goal)
  (match goal
    [(or (delay-goal (disj g1 g2))
         (compiled-delay-goal (disj g1 g2)))
     (and (disj-certifies? g1)
          (disj-certifies? g2))]
    [(or (delay-goal _)
         (compiled-delay-goal _)
         (disj _ _))
     #f]
    [(fresh _ g) (disj-certifies? g)]
    [(conj g1 g2)
     (and (disj-certifies? g1)
          (disj-certifies? g2))]
    [_ #t]))

(define/match (certify-guarded-mini-program ast profile)
  [((prog rels (run _ _ query-goal)) (compile-profile _ _ placement))
   (case (string->symbol placement)
     [(relbody)
      (unless (andmap (lambda (rel)
                        (match rel
                          [(defrel _ _ goal) (relbody-certifies? goal)]
                          [_ #f]))
                      rels)
        (error 'certify-guarded-mini-program
               "relbody profile did not produce delayed relation bodies"))
      (when (contains-delay-goal? query-goal)
        (error 'certify-guarded-mini-program
               "relbody profile should not delay the query"))]
     [(relcall)
      (unless (andmap (lambda (rel)
                        (match rel
                          [(defrel _ _ goal) (relcall-certifies? goal)]
                          [_ #f]))
                      rels)
        (error 'certify-guarded-mini-program
               "relcall profile did not delay every relation call in bodies"))
      (unless (relcall-certifies? query-goal)
        (error 'certify-guarded-mini-program
               "relcall profile did not delay every relation call in query"))]
     [(disj)
      (unless (andmap (lambda (rel)
                        (match rel
                          [(defrel _ _ goal) (disj-certifies? goal)]
                          [_ #f]))
                      rels)
        (error 'certify-guarded-mini-program
               "disj profile did not delay every disjunction in bodies"))
      (unless (disj-certifies? query-goal)
        (error 'certify-guarded-mini-program
               "disj profile did not delay every disjunction in query"))]
     [else
      (error 'certify-guarded-mini-program
             "unknown delay placement ~e"
             placement)])
   ast]
  [(ast _)
   (error 'certify-guarded-mini-program
          "unexpected normalized program shape: ~e"
          ast)])

(define (parse-run/surface-mini r)
  (match r
    [`(run ,n (,q ..1) . ,gs)
     (run n (map var q) (conj-goals/left (map parse-goal/surface-mini gs)))]
    [`(run* (,q ..1) . ,gs)
     (run +inf.0 (map var q) (conj-goals/left (map parse-goal/surface-mini gs)))]
    [_ (error 'parse-run/surface-mini "invalid run form: ~e" r)]))

(define (parse-relation-def/surface-mini a-relation)
  (match a-relation
    [`(defrel (,r . ,params) . ,gs)
     (defrel (relname r)
             (map var params)
             (conj-goals/left (map parse-goal/surface-mini gs)))]
    [_ (error 'parse-relation-def/surface-mini
              "invalid defrel form: ~e"
              a-relation)]))

(define (parse-goal/surface-mini goal)
  (match goal
    [`(fresh ,vars . ,goals)
     (fresh (map var vars) (conj-goals/left (map parse-goal/surface-mini goals)))]
    [`(conde . ,clauses)
     (conde (map parse-clause/surface-mini clauses))]
    [`(== ,t1 ,t2)
     (unify (parse-term t1) (parse-term t2))]
    [`(=/= ,t1 ,t2)
     (diseq (parse-term t1) (parse-term t2))]
    ['succeed (succeed)]
    ['fail (fail)]
    [`(,r . ,terms)
     #:when (symbol? r)
     (relcall (relname r) (map parse-term terms))]
    [_ (error 'parse-goal/surface-mini "invalid goal form: ~e" goal)]))

(define (parse-clause/surface-mini goals)
  (conj-goals/left (map parse-goal/surface-mini goals)))

(define (prepare-mini-program defrels run-expr profile)
  (define display-ast
    (prog (map parse-relation-def/surface-mini defrels)
          (parse-run/surface-mini run-expr)))
  (define normalized-ast
    (certify-guarded-mini-program
     (mini-ast->normalized-micro display-ast profile)
     profile))
  (values normalized-ast display-ast))
