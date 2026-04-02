#lang racket
(require racket/match
         "../Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/model-registry.rkt"
         "../Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/transpiler.rkt")

(define prog-str #<<P
(defrel (fives x)
  (conde
    [(fives x)]
    [(== x 'five)]))

(defrel (fours x)
  (conde
    [(fours x)]
    [(== x 'four)]))

(run 8 (q)
  (conde
    [(fives q)]
    [(fours q)]))
P
)

(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr) '() (cons expr (read-all port)))))

(define sexprs (read-all (open-input-string prog-str)))
(define-values (prog _html) (parse-prog/canonical sexprs))
(define step-once (lookup-model-step-once "microKanren-rail"))

(define st prog)
(for ([i (in-range 1 30)])
  (define succs (step-once st))
  (cond
    [(null? succs)
     (printf "step ~a: STOP\n" i)
     (set! i 100)]
    [else
     (match-define (list nm nxt) (first succs))
     (when (<= 18 i 22)
       (printf "step ~a rule=~a\n" i nm)
       (printf "state=~s\n" nxt))
     (set! st nxt)]))
