#lang racket

(require racket/match
         redex/reduction-semantics
         "../src/sexpr-read.rkt"
         "../src/transpiler.rkt"
         (only-in "../src/search-lattice/reduction-relations/search-base-seq-red.rkt"
                  search-base-seq-red)
         (only-in "../src/search-lattice/reduction-relations/search-dfs-seq-red.rkt"
                  search-dfs-seq-red))

(provide branch-fresh-program
         shared-fresh-program
         parse-witness
         print-trace)

;; For the GUI Redex stepper in DrRacket, evaluate one of:
;;   (require redex)
;;   (traces search-dfs-seq-red (parse-witness branch-fresh-program))
;;   (traces search-dfs-seq-red (parse-witness shared-fresh-program))
;;
;; The shell here is headless, so this file only compiles/prints terminal traces.

(define branch-fresh-program
  "(run* (q)
  (conde
    [(fresh (x)
       (== x 'left)
       (== q 'left))]
    [(fresh (x)
       (== x 'right)
       (== q 'right))]))")

(define shared-fresh-program
  "(run* (q)
  (fresh (x)
    (conde
      [(== x 'left)
       (== q 'left)]
      [(== x 'right)
       (== q 'right)])))")

(define (parse-witness src)
  (define-values (cfg _html)
    (parse-prog/canonical (read-all-sexprs (open-input-string src))))
  cfg)

(define (print-trace src [step-rel search-dfs-seq-red] [limit 24])
  (define (loop cfg i)
    (printf "CFG ~a:\n~s\n" i cfg)
    (match (apply-reduction-relation/tag-with-names step-rel cfg)
      ['() (void)]
      [(list (list name cfg^))
       (printf "STEP ~a: ~a\n\n" i name)
       (when (< i limit)
         (loop cfg^ (add1 i)))]
      [next*
       (printf "NONDETERMINISTIC:\n~s\n" next*)]))
  (loop (parse-witness src) 0))

(module+ main
  (define choice
    (match (current-command-line-arguments)
      [(vector "shared") 'shared]
      [_ 'branch]))
  (match choice
    ['shared
     (displayln "Printing shared-fresh witness trace.")
     (displayln "In DrRacket, run `(traces search-dfs-seq-red (parse-witness shared-fresh-program))`.")
     (print-trace shared-fresh-program)]
    ['branch
     (displayln "Printing branch-local fresh witness trace.")
     (displayln "In DrRacket, run `(traces search-dfs-seq-red (parse-witness branch-fresh-program))`.")
     (print-trace branch-fresh-program)]))
