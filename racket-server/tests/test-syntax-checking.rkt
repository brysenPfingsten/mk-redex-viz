#lang racket

(require rackunit
         rackunit/text-ui
         redex/reduction-semantics
         "../src/search-lattice/languages/canonical-core-lang.rkt"
         "../src/sexpr-read.rkt"
         "../src/syntax-checking.rkt"
         "../src/transpiler.rkt")

(define WELL-FORMED-CONFIG (term (() (empty-tree) (empty-stream))))
(define BAD-FORMED-CONFIG
  (term (() ((u:1 =? (sym "a") (label "t"))
             (state () () () () (label "s")))
            (empty-stream))))

(define-test-suite WELL-FORMED
  (test-case "Well-formed canonical config is accepted"
             (check-true (redex-match? canonical-core-lang config WELL-FORMED-CONFIG))
             (check-true (canonical-core-shape? WELL-FORMED-CONFIG))
             (check-true (canonical-well-formed? WELL-FORMED-CONFIG))
             (check-not-exn (λ () (check-canonical-well-formed WELL-FORMED-CONFIG)) ""))

  (test-case "Malformed canonical config is rejected"
             (check-true (redex-match? canonical-core-lang config BAD-FORMED-CONFIG))
             (check-true (canonical-core-shape? BAD-FORMED-CONFIG))
             (check-false (canonical-well-formed? BAD-FORMED-CONFIG))
             (check-exn exn:fail?
                        (λ () (check-canonical-well-formed BAD-FORMED-CONFIG))))

  (test-case "Canonical core gate rejects malformed canonical core program"
             (define bad-canonical
               '(() ((u:1 =? (sym "a") (label "t"))
                     (state () () () () (label "s")))
                    (empty-stream)))
             (check-true (canonical-core-shape? bad-canonical))
             (check-false (canonical-well-formed? bad-canonical))
             (check-exn exn:fail?
                        (λ () (check-canonical-well-formed bad-canonical))))

  (test-case "Canonical gate accepts non-core shape directly"
             (define non-core-canonical '(() (delay (empty-tree)) (empty-stream)))
             (check-false (canonical-core-shape? non-core-canonical))
             (check-true (canonical-target-in-domain? non-core-canonical "canonical/config"))
             (check-true (canonical-target-well-formed? non-core-canonical "canonical/config"))
             (check-not-exn
              (λ () (check-canonical-well-formed non-core-canonical))))

  (test-case "Canonical gate rejects out-of-target-domain term"
             (define out-of-domain-canonical '(bogus))
             (check-false (canonical-target-in-domain? out-of-domain-canonical "canonical/config"))
             (check-exn exn:fail?
                        (λ () (check-canonical-well-formed out-of-domain-canonical))))

  (test-case "Surface parser emits canonical config accepted by canonical gate"
             (define src
               "(defrel (same x y) (== x y))
(run* (q) (same q 'cat))")
             (define-values (canonical _html)
               (parse-prog/canonical (read-all-sexprs (open-input-string src))))
             (check-true (canonical-target-in-domain? canonical "canonical/config"))
             (check-true (canonical-target-well-formed? canonical "canonical/config"))
             (check-not-exn
              (λ () (check-canonical-well-formed canonical)))))

(define GOOD-SYNTAX-PROG 
  "
(defrel (foo x) (== x 'bar))
(run* (q) (foo q))
"
  )
(define BAD-SYNTAX-PROG
  "
(defrel (foo x) (== x 'foo))
(run* (foo q))
"
  )

(define LOOP-PROG
  "
(defrel (loopo x) (loopo x))
(run* (q) (loopo q))
"
  )

(define ARITY-MISMATCH
  "
(defrel (foo x y)
  (== x 'x))
(run* (q r s t) (foo q r s t))
")

(define-test-suite SYNTAX-CHECKING
  (test-not-exn "Syntactically Valid Program Does Not Throw Error"
                (λ () (check-syntax-capture-error GOOD-SYNTAX-PROG)))

  (test-exn "Syntacically Invalid Program Throws Error"
            exn:fail?
            (λ () (check-syntax-capture-error BAD-SYNTAX-PROG)))
  ;; "syntax-checker: run*: expected more terms starting with goal expression
  ;;  at: ()
  ;;  within: (run* (foo q))
  ;;  in: (run* (foo q))
  ;;  parsing context:
  ;;  while parsing (run* (<id> ...+) <goal> ...+)
  ;;  term: (run* (foo q))
  ;;  location: syntax-checker"
  (test-not-exn "Non Terminating Program Does Not Evaluate Forever"
                (λ ()  (check-syntax-capture-error LOOP-PROG)))

  (test-exn "Arity Mismatch is Detected"
            exn:fail?
            (λ () (check-syntax-capture-error ARITY-MISMATCH))))

(define/provide-test-suite SYNTAX-CHECKER
  #:before (thunk (displayln "Running Rests For Syntax Checking..."))
  #:after  (thunk (displayln "Finished Running Tests For Syntax Checking."))
  WELL-FORMED
  SYNTAX-CHECKING)

(module+ test
  (run-tests SYNTAX-CHECKER))
