#lang racket

(require racket/file
         racket/path
         racket/port
         racket/runtime-path
         racket/string
         rackunit
         rackunit/text-ui)

(provide RETIRED-WORK-SYNTAX)

(define-runtime-path SERVER-ROOT "../..")
(define ACTIVE-ROOT
  (simplify-path SERVER-ROOT))
(define ACTIVE-SOURCE-ROOTS
  (list (build-path ACTIVE-ROOT "src")
        (build-path ACTIVE-ROOT "tests")
        (build-path ACTIVE-ROOT "derivations" "experiments")
        (build-path ACTIVE-ROOT "derivations" "test-support")))
(define DORMANT-LANGUAGE-ROOT
  (build-path ACTIVE-ROOT "derivations" "experiments"
              "dormant-branch-semantics" "source" "languages"))
(define SCAN-TEST-PATH "derivations/experiments/architecture-tests.rkt")

(define DORMANT-RELATION-DEPENDENCY-PATHS
  '("derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-relcall-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/rail-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/rail-relcall-red.rkt"))

(define RETAINED-JOIN-SEAM-CONSUMERS
  '("derivations/experiments/early-conjunction-distribution/reduction-relations/rail-red.rkt"
    "derivations/experiments/early-conjunction-distribution/reduction-relations/search-join-base-red.rkt"))

(define RETAINED-JOIN-SEAM-PATH
  "derivations/experiments/early-conjunction-distribution/reduction-relations/factored-search-base.rkt")

(define DISJR-FORBIDDEN-RELATIVE-PATHS
  '("derivations/experiments/dormant-branch-semantics/source/languages/search-lang.rkt"
    "derivations/experiments/dormant-branch-semantics/source/languages/search-relcall-lang.rkt"
    "derivations/experiments/dormant-branch-semantics/source/wf/search-wf.rkt"
    "derivations/experiments/dormant-branch-semantics/source/wf/search-relcall-wf.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-relcall-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-dfs-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-flip-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-dfs-relcall-red.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/search-flip-relcall-red.rkt"))

(define DISJR-REQUIRED-RELATIVE-PATHS
  '("derivations/experiments/dormant-branch-semantics/source/languages/rail-lang.rkt"
    "derivations/experiments/dormant-branch-semantics/source/wf/rail-wf.rkt"
    "derivations/experiments/dormant-branch-semantics/source/wf/rail-relcall-wf.rkt"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations/rail-red.rkt"))

(define DISJR-ALLOWED-RELATIVE-PATHS
  (append
   DISJR-REQUIRED-RELATIVE-PATHS
   '("derivations/experiments/dormant-branch-semantics/source/languages/rail-relcall-lang.rkt"
     "derivations/experiments/dormant-branch-semantics/source/reduction-relations/rail-relcall-red.rkt")))

(define DISJR-SEMANTIC-RELATIVE-ROOTS
  '("derivations/experiments/dormant-branch-semantics/source/languages"
    "derivations/experiments/dormant-branch-semantics/source/wf"
    "derivations/experiments/dormant-branch-semantics/source/reduction-relations"))

(define DISTRIBUTED-DISJR-SEMANTIC-RELATIVE-ROOTS
  '("derivations/experiments/early-conjunction-distribution/languages"
    "derivations/experiments/early-conjunction-distribution/reduction-relations"))

(define DISTRIBUTED-DISJR-REQUIRED-RELATIVE-PATHS
  '("derivations/experiments/early-conjunction-distribution/languages/search-lang.rkt"
    "derivations/experiments/early-conjunction-distribution/reduction-relations/search-join-base-red.rkt"
    "derivations/experiments/early-conjunction-distribution/reduction-relations/search-red.rkt"))

(define DISTRIBUTED-DISJR-SCHEDULER-RELATION-PATHS
  '("derivations/experiments/early-conjunction-distribution/reduction-relations/rail-red.rkt"))

(define DISTRIBUTED-RAIL-RELATIVE-PATH
  "derivations/experiments/early-conjunction-distribution/reduction-relations/rail-red.rkt")

(define DISJR-PATTERN
  #px"(?:^|[^A-Za-z0-9_-])DisjR(?=$|[^A-Za-z0-9_-])")

(define retired-module/api-patterns
  (list
   (cons "lower-canonical module" #rx"lower-canonical\\.rkt")
   (cons "canonical core language module" #rx"canonical-core-lang\\.rkt")
   (cons "canonical language module" #rx"canonical-lang\\.rkt")
   (cons "canonical core WF module" #rx"canonical-core-wf\\.rkt")
   (cons "canonical WF module" #rx"canonical-wf\\.rkt")
   (cons "canonical parser target metadata" #rx"canonical-parser-target-id")
   (cons "canonical parser profile metadata" #rx"canonical-parser-profile")
   (cons "canonical target API" #rx"canonical-target-")
   (cons "duplicate canonical WF checker" #rx"check-canonical-well-formed")
   (cons "duplicate canonical target-domain checker" #rx"config-in-target-domain")
   (cons "duplicate target WF judgment" #rx"wf-config/target")))

(define retired-cache-wf-patterns
  (list
   (cons "WF forwarding kernel" #px"(?:^|[/\"])wf/kernel\\.rkt")
   (cons "summary kernel" #rx"summary-kernel\\.rkt")
   (cons "summary-producing WF schema" #rx"phase-wf-schema\\.rkt")
   (cons "summary-producing relcall WF schema" #rx"relcall-phase-wf-schema\\.rkt")
   (cons "summary-producing WF API" #rx"wf-summary")
   (cons "cached-scope append API" #rx"c-append")
   (cons "cached-state projection API"
         #px"(?:^|[^A-Za-z0-9_-])state-c(?=$|[^A-Za-z0-9_-])")
   (cons "cached-config projection API"
         #px"(?:^|[^A-Za-z0-9_-])config-c(?=$|[^A-Za-z0-9_-])")
   (cons "cache insertion bridge" #rx"cache-insert")
   (cons "cache erasure bridge" #rx"cache-erase")
   (cons "exact cached-scope checker" #rx"exact-scope")))

(define retired-owner-marker-patterns
  (list
   (cons "WorkFresh constructor"
         #px"(?:^|[^A-Za-z0-9_-])WorkFresh(?=$|[^A-Za-z0-9_-])")
   (cons "AnswerFresh constructor"
         #px"(?:^|[^A-Za-z0-9_-])AnswerFresh(?=$|[^A-Za-z0-9_-])")
   (cons "FrontierFresh constructor"
         #px"(?:^|[^A-Za-z0-9_-])FrontierFresh(?=$|[^A-Za-z0-9_-])")
   (cons "ScopeFrame grammar"
         #px"(?:^|[^A-Za-z0-9_-])ScopeFrame(?=$|[^A-Za-z0-9_-])")
   (cons "expose-frontier-fresh rule" #rx"expose-frontier-fresh")
   (cons "left fresh-choice exposure rule"
         #rx"expose-choice-through-work-fresh/disj")
   (cons "right fresh-choice exposure rule"
         #rx"expose-choice-through-work-fresh/search-join")
   (cons "erase-dead-fresh rule" #rx"erase-dead-fresh")
   (cons "bubble-delay-through-fresh rule"
         #rx"bubble-delay-through-fresh")
   (cons "work-introduction observation API"
         #rx"structural-work-introduction-count")
   (cons "spine-introduction observation API"
         #rx"structural-spine-introduction-count")))

;; Owner fields now have one tagged representation: (Owners owner ...).
;; These constructor-specific patterns reject the retired untagged empty and
;; nonempty shapes without confusing unrelated empty lists such as intro,
;; substitutions, stores, relation environments, or lexical-variable lists.
(define retired-owner-stack-patterns
  (list
   (cons "owners+ grammar workaround"
         #px"(?:^|[^A-Za-z0-9_-])owners\\+(?=$|[^A-Za-z0-9_-])")
   (cons "untagged empty owner field"
         #px"\\(\\s*(?:Answer|Returned|Work|Dead|Conj|Done|Solo|Last|PendingDelay|Forced|DisjL|DisjR|Emit)\\s+\\(\\)(?=\\s|\\))")
   (cons "untagged nonempty owner field"
         #px"\\(\\s*(?:Answer|Returned|Work|Dead|Conj|Done|Solo|Last|PendingDelay|Forced|DisjL|DisjR|Emit)\\s+\\(\\s*\\(\\s*Owner(?=\\s|\\))")))

(define retired-grammar-abbreviation-patterns
  (list
   (cons "retired neq grammar category"
         ;; A quoted source label such as (label "neq") names a witness;
         ;; it does not reintroduce the retired grammar category.
         #px"(?:^|[^A-Za-z0-9_\"-])neq(?=$|[^A-Za-z0-9_\"-])")
   (cons "retired BranchFrame grammar category"
         #px"(?:^|[^A-Za-z0-9_-])BranchFrame(?=$|[^A-Za-z0-9_-])")
   (cons "retired SpineFrame grammar category"
         #px"(?:^|[^A-Za-z0-9_-])SpineFrame(?=$|[^A-Za-z0-9_-])")))

(define required-owner-stack-patterns
  (list
   (cons "tagged owner-stack grammar"
         #px"\\[owners\\s+\\(Owners\\s+owner\\s+\\.\\.\\.\\)\\]")
   (cons "tagged empty owner stack"
         #px"\\(Owners\\s*\\)")
   (cons "tagged nonempty owner stack"
         #px"\\(Owners\\s+\\(Owner(?=\\s|\\))")))

;; These regular expressions recognize term-shaped constructor occurrences.
;; Renderer strings such as "Deferred", "<-+", and "+->" are deliberately
;; outside the match, so their visible historical vocabulary is preserved.
(define retired-runtime-patterns
  (list
   (cons "empty-tree" #px"\\(\\s*empty-tree(?=\\s|\\))")
   (cons "top-result" #px"\\(\\s*⊤(?=\\s|\\))")
   (cons "ScopedTree" #px"\\(\\s*ScopedTree(?=\\s|\\))")
   (cons "ScopedShell" #px"\\(\\s*ScopedShell(?=\\s|\\))")
   (cons "Deferred" #px"\\(\\s*Deferred(?=\\s|\\))")
   (cons "left-active choice" #px"\\(\\s*<-\\+(?=\\s|\\))")
   (cons "right-active choice" #px"\\(\\s*\\+->(?=\\s|\\))")
   (cons "internal delay constructor" #px"\\(\\s*delay(?=\\s|\\))")))

;; These context and result grammars belonged to the anticipatory phase
;; presentation. The dormant feature languages now expose only carrier sorts
;; plus the recursive focus grammar induced by their constructors. The explicit
;; distributed experiment lives outside DORMANT-LANGUAGE-ROOT.
(define retired-dormant-grammar-patterns
  (list
   (cons "retired context/result category"
         #px"(?:^|[^A-Za-z0-9_-])(?:WW|WFrame|NFWW|FF|BF|LF|WF|T|SC|SR|R|U|NR|NW|NF)(?=$|[^A-Za-z0-9_-])")
   (cons "distributed Early category"
         #px"(?:^|[^A-Za-z0-9_-])Early[A-Za-z0-9_-]*(?=$|[^A-Za-z0-9_-])")
   (cons "retired summary grammar"
         #px"(?:^|[^A-Za-z0-9_-])summary(?=$|[^A-Za-z0-9_-])")))

(define (racket-source-files)
  (sort
   (for*/list ([root (in-list ACTIVE-SOURCE-ROOTS)]
               [path (in-directory root)]
               #:when (and (file-exists? path)
                           (equal? (path-get-extension path) #".rkt")
                           ;; The suite defines the forbidden spellings that it
                           ;; searches for, so it cannot usefully scan itself.
                           (not (equal? (relative-source-path path)
                                        SCAN-TEST-PATH))))
     path)
   string<?
   #:key path->string))

(define (relative-source-path path)
  (path->string (find-relative-path ACTIVE-ROOT path)))

(define (dormant-language-files)
  (sort
   (for/list ([name (in-list (directory-list DORMANT-LANGUAGE-ROOT))]
              #:do [(define path (build-path DORMANT-LANGUAGE-ROOT name))]
              #:when (and (file-exists? path)
                          (equal? (path-get-extension path) #".rkt")))
     path)
   string<?
   #:key path->string))

(define (allowed-runtime-occurrence? relative-path constructor line)
  (or
   ;; This is Racket's promise constructor, not a semantic term constructor.
   (and (equal? relative-path "derivations/experiments/dormant-branch-semantics/source/answer-node.rkt")
        (equal? constructor "internal delay constructor")
        (equal? (string-trim line)
                (string-append "(" "delay (prepare-minikanren-namespace)))")))
   ;; Direct micro source explicitly rejects the retired/internal spelling.
   (and (equal? relative-path "src/transpiler/micro.rkt")
        (equal? constructor "internal delay constructor")
        (equal? (string-trim line)
                (string-append "[`(" "delay . ,_)")))
   ;; HTTP requests use a Racket promise for their bindings payload.
   (and (equal? relative-path "tests/test-http-helpers.rkt")
        (equal? constructor "internal delay constructor")
        (equal? (string-trim line) (string-append "(" "delay '())")))
   ;; This string is an intentionally rejected direct-micro input.
   (and (equal? relative-path "tests/test-transpiler.rkt")
        (equal? constructor "internal delay constructor")
        (equal? (string-trim line)
                (string-append "\"(run* (q) (" "delay (== q 'cat)))\"")))))

(define (number-lines lines)
  (for/list ([line (in-list lines)]
             [line-number (in-naturals 1)])
    (list line-number line)))

(define (scan-files files patterns [allowed? (lambda (_path _name _line) #f)])
  (for*/list ([path (in-list files)]
              [numbered-line
               (in-list
                (number-lines (call-with-input-file path port->lines)))]
              [entry (in-list patterns)]
              #:when (regexp-match? (cdr entry) (second numbered-line))
              #:unless (allowed? (relative-source-path path)
                                  (car entry)
                                  (second numbered-line)))
    (format "~a:~a: retired ~a in ~s"
            (relative-source-path path)
            (first numbered-line)
            (car entry)
            (string-trim (second numbered-line)))))

(define (scan-sources patterns [allowed? (lambda (_path _name _line) #f)])
  (scan-files (racket-source-files) patterns allowed?))

(define (check-no-violations! label violations)
  (check-equal? violations
                '()
                (if (null? violations)
                    label
                    (string-append label ":\n" (string-join violations "\n")))))

(define (source-pattern-present? regexp)
  (for/or ([path (in-list (racket-source-files))])
    (call-with-input-file
     path
     (lambda (in)
       (regexp-match? regexp (port->string in))))))

(define (relative-paths->active-files relative-paths)
  (map (lambda (relative-path)
         (build-path ACTIVE-ROOT relative-path))
       relative-paths))

(define (file-pattern-present? path regexp)
  (call-with-input-file
   path
   (lambda (in)
     (regexp-match? regexp (port->string in)))))

(define (semantic-racket-files relative-roots)
  (sort
   (for*/list ([relative-root (in-list relative-roots)]
               [path (in-directory (build-path ACTIVE-ROOT relative-root))]
               #:when (and (file-exists? path)
                           (equal? (path-get-extension path) #".rkt")))
     path)
   string<?
   #:key path->string))

(define/provide-test-suite RETIRED-WORK-SYNTAX
  (test-case "architecture checks include application consumers and the complete experiments"
    (define scanned-paths (map relative-source-path (racket-source-files)))
    (for ([root (in-list ACTIVE-SOURCE-ROOTS)])
      (define prefix (string-append (relative-source-path root) "/"))
      (check-true
       (for/or ([path (in-list scanned-paths)]) (string-prefix? path prefix))
       (format "architecture scan omitted source root: ~a" prefix)))
    (for ([path (in-list
                 '("src/search-runtime.rkt"
                   "tests/scheduler-integration-tests.rkt"
                   "derivations/experiments/dormant-branch-semantics/source/languages/core-lang.rkt"
                   "derivations/experiments/dormant-branch-semantics/tests/nodes/core-tests.rkt"
                   "derivations/experiments/early-conjunction-distribution/tests.rkt"))])
      (check-not-false (member path scanned-paths))))

  (test-case "the broad gate explicitly invokes both semantic accounts"
    (define headless-imports
      (call-with-input-file (build-path ACTIVE-ROOT "tests" "test-all-headless.rkt")
        (lambda (input)
          (parameterize ([read-accept-reader #t])
            (syntax->datum (read-syntax "test-all-headless.rkt" input))))))
    (match-define `(module ,_ ,_ (#%module-begin ,forms ...)) headless-imports)
    (for ([aggregate (in-list '("../derivations/all.rkt"
                               "../derivations/experiments/all.rkt"))])
      (check-not-false
       (for/or ([form (in-list forms)])
         (match form
           [`(module+ test ,body ...)
            (for/or ([expression (in-list body)])
              (match expression
                [`(require ,specifications ...) (member aggregate specifications)]
                [_ #f]))]
           [_ #f])))))

  (test-case "active source and test modules do not reference the retired canonical work layer"
    (define active-files
      (racket-source-files))
    (check-true (pair? active-files)
                "source scan must inspect at least one Racket module")
    (check-false
     (member SCAN-TEST-PATH (map relative-source-path active-files))
     "retirement scan must exclude its own forbidden-pattern declarations")
    (check-no-violations!
     "retired canonical module/API references remain"
     (scan-sources retired-module/api-patterns))
    (check-no-violations!
     "retired cache/WF-summary module or API references remain"
     (scan-sources retired-cache-wf-patterns))
    ;; The retired forwarding module was wf/kernel.rkt. A local import from
    ;; that directory needs the same check, but derivations/shared/kernel.rkt
    ;; is a live provider and must not be rejected for sharing its basename.
    (check-false
     (file-exists?
      (build-path ACTIVE-ROOT "derivations" "experiments"
                  "dormant-branch-semantics" "source" "wf" "kernel.rkt")))
    (check-no-violations!
     "a WF module imports the retired local forwarding kernel"
     (scan-files
      (semantic-racket-files '("derivations/experiments/dormant-branch-semantics/source/wf"))
      (list (cons "WF forwarding kernel" #px"\"(?:\\./)?kernel\\.rkt\""))))
    (check-no-violations!
     "retired fresh-marker constructor, rule, or observation API remains"
     (scan-sources retired-owner-marker-patterns))
    (check-no-violations!
     "retired untagged owner-stack grammar or carrier syntax remains"
     (scan-sources retired-owner-stack-patterns))
    (check-no-violations!
     "retired one-use grammar abbreviation remains"
     (scan-sources retired-grammar-abbreviation-patterns)))

  (test-case "tagged owner-stack scan is positive and non-vacuous"
    (for ([entry (in-list required-owner-stack-patterns)])
      (check-true
       (source-pattern-present? (cdr entry))
       (format "active source/tests contain no ~a witness" (car entry)))))

  (test-case "DisjR is rail-local in the dormant source"
    (define forbidden-files
      (relative-paths->active-files DISJR-FORBIDDEN-RELATIVE-PATHS))
    (define required-files
      (relative-paths->active-files DISJR-REQUIRED-RELATIVE-PATHS))
    (check-equal?
     (filter-not file-exists? (append forbidden-files required-files))
     '()
     "rail-local DisjR scan must not silently skip a named module")
    (define semantic-files
      (semantic-racket-files DISJR-SEMANTIC-RELATIVE-ROOTS))
    (define non-rail-semantic-files
      (filter
       (lambda (path)
         (not (member (relative-source-path path)
                      DISJR-ALLOWED-RELATIVE-PATHS)))
       semantic-files))
    (check-true (pair? non-rail-semantic-files)
                "rail-local scan must inspect non-rail semantic modules")
    (check-no-violations!
     "a non-rail semantic module contains rail-local DisjR"
     (scan-files non-rail-semantic-files
                 (list (cons "rail-local DisjR" DISJR-PATTERN))))
    (for ([path (in-list required-files)])
      (check-true
       (file-pattern-present? path DISJR-PATTERN)
       (format "rail module contains no positive DisjR witness: ~a"
               (relative-source-path path)))))

  (test-case "the distributed experiment retains its common right-active carrier"
    (define required-files
      (relative-paths->active-files
       DISTRIBUTED-DISJR-REQUIRED-RELATIVE-PATHS))
    (define distributed-rail-path
      (build-path ACTIVE-ROOT DISTRIBUTED-RAIL-RELATIVE-PATH))
    (check-equal?
     (filter-not file-exists? (cons distributed-rail-path required-files))
     '()
     "distributed right-active scan must not silently skip a named module")
    (for ([path (in-list required-files)])
      (check-true
       (file-pattern-present? path DISJR-PATTERN)
       (format "distributed search module contains no DisjR witness: ~a"
               (relative-source-path path))))
    (define distributed-disjr-files
      (sort
       (for/list
           ([path
             (in-list
              (semantic-racket-files
               DISTRIBUTED-DISJR-SEMANTIC-RELATIVE-ROOTS))]
            #:when (file-pattern-present? path DISJR-PATTERN))
         (relative-source-path path))
       string<?))
    (check-equal?
     distributed-disjr-files
     (sort
      (append DISTRIBUTED-DISJR-REQUIRED-RELATIVE-PATHS
              DISTRIBUTED-DISJR-SCHEDULER-RELATION-PATHS)
      string<?)
     "distributed DisjR must stay on common search or its scheduler relation")
    (check-true
     (file-pattern-present? distributed-rail-path #rx"rail-scheduler/raw"))
    (check-true
     (file-pattern-present? distributed-rail-path #rx"\"\\./search-red\\.rkt\""))
    (check-true
     (file-pattern-present? distributed-rail-path
                            #rx"distributed-base:right-active/work/raw"))
    (check-false
     (file-pattern-present? distributed-rail-path
                            #px"\\(define\\s+(?:right-active|rail-scheduler/raw)")))

  (test-case "dormant relations compose through immediate predecessor surfaces"
    (define dependency-files
      (relative-paths->active-files DORMANT-RELATION-DEPENDENCY-PATHS))
    (check-equal? (filter-not file-exists? dependency-files)
                  '()
                  "relation dependency scan must not silently skip a module")
    (define search-path (first dependency-files))
    (define search-relcall-path (second dependency-files))
    (define rail-path (third dependency-files))
    (define rail-relcall-path (fourth dependency-files))
    (check-true (file-pattern-present? search-path #rx"\"\\./disj-red\\.rkt\""))
    (check-true (file-pattern-present? search-path #rx"\"\\./delay-red\\.rkt\""))
    (check-false
     (file-pattern-present? search-path #rx"search-join-base-red\\.rkt"))
    (check-true
     (file-pattern-present? search-relcall-path #rx"\"\\./search-red\\.rkt\""))
    (check-true (file-pattern-present? rail-path #rx"\"\\./search-red\\.rkt\""))
    (check-true
     (file-pattern-present? rail-relcall-path #rx"\"\\./search-relcall-red\\.rkt\""))
    (check-true
     (file-pattern-present? rail-relcall-path #rx"\"\\./rail-red\\.rkt\""))
    (for ([path (in-list (list rail-path rail-relcall-path))])
      (check-false
       (file-pattern-present?
        path
        #px"\"\\./(?:core|delay|disj(?:-base)?|search-join-base)-red\\.rkt\"")))
    ;; The retained raw join seam is an implementation dependency of the
    ;; isolated distributed presentation only; it is not a dormant source node or
    ;; a predecessor of dormant search.
    (define seam-path (build-path ACTIVE-ROOT RETAINED-JOIN-SEAM-PATH))
    (check-true (file-exists? seam-path)
                "the experiment's factored base must remain inspectable")
    (check-false (file-pattern-present? seam-path DISJR-PATTERN)
                 "the factored base must not acquire distributed DisjR")
    (define seam-consumers
      (sort
       (for/list ([path (in-list (racket-source-files))]
                  #:when
                  (file-pattern-present? path #rx"factored-search-base\\.rkt"))
         (relative-source-path path))
       string<?))
    (check-equal? seam-consumers RETAINED-JOIN-SEAM-CONSUMERS))

  (test-case "active source and test modules contain no retired mixed-carrier term syntax"
    (check-no-violations!
     "retired mixed-carrier syntax remains"
     (scan-sources retired-runtime-patterns allowed-runtime-occurrence?)))

  (test-case "dormant feature languages contain no anticipatory phase grammar"
    (check-true (pair? (dormant-language-files))
                "dormant language scan must inspect at least one Racket module")
    (check-no-violations!
     "retired anticipatory grammar remains in a dormant feature language"
     (scan-files (dormant-language-files)
                 retired-dormant-grammar-patterns))))

(module+ test
  (define failures (run-tests RETIRED-WORK-SYNTAX))
  (unless (zero? failures)
    (error 'RETIRED-WORK-SYNTAX "~a test case(s) failed" failures)))
