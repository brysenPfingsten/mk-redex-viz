#lang racket

(require redex/reduction-semantics
         "../src/core-definitions.rkt"
         "../src/wf-core.rkt"
         "../src/extensions/variant-languages.rkt")

(provide final-config?
         wf-config-term?
         progress?
         unique-decomposition?
         states-wf?
         tagged-successor-name
         tagged-successor-cfg
         canonical-term
         overlap-kind
         overlap-event
         shape-closed/L1?
         shape-closed/L2?
         shape-closed/L3?
         shape-closed/L4?
         symbols-in
         tree-of
         seam-config-candidates
         sigma-a
         sigma-b
         cfg-core
         cfg-call
         cfg-disj
         cfg-flip
         cfg-rail)

(define (final-config? cfg)
  (redex-match? Core end-config cfg))

(define (wf-config-term? cfg)
  (judgment-holds (wf-config? ,cfg)))

(define (progress? rel cfg)
  (or (final-config? cfg)
      (not (null? (apply-reduction-relation rel cfg)))))

(define (unique-decomposition? rel cfg)
  (define next* (apply-reduction-relation rel cfg))
  (if (final-config? cfg)
      (null? next*)
      (= (length next*) 1)))

(define (tagged-successor-name succ)
  (match succ
    [(list name _cfg) (~a name)]
    [_ "<unknown>"]))

(define (tagged-successor-cfg succ)
  (match succ
    [(list _name cfg) cfg]
    [_ succ]))

(define (canonical-term t)
  (format "~s" t))

;; Returns one of: #f, 'same-term, 'different-term.
(define (overlap-kind tagged-next*)
  (cond
    [(<= (length tagged-next*) 1) #f]
    [else
     (define cfg-terms
       (for/list ([succ (in-list tagged-next*)])
         (canonical-term (tagged-successor-cfg succ))))
     (if (= (length (remove-duplicates cfg-terms)) 1)
         'same-term
         'different-term)]))

(define (overlap-event rel-name cfg tagged-next* step-index)
  (hash 'relation rel-name
        'kind (overlap-kind tagged-next*)
        'step step-index
        'cfg (canonical-term cfg)
        'rule-names
        (for/list ([succ (in-list tagged-next*)])
          (tagged-successor-name succ))
        'next-terms
        (for/list ([succ (in-list tagged-next*)])
          (canonical-term (tagged-successor-cfg succ)))))

(define (states-in datum [acc '()])
  (match datum
    [`(state ,_sub ,_c ,_trail ,_tag) (cons datum acc)]
    ['() acc]
    [(cons a d) (states-in a (states-in d acc))]
    [_ acc]))

(define (states-wf? cfg)
  (for/and ([st (in-list (states-in cfg))])
    (judgment-holds (wf-state? ,st))))

(define (shape-closed? lang-id rel cfg)
  (define cfg-in-lang?
    (case lang-id
      [(L1) (lambda (cfg^) (redex-match? L1 config cfg^))]
      [(L2) (lambda (cfg^) (redex-match? L2 config cfg^))]
      [(L3) (lambda (cfg^) (redex-match? L3 config cfg^))]
      [(L4) (lambda (cfg^) (redex-match? L4 config cfg^))]
      [else (lambda (_cfg^) #f)]))
  (for/and ([cfg^ (in-list (apply-reduction-relation rel cfg))])
    (cfg-in-lang? cfg^)))

(define (shape-closed/L1? rel cfg)
  (shape-closed? 'L1 rel cfg))

(define (shape-closed/L2? rel cfg)
  (shape-closed? 'L2 rel cfg))

(define (shape-closed/L3? rel cfg)
  (shape-closed? 'L3 rel cfg))

(define (shape-closed/L4? rel cfg)
  (shape-closed? 'L4 rel cfg))

(define (symbols-in d)
  (match d
    ['() '()]
    [(? symbol?) (list d)]
    [(cons a b) (append (symbols-in a) (symbols-in b))]
    [_ '()]))

(define (tree-of cfg)
  (second cfg))

(define sigma-a
  (term (state () () () (label "a"))))

(define sigma-b
  (term (state () () () (label "b"))))

(define cfg-core
  (term (() (⊤ (state () () () (label "s"))) (empty-stream))))

(define cfg-call
  (term (((r:id (x:0) (x:0 =? (sym "ok") (label "eq"))))
         ((r:id (sym "ok") (label "call"))
          (state () () () (label "s")))
         (empty-stream))))

(define cfg-disj
  (term (() ((⊤ (state () () () (label "a")))
             <-+
             (⊤ (state () () () (label "b"))))
         (empty-stream))))

(define cfg-flip
  (term (() ((delay (empty-tree))
             <-+
             (⊤ (state () () () (label "b"))))
         (empty-stream))))

(define cfg-rail
  (term (() ((delay (empty-tree))
             <-+
             (⊤ (state () () () (label "b"))))
         (empty-stream))))

;; Shared seam corpus for bounded smoke/determinism checks at relation boundaries.
(define seam-config-candidates
  (list cfg-core
        cfg-call
        cfg-disj
        cfg-flip
        cfg-rail))
