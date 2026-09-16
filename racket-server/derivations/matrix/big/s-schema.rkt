#lang racket
(require redex/reduction-semantics "../source-s.rkt" "../../shared/kernel.rkt" "feature-schema.rkt"
         (for-syntax racket/base racket/syntax syntax/parse))
(provide define-s-big)
(define-syntax (define-s-big stx)
  (syntax-parse stx
    [(_ coordinate:id language:id parent:id value?:id observation?:id feature:id
        (~optional (~seq #:relations environment:id) #:defaults ([environment #'#f])))
     #:with (env ...) (if (syntax-e #'environment) #'(environment) #'())
     #:with (env-mode ...) (if (syntax-e #'environment) #'(I) #'())
     #:with relation-value (if (syntax-e #'environment) #'environment #''())
     #:with relation-term (if (syntax-e #'environment) #'(term environment) #''())
     #:with search-big (format-id #'coordinate "search-big/~a" #'coordinate)
     #:with merge-big (format-id #'coordinate "merge-big/~a" #'coordinate)
     #:with bind-big (format-id #'coordinate "bind-big/~a" #'coordinate)
     #:with render-big (format-id #'coordinate "render-big/~a" #'coordinate)
     #:with commit-big (format-id #'coordinate "commit-big/~a" #'coordinate)
     #:with advance-big (format-id #'coordinate "advance-big/~a" #'coordinate)
     #:with collect-big (format-id #'coordinate "collect-big/~a" #'coordinate)
     #:with observe-big (format-id #'coordinate "observe-big/~a" #'coordinate)
     #:with evaluate (format-id #'coordinate "evaluate/~a" #'coordinate)
     #:with promote (format-id #'coordinate "promote/~a" #'coordinate)
     #:with raw-derivations (format-id #'coordinate "raw-derivations/~a" #'coordinate)
     #'(feature-specialize feature
        (provide language search-big merge-big bind-big render-big commit-big advance-big collect-big
                 observe-big evaluate promote raw-derivations)
;; P is the ordered support of enclosing Owner fields on the active path.
;; It is inherited during descent, never obtained by scanning a sibling or
;; the whole term. State retains the S row's original four-field carrier.
;; These inductive equations are a least fixed point, with no fuel or machine
;; transitions. Exact source-label traces expose strict premise order.
(define-extended-language language parent
  [P any] [label string] [trace (label (... ...))])

(define-metafunction language
  traces : trace (... ...) -> trace
  [(traces (label (... ...)) (... ...)) (label (... ...) (... ...))])
(define-metafunction language
  support+ : P owners -> P
  [(support+ P owners) ,(owners-support (term owners) (term P))])
(define-metafunction language
  owners+ : owners owners -> owners
  [(owners+ owners_1 owners_2) ,(owners-append (term owners_1) (term owners_2))])

(define (atomic-result owners goal state)
  (match (atomic/s goal state)
    [(Failure) `(Empty ,owners)]
    [(Success next) `(One ,owners ,next)]))

;; Big's structural owner operation is stated independently of the source
;; contraction. Owners surround the active root before it evaluates; only
;; force is transparent. Neither delayed bodies nor siblings are scanned.
(define (retain-owners owners computation)
  (match computation
    [`(force ,inner) `(force ,(retain-owners owners inner))]
    [`(,constructor ,local ,rest (... ...))
     `(,constructor ,(owners-append owners local) ,@rest)]))

(define-judgment-form language
  #:mode (search-big env-mode ... I I O O)
  #:contract (search-big env ... P c SV trace)
  [---------------------------------------------------- "value"
   (search-big env ... P SV SV ())]
  [(where SV ,(atomic-result (term owners) (term a) (term σ)))
   ---------------------------------------------------- "atom"
   (search-big env ... P (eval owners a σ) SV ("eval-atom"))]
  [(where g ,(instantiate-relation relation-term (term call)))
   (search-big env ... P (eval owners g σ) SV trace)
   ---------------------------------------------------- "relation call"
   (search-big env ... P (eval owners call σ) SV (traces ("eval-call") trace))]
  [(where c ,(allocate/s (term owners) (term (x (... ...))) (term g)
                        (term tag) (term σ) (term P)))
   (search-big env ... P c SV trace)
   ---------------------------------------------------- "fresh"
   (search-big env ... P (eval owners (∃ (x (... ...)) g tag) σ) SV
                 (traces ("allocate-fresh") trace))]
  [(search-big env ... (support+ P owners) (eval (Owners) g_1 σ) SV_1 trace_1)
   (search-big env ... (support+ P owners) (eval (Owners) g_2 σ) SV_2 trace_2)
   (merge-big env ... P owners SV_1 SV_2 SV trace_merge)
   ---------------------------------------------------- "disjunction"
   (search-big env ... P (eval owners (g_1 ∨ g_2 tag) σ) SV
                 (traces ("eval-disj") trace_1 trace_2 trace_merge))]
  [(search-big env ... (support+ P owners) (eval (Owners) g_1 σ) SV_1 trace_1)
   (bind-big env ... P owners SV_1 g_2 SV trace_bind)
   ---------------------------------------------------- "conjunction"
   (search-big env ... P (eval owners (g_1 ∧ g_2 tag) σ) SV
                 (traces ("eval-conj") trace_1 trace_bind))]
  [---------------------------------------------------- "suspension"
   (search-big env ... P (eval owners (suspend g tag) σ)
                 (Delay owners (eval (Owners) g σ)) ("eval-suspend"))]
  [(search-big env ... (support+ P owners) c_1 SV_1 trace_1)
   (search-big env ... (support+ P owners) c_2 SV_2 trace_2)
   (merge-big env ... P owners SV_1 SV_2 SV trace_merge)
   ---------------------------------------------------- "strict merge operands"
   (search-big env ... P (mplus owners c_1 c_2) SV (traces trace_1 trace_2 trace_merge))]
  [(search-big env ... (support+ P owners) c SV_1 trace_1)
   (bind-big env ... P owners SV_1 g SV trace_bind)
   ---------------------------------------------------- "strict bind operand"
   (search-big env ... P (bind owners c g) SV (traces trace_1 trace_bind))]
  [(side-condition ,(not (value? (term c))))
   (search-big env ... (support+ P owners) c SV trace)
   ---------------------------------------------------- "eager Yield tail"
   (search-big env ... P (Yield owners A c) (Yield owners A SV) trace)]
  [(search-big env ... P c_1 (Delay owners c_2) trace_1)
   (where c_retained ,(retain-owners (term owners) (term c_2)))
   (search-big env ... P c_retained SV trace_2)
   ---------------------------------------------------- "force"
   (search-big env ... P (force c_1) SV (traces trace_1 ("force-delay") trace_2))])

(define-judgment-form language
  #:mode (merge-big env-mode ... I I I I O O)
  #:contract (merge-big env ... P owners SV SV SV trace)
  [(where SV_result ,(retain-owners (term owners) (term SV)))
   ---------------------------------------------------- "merge empty"
   (merge-big env ... P owners (Empty owners_1) SV SV_result ("mplus-empty"))]
  [---------------------------------------------------- "merge one"
   (merge-big env ... P owners (One owners_1 σ) SV
                (Yield owners (Answer owners_1 σ) SV) ("mplus-one"))]
  [(where SV_left ,(retain-owners (term owners_1) (term SV_1)))
   (merge-big env ... (support+ P owners) (Owners) SV_left SV_2 SV trace)
   ---------------------------------------------------- "merge Yield"
   (merge-big env ... P owners (Yield owners_1 (Answer owners_2 σ) SV_1) SV_2
                (Yield owners (Answer (owners+ owners_1 owners_2) σ) SV)
                (traces ("mplus-yield") trace))]
  [---------------------------------------------------- "merge suspension"
   (merge-big env ... P owners (Delay owners_1 c) SV
                (Delay owners (mplus (Owners) SV (force (Delay owners_1 c))))
                ("mplus-delay"))])

(define-judgment-form language
  #:mode (bind-big env-mode ... I I I I O O)
  #:contract (bind-big env ... P owners SV g SV trace)
  [---------------------------------------------------- "bind empty"
   (bind-big env ... P owners (Empty owners_1) g
               (Empty (owners+ owners owners_1)) ("bind-empty"))]
  [(search-big env ... P (eval (owners+ owners owners_1) g σ) SV trace)
   ---------------------------------------------------- "bind one"
   (bind-big env ... P owners (One owners_1 σ) g SV (traces ("bind-one") trace))]
  [(where owners_shared (owners+ owners owners_1))
   (search-big env ... (support+ P owners_shared) (eval owners_2 g σ) SV_1 trace_1)
   (bind-big env ... (support+ P owners_shared) (Owners) SV_tail g SV_2 trace_2)
   (merge-big env ... P owners_shared SV_1 SV_2 SV trace_merge)
   ---------------------------------------------------- "bind eager residual"
   (bind-big env ... P owners (Yield owners_1 (Answer owners_2 σ) SV_tail) g SV
               (traces ("bind-yield") trace_1 trace_2 trace_merge))]
  [---------------------------------------------------- "bind suspension"
   (bind-big env ... P owners (Delay owners_1 c) g
               (Delay (owners+ owners owners_1)
                      (bind (Owners) c g))
               ("bind-delay"))])

(define-judgment-form language
  #:mode (render-big env-mode ... I I O O)
  #:contract (render-big env ... P SV O trace)
  [---------------------------------------------------- "render empty"
   (render-big env ... P (Empty owners) (Done owners) ("render-empty"))]
  [---------------------------------------------------- "render one"
   (render-big env ... P (One owners σ) (Solo owners σ) ("render-one"))]
  [(render-big env ... (support+ P owners) SV O trace)
   ---------------------------------------------------- "render Yield"
   (render-big env ... P (Yield owners A SV) (Emit owners A O)
                 (traces ("render-yield") trace))]
  [(search-big env ... (support+ P owners) c SV trace_search)
   (render-big env ... (support+ P owners) SV O trace_render)
   ---------------------------------------------------- "render suspension"
   (render-big env ... P (Delay owners c) (Forced owners O)
                 (traces ("render-delay") trace_search trace_render))])

(define-judgment-form language
  #:mode (commit-big env-mode ... I I O O)
  #:contract (commit-big env ... P SV F trace)
  [---------------------------------------------------- "commit empty"
   (commit-big env ... P (Empty owners) (Done owners) ("commit-empty"))]
  [---------------------------------------------------- "commit one"
   (commit-big env ... P (One owners σ) (Solo owners σ) ("commit-one"))]
  [(commit-big env ... (support+ P owners) SV F trace)
   ---------------------------------------------------- "commit eager tail"
   (commit-big env ... P (Yield owners A SV) (Emit owners A F)
                 (traces ("commit-yield") trace))]
  [---------------------------------------------------- "commit suspended tip"
   (commit-big env ... P (Delay owners c) (More (Delay owners c)) ("commit-delay"))])

(define-judgment-form language
  #:mode (advance-big env-mode ... I I O O)
  #:contract (advance-big env ... P F F trace)
  [---------------------------------------------------- "advance done"
   (advance-big env ... P (Done owners) (Done owners) ("advance-done"))]
  [---------------------------------------------------- "advance solo"
   (advance-big env ... P (Solo owners σ) (Solo owners σ) ("advance-solo"))]
  [(advance-big env ... (support+ P owners) F_1 F_2 trace)
   ---------------------------------------------------- "advance Emit"
   (advance-big env ... P (Emit owners A F_1) (Emit owners A F_2)
                  (traces ("advance-emit") trace))]
  [(advance-big env ... (support+ P owners) F_1 F_2 trace)
   ---------------------------------------------------- "advance existing history"
   (advance-big env ... P (Forced owners F_1) (Forced owners F_2)
                  (traces ("advance-forced") trace))]
  [(search-big env ... (support+ P owners) c SV trace_search)
   (commit-big env ... (support+ P owners) SV F trace_commit)
   ---------------------------------------------------- "advance one exposed suspension"
   (advance-big env ... P (More (Delay owners c)) (Forced owners F)
                  (traces ("advance-delay") trace_search trace_commit))])

(define-judgment-form language
  #:mode (collect-big env-mode ... I I O O)
  #:contract (collect-big env ... P F O trace)
  [---------------------------------------------------- "collect done"
   (collect-big env ... P (Done owners) (Done owners) ("collect-done"))]
  [---------------------------------------------------- "collect solo"
   (collect-big env ... P (Solo owners σ) (Solo owners σ) ("collect-solo"))]
  [(collect-big env ... (support+ P owners) F O trace)
   ---------------------------------------------------- "collect Emit"
   (collect-big env ... P (Emit owners A F) (Emit owners A O)
                  (traces ("collect-emit") trace))]
  [(collect-big env ... (support+ P owners) F O trace)
   ---------------------------------------------------- "collect existing history"
   (collect-big env ... P (Forced owners F) (Forced owners O)
                  (traces ("collect-forced") trace))]
  [(search-big env ... (support+ P owners) c SV trace_search)
   (commit-big env ... (support+ P owners) SV F trace_commit)
   (collect-big env ... (support+ P owners) F O trace_collect)
   ---------------------------------------------------- "collect suspended frontier"
   (collect-big env ... P (More (Delay owners c)) (Forced owners O)
                  (traces ("collect-delay") trace_search trace_commit trace_collect))])

(define-judgment-form language
  #:mode (observe-big env-mode ... I I O O)
  #:contract (observe-big env ... P o F trace)
  [---------------------------------------------------- "observation value"
   (observe-big env ... P F F ())]
  [(search-big env ... P c SV trace_search)
   (render-big env ... P SV O trace_render)
   ---------------------------------------------------- "strict render operand"
   (observe-big env ... P (render c) O (traces trace_search trace_render))]
  [(search-big env ... P c SV trace_search)
   (commit-big env ... P SV F trace_commit)
   ---------------------------------------------------- "strict commit operand"
   (observe-big env ... P (commit c) F (traces trace_search trace_commit))]
  [(observe-big env ... P o F_1 trace_operand)
   (advance-big env ... P F_1 F_2 trace_advance)
   ---------------------------------------------------- "strict advance operand"
   (observe-big env ... P (advance o) F_2 (traces trace_operand trace_advance))]
  [(observe-big env ... P o F trace_operand)
   (collect-big env ... P F O trace_collect)
   ---------------------------------------------------- "strict collect operand"
   (observe-big env ... P (collect o) O (traces trace_operand trace_collect))]
  [(side-condition ,(not (redex-match? parent F (term o))))
   (observe-big env ... (support+ P owners) o F trace)
   ---------------------------------------------------- "observation Emit tail"
   (observe-big env ... P (Emit owners A o) (Emit owners A F) trace)]
  [(side-condition ,(not (redex-match? parent F (term o))))
   (observe-big env ... (support+ P owners) o F trace)
   ---------------------------------------------------- "observation Forced tail"
   (observe-big env ... P (Forced owners o) (Forced owners F) trace)])

;; Constructor-erased fixed-point equations over the actual S carriers.
(define (promote-search env ... computation ancestry)
  (match computation
    [(? value? value) value]
    [`(eval ,owners ,(and goal (or `(succeed ,_) `(fail ,_)
                                 `(,_ =? ,_ ,_) `(,_ != ,_ ,_))) ,state)
     (atomic-result owners goal state)]
    [`(eval ,owners ,(and call `(,(? relation-name?) ,_ (... ...))) ,state)
     (promote-search env ...
                     `(eval ,owners ,(instantiate-relation relation-value call) ,state) ancestry)]
    [`(eval ,owners (∃ ,binders ,body ,tag) ,state)
     (promote-search env ... (allocate/s owners binders body tag state ancestry) ancestry)]
    [`(eval ,owners (,left ∨ ,right ,_) ,state)
     (define inner (owners-support owners ancestry))
     (promote-merge env ... owners (promote-search env ... `(eval (Owners) ,left ,state) inner)
                    (promote-search env ... `(eval (Owners) ,right ,state) inner) ancestry)]
    [`(eval ,owners (,left ∧ ,right ,_) ,state)
     (promote-bind env ... owners
                   (promote-search env ... `(eval (Owners) ,left ,state)
                                   (owners-support owners ancestry)) right ancestry)]
    [`(eval ,owners (suspend ,goal ,_) ,state)
     `(Delay ,owners (eval (Owners) ,goal ,state))]
    [`(mplus ,owners ,left ,right)
     (define inner (owners-support owners ancestry))
     (promote-merge env ... owners (promote-search env ... left inner) (promote-search env ... right inner) ancestry)]
    [`(bind ,owners ,search ,goal)
     (promote-bind env ... owners (promote-search env ... search (owners-support owners ancestry)) goal ancestry)]
    [`(Yield ,owners ,answer ,tail)
     `(Yield ,owners ,answer ,(promote-search env ... tail (owners-support owners ancestry)))]
    [`(force ,search)
     (match-define `(Delay ,owners ,body) (promote-search env ... search ancestry))
     (promote-search env ... (retain-owners owners body) ancestry)]))

(define (promote-merge env ... owners left right ancestry)
  (match left
    [`(Empty ,_) (retain-owners owners right)]
    [`(One ,left-owners ,state) `(Yield ,owners (Answer ,left-owners ,state) ,right)]
    [`(Yield ,left-owners (Answer ,answer-owners ,state) ,tail)
     `(Yield ,owners (Answer ,(owners-append left-owners answer-owners) ,state)
            ,(promote-merge env ... '(Owners) (retain-owners left-owners tail) right
                            (owners-support owners ancestry)))]
    [`(Delay ,left-owners ,body)
     `(Delay ,owners (mplus (Owners) ,right (force (Delay ,left-owners ,body))))]))

(define (promote-bind env ... owners search goal ancestry)
  (match search
    [`(Empty ,inner) `(Empty ,(owners-append owners inner))]
    [`(One ,inner ,state)
     (promote-search env ... `(eval ,(owners-append owners inner) ,goal ,state) ancestry)]
    [`(Yield ,inner (Answer ,answer-owners ,state) ,tail)
     (define shared (owners-append owners inner))
     (define inner-prefix (owners-support shared ancestry))
     (promote-merge env ... shared (promote-search env ... `(eval ,answer-owners ,goal ,state) inner-prefix)
                    (promote-bind env ... '(Owners) tail goal inner-prefix) ancestry)]
    [`(Delay ,inner ,body)
     `(Delay ,(owners-append owners inner) (bind (Owners) ,body ,goal))]))

(define (promote-render env ... search ancestry)
  (match search
    [`(Empty ,owners) `(Done ,owners)]
    [`(One ,owners ,state) `(Solo ,owners ,state)]
    [`(Yield ,owners ,answer ,tail)
     `(Emit ,owners ,answer ,(promote-render env ... tail (owners-support owners ancestry)))]
    [`(Delay ,owners ,body)
     (define inner (owners-support owners ancestry))
     `(Forced ,owners ,(promote-render env ... (promote-search env ... body inner) inner))]))

(define (promote-commit env ... search ancestry)
  (match search
    [`(Empty ,owners) `(Done ,owners)]
    [`(One ,owners ,state) `(Solo ,owners ,state)]
    [`(Yield ,owners ,answer ,tail)
     `(Emit ,owners ,answer ,(promote-commit env ... tail (owners-support owners ancestry)))]
    [`(Delay ,owners ,body) `(More (Delay ,owners ,body))]))

(define (promote-advance env ... frontier ancestry)
  (match frontier
    [`(Done ,_) frontier]
    [`(Solo ,_ ,_) frontier]
    [`(Emit ,owners ,answer ,tail)
     `(Emit ,owners ,answer ,(promote-advance env ... tail (owners-support owners ancestry)))]
    [`(Forced ,owners ,tail)
     `(Forced ,owners ,(promote-advance env ... tail (owners-support owners ancestry)))]
    [`(More (Delay ,owners ,body))
     (define inner (owners-support owners ancestry))
     `(Forced ,owners ,(promote-commit env ... (promote-search env ... body inner) inner))]))

(define (promote-collect env ... frontier ancestry)
  (match frontier
    [`(Done ,_) frontier]
    [`(Solo ,_ ,_) frontier]
    [`(Emit ,owners ,answer ,tail)
     `(Emit ,owners ,answer ,(promote-collect env ... tail (owners-support owners ancestry)))]
    [`(Forced ,owners ,tail)
     `(Forced ,owners ,(promote-collect env ... tail (owners-support owners ancestry)))]
    [`(More (Delay ,owners ,body))
     (define inner (owners-support owners ancestry))
     `(Forced ,owners ,(promote-collect env ...
                                      (promote-commit env ... (promote-search env ... body inner) inner)
                                      inner))]))

(define (promote-observe env ... computation ancestry)
  (match computation
    [(? (lambda (value) (redex-match? parent F value)) value) value]
    [`(render ,search) (promote-render env ... (promote-search env ... search ancestry) ancestry)]
    [`(commit ,search) (promote-commit env ... (promote-search env ... search ancestry) ancestry)]
    [`(advance ,frontier) (promote-advance env ... (promote-observe env ... frontier ancestry) ancestry)]
    [`(collect ,frontier) (promote-collect env ... (promote-observe env ... frontier ancestry) ancestry)]
    [`(Emit ,owners ,answer ,tail)
     `(Emit ,owners ,answer ,(promote-observe env ... tail (owners-support owners ancestry)))]
    [`(Forced ,owners ,tail)
     `(Forced ,owners ,(promote-observe env ... tail (owners-support owners ancestry)))]))

(define (promote env ... computation [ancestry '()])
  (unless (redex-match? parent q computation) (raise-argument-error 'promote "feature computation" computation))
  (if (redex-match? parent c computation)
      (promote-search env ... computation ancestry) (promote-observe env ... computation ancestry)))

(define (evaluate env ... computation [ancestry '()])
  (define answers
    (if (redex-match? parent c computation)
        (judgment-holds (search-big ,env ... ,ancestry ,computation SV trace) (SV trace))
        (judgment-holds (observe-big ,env ... ,ancestry ,computation F trace) (F trace))))
  (match answers
    [(list (list value labels)) (list value labels)]
    [_ (error 'evaluate "nonunique or missing finite Big derivation: ~e" answers)]))

(define (raw-derivations env ... computation [ancestry '()])
  (if (redex-match? parent c computation)
      (build-derivations (search-big ,env ... ,ancestry ,computation any_value any_trace))
      (build-derivations (observe-big ,env ... ,ancestry ,computation any_value any_trace))))
)]))
