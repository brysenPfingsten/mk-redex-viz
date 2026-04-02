#lang racket
(require racket/match
         racket/string
         "../Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/model-registry.rkt"
         "../Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/transpiler.rkt")

(define STEP-CAP 25)
(define HARD-CAP 400)

(define appendo #<<P
(defrel (appendo l s out)
  (conde
    [(== l '())
    (== s out)]
    [(fresh (a d res)
      (== l (cons a d))
      (== out (cons a res))
      (appendo d s res))]
  ))

(run* (q) (appendo (list 'minikanren) (list 'visualizer) q))
P
)

(define appendoh1 #<<P
(defrel (appendoh l s out)
  (conde
   [(== l '()) (== s out)]
   [(fresh (a d res)
      (== l (cons a d))
      (== out (cons a res))
      (appendoh d s out))]))

(run* (q) (appendoh '(dog) q '(dog cat)))
P
)

(define appendoh2 #<<P
(defrel (appendoh l s out)
  (conde
   [(== l '()) (== s out)]
   [(fresh (a d res)
      (appendoh d s res)
      (== l (cons a d))
      (== out (cons a res)))]))

(run* (q r s) (appendoh q r s))
P
)

(define same #<<P
(defrel (same x y)
  (== x y))

(run* (q)
  (conde
    [(conde
       [(same q 'turtle)]
       [(same q 'cat)]
       [(== q 'dog)])]
    [(same q 'fish)]))
P
)

(define div3o #<<P
(defrel (same-counto bn)
  (conde
   [(== bn `(1 1))]
   [(fresh (a ad dd)
      (== `(,a ,ad . ,dd) bn)
      (conde
       [(== a ad) (same-counto dd)]
       [(== `(,a ,ad) '(1 0)) (mod+1o dd)]
       [(== `(,a ,ad) '(0 1)) (mod+2o dd)]))]))

(defrel (mod+1o bn)
  (conde
   [(== bn `(0 1))]
   [(fresh (a ad dd)
      (== `(,a ,ad . ,dd) bn)
      (conde
       [(== a ad) (mod+1o dd)]
       [(== `(,a ,ad) '(1 0)) (mod+2o dd)]
       [(== `(,a ,ad) '(0 1)) (same-counto dd)]))]))

(defrel (mod+2o bn)
  (conde
   [(== bn '(1))]
   [(fresh (a ad dd)
      (== `(,a ,ad . ,dd) bn)
      (conde
       [(== a ad) (mod+2o dd)]
       [(== `(,a ,ad) '(1 0)) (same-counto dd)]
       [(== `(,a ,ad) '(0 1)) (mod+1o dd)]))]))

(defrel (multiple-of-threeo bn)
  (conde
   [(== bn '())]
   [(same-counto bn)]))

(run* (q) (multiple-of-threeo q))
P
)

(define fives-fours #<<P
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

(define call-timing #<<P
(defrel (id x y)
  (== x y))

(run 3 (q)
  (id q 'ok))
P
)

(define examples
  (list (list "appendo" appendo)
        (list "appendoh 1" appendoh1)
        (list "appendoh 2" appendoh2)
        (list "fives/fours" fives-fours)
        (list "call timing" call-timing)
        (list "same" same)
        (list "div3o" div3o)))

(define (read-all port)
  (let ([expr (read port)])
    (if (eof-object? expr) '() (cons expr (read-all port)))))

(define (domain-err? e)
  (and (exn:fail? e)
       (regexp-match? #px"not in domain" (exn-message e))))

(define (unpack-succ succ)
  (match succ
    [(list nm nxt) (values nm nxt)]
    [_ (values "<unknown>" succ)]))

(define rows '())

(for ([spec (in-list all-model-specs)])
  (define mid (model-spec-id spec))
  (for ([ex (in-list examples)])
    (match-define (list lbl prog-str) ex)
    (printf "running ~a :: ~a\n" mid lbl)
    (flush-output)
    (define row
      (with-handlers ([exn:fail? (lambda (e) (list mid lbl 'error 0 #f "" "" (exn-message e) #f))])
        (define maybe-step (lookup-model-step-once mid))
        (define sexprs (read-all (open-input-string prog-str)))
        (define-values (prog _html) (parse-prog/canonical sexprs))
        (define status 'running)
        (define reason "")
        (define steps 0)
        (define last-rule "")
        (define step25-rule "")
        (define step25-state #f)
        (define st prog)
        (let loop ()
          (when (eq? status 'running)
            (with-handlers ([domain-err?
                             (lambda (e)
                               (set! status 'incompatible)
                               (set! reason (exn-message e)))])
              (define succs (maybe-step st))
              (cond
                [(null? succs) (set! status 'terminated)]
                [(> (length succs) 1)
                 (set! status 'nondet)
                 (set! reason (format "~a successors" (length succs)))]
                [else
                 (define-values (nm nxt) (unpack-succ (first succs)))
                 (set! steps (add1 steps))
                 (set! last-rule nm)
                 (set! st nxt)
                 (when (= steps STEP-CAP)
                   (set! step25-rule nm)
                   (set! step25-state nxt))
                 (if (>= steps HARD-CAP)
                     (begin
                       (set! status 'cap)
                       (set! reason (format "reached hard cap ~a" HARD-CAP)))
                     (loop))]))))
        (list mid lbl status steps (and (eq? status 'terminated) (<= steps STEP-CAP)) last-rule step25-rule reason step25-state)))
    (set! rows (cons row rows))
    (match-define (list _ _ status steps by25 last s25 reason _s25state) row)
    (printf "  => status=~a steps=~a by25=~a last=~a step25=~a\n" status steps by25 last s25)
    (when (and (not (eq? status 'terminated)) (not (string=? reason "")))
      (printf "     reason: ~a\n" reason))
    (flush-output)))

(set! rows (reverse rows))

(define out "/tmp/matrix_audit_manual.csv")
(call-with-output-file out
  (lambda (op)
    (displayln "model,label,status,steps,by25,last,step25,reason" op)
    (for ([r (in-list rows)])
      (match-define (list m l st n by25 last s25 reason _s25state) r)
      (define (q x)
        (string-append "\"" (string-replace (format "~a" x) "\"" "\"\"") "\""))
      (fprintf op "~a,~a,~a,~a,~a,~a,~a,~a\n"
               (q m) (q l) (q st) n (if by25 "true" "false") (q last) (q s25) (q reason))))
  #:exists 'replace)

(printf "wrote ~a\n" out)

(define focus
  (for/first ([r (in-list rows)]
              #:when (and (equal? (first r) "microKanren-rail")
                          (equal? (second r) "fives/fours")))
    r))
(when focus
  (match-define (list _ _ st n by25 last s25 reason s25state) focus)
  (printf "FOCUS microKanren-rail/fives-fours status=~a steps=~a by25=~a last=~a step25=~a reason=~a\n"
          st n by25 last s25 reason)
  (when s25state
    (call-with-output-file "/tmp/matrix_audit_focus_step25.txt"
      (lambda (op) (display (format "~s" s25state) op))
      #:exists 'replace)
    (printf "wrote /tmp/matrix_audit_focus_step25.txt\n")))
