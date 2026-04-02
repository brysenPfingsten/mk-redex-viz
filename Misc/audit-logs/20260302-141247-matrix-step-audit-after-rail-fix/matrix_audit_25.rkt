#lang racket
(require racket/match
         "../Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/model-registry.rkt"
         "../Users/jhemann/Code/Modeling-miniKanren-in-Redex/racket-server/src/transpiler.rkt")

(define HARD-CAP 25)

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

(define (final-config? cfg)
  (match cfg
    [`(,_ ,_ (empty-tree)) #t]
    [_ #f]))

(define (domain-err? e)
  (and (exn:fail? e) (regexp-match? #px"not in domain" (exn-message e))))

(for ([spec (in-list all-model-specs)])
  (define mid (model-spec-id spec))
  (for ([ex (in-list examples)])
    (match-define (list lbl pstr) ex)
    (define maybe-step (lookup-model-step-once mid))
    (define sexprs (read-all (open-input-string pstr)))
    (define-values (prog _h) (parse-prog/canonical sexprs))
    (define st prog)
    (define steps 0)
    (define status 'running)
    (define last-rule "")
    (define reason "")
    (let loop ()
      (when (eq? status 'running)
        (with-handlers ([domain-err?
                         (lambda (e)
                           (set! status 'incompatible)
                           (set! reason "not-in-domain"))])
          (define succs (maybe-step st))
          (cond
            [(null? succs)
             (set! status (if (final-config? st) 'value 'stuck))]
            [(> (length succs) 1)
             (set! status 'nondet)
             (set! reason (format "~a successors" (length succs)))]
            [else
             (match-define (list nm nxt) (first succs))
             (set! last-rule nm)
             (set! st nxt)
             (set! steps (add1 steps))
             (if (>= steps HARD-CAP)
                 (begin
                   (set! status 'cap)
                   (set! reason (format "cap~a" HARD-CAP)))
                 (loop))]))))
    (printf "~a | ~a | ~a | steps=~a | last=~a | ~a\n" mid lbl status steps last-rule reason)
    (flush-output)))
