#lang racket

(provide final-config?
         tagged-successor-name
         tagged-successor-cfg
         canonical-term
         overlap-kind
         overlap-event)

(define (final-answer-stream? as)
  (match as
    ['(empty-stream) #t]
    [`(⊤ ,_) #t]
    [`((⊤ ,_) + ,rest) (final-answer-stream? rest)]
    [_ #f]))

(define (final-config? cfg)
  (match cfg
    [`(,_gamma (empty-tree) ,as) (final-answer-stream? as)]
    [_ #f]))

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
