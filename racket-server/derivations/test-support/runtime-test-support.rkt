#lang racket

(provide final-config?
         tagged-successor-name
         tagged-successor-cfg
         canonical-term
         overlap-kind
         overlap-event)

(define (final-config? configuration)
  ;; Shared observation for strict and historical test carriers. Production
  ;; routing need not accept the older carrier to test its completed Frontier.
  (match configuration
    [`(program ,_ ,frontier) (final-frontier? frontier)]
    [`(,_ ,frontier) (final-frontier? frontier)]
    [_ #f]))

(define (final-frontier? frontier)
  (match frontier
    ;; Solo belongs to the current account; Last is observed only in tests
    ;; of the independent earlier dormant-branch account.
    [(or `(Done ,_) `(Solo ,_ ,_) `(Last ,_ (Answer ,_ ,_))) #t]
    [(or `(Emit ,_ (Answer ,_ ,_) ,tail) `(Forced ,_ ,tail))
     (final-frontier? tail)]
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

(module+ test
  (require rackunit)
  (test-case "complete observation is distinct from a halted delayed Frontier"
    (define state '(state () () () (label "initial")))
    (define paused `(More (Delay (Owners) (eval (Owners) (succeed (label "ready")) ,state))))
    (for ([frontier (list '(Done (Owners))
                         `(Solo (Owners) ,state))])
      (check-true (final-config? `(program () ,frontier))))
    (check-true (final-config? `(() (Last (Owners) (Answer (Owners) ,state)))))
    (check-false (final-config? `(program () ,paused)))
    (check-false (final-config? `(program () (Forced (Owners) ,paused))))
    (check-false (final-config? `(() (More (PendingDelay (Owners)
                                          (Work (Owners) (succeed (label "ready")) ,state))))))))
