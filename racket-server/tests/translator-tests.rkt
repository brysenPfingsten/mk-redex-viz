#lang racket
(require redex redex/gui)
(require redex/reduction-semantics)
(require rackunit)
(check-redundancy #t)

(require "../src/definitions.rkt"
         "../src/judgment-forms.rkt"
         "../src/reduction-relations/reduction-relations.rkt")

(module+ test
  (test-true "translation successful"
  (redex-match?
   L
   Γ
   '((r:assoco (x:key x:table x:value)
               (∃ (x:car x:table-cdr)
                  ((x:table =? (x:car : x:table-cdr))
                   ∧
                   (((x:key : x:value) =? x:car)
                    ∨
                    (r:assoco x:key x:table-cdr x:value)))))
     (r:same-lengtho (x:l1 x:l2)
                     (((x:l1 =? empty) ∧ (x:l2 =? empty))
                      ∨
                      (∃ (x:car1 x:cdr1 x:car2 x:cdr2)
                         ((x:l1 =? (x:car1 : x:cdr1))
                          ∧
                          ((x:l2 =? (x:car2 : x:cdr2))
                           ∧
                           (r:same-lengtho x:cdr1 x:cdr2))))))
     (r:make-assoc-tableo (x:l1 x:l2 x:table)
                          (((x:l1 =? empty)
                            ∧
                            ((x:l2 =? empty)
                             ∧
                             (x:table =? empty)))
                           ∨
                           (∃ (x:car1 x:cdr1 x:car2 x:cdr2 x:cdr3)
                              ((x:l1 =? (x:car1 : x:cdr1))
                               ∧
                               ((x:l2 =? (x:car2 : x:cdr2))
                                ∧
                                ((x:table =? ((x:car1 : x:car2) : x:cdr3))
                                 ∧ (r:make-assoc-tableo x:cdr1 x:cdr2 x:cdr3)))))))
     )))

(test-true "dfao from paper translated properly"
  (redex-match?
   L
   p
   (term (prog
          ((r:poso (x:n)
                   (∃ (x:a x:d)
                      ((x:a : x:d) =? x:n)))
           (r:dfao (x:l x:state)
                   (((x:l =? empty) ∧ (x:state =? "q1"))
                    ∨
                    (∃ (x:a x:d x:next-state)
                       ((x:l =? (x:a : x:d))
                        ∧
                        ((((x:a =? "z")
                           ∧
                           ((r:poso x:d)
                            ∧
                            (((x:state =? "q1")
                              ∧
                              (x:next-state =? "q1"))
                             ∨
                             (((x:state =? "q2")
                               ∧
                               (x:next-state =? "q3"))
                              ∧
                              ((x:state =? "q3")
                               ∧
                               (x:next-state =? "q2"))))))
                          ∨
                          ((x:a =? "s")
                           ∧
                           (((x:state =? "q1")
                             ∧
                             (x:next-state =? "q2"))
                            ∨
                            (((x:state =? "q2")
                              ∧
                              (x:next-state =? "q1"))
                             ∧
                             ((x:state =? "q3")
                              ∧
                              (x:next-state =? "q3"))))))
                         ∧
                         (r:dfao x:d x:next-state)))))))
          ((∃ (x:q) (r:dfao x:q "q1")))
           (state () 0)))))

  (test-results))


#;(module+ test-translator
    (test-true
     ""
     (judgment-holds
      (closed-program?
       (parse-prog
        '(defrel (assoco key table value)
           (fresh (car table-cdr)
             (== table `(,car . ,table-cdr))
             (conde ((== `(,key . ,value) car))
                    ((assoco key table-cdr value)))))
        '(defrel (same-lengtho l1 l2)
           (conde ((== l1 '()) (== l1 '()))
                  ((fresh (car1 cdr1 car2 cdr2)
                     (== l1 `(,car1 . ,cdr1))
                     (== l2 `(,car2 . ,cdr2))
                     (same-lengtho cdr1 cdr2)))))
        '(defrel (make-assoc-tableo l1 l2 table)
           (conde ((== l1 '()) (== l1 '()) (== table '()))
                  ((fresh (car1 cdr1 car2 cdr2 cdr3)
                     (== l1 `(,car1 . ,cdr1))
                     (== l2 `(,car2 . ,cdr2))
                     (== table `((,car1 . ,car2) . ,cdr3))
                     (make-assoc-tableo cdr1 cdr2 cdr3)))))
        '(run 5 (q) (same-lengtho '(abc def ghi) q)))))))
