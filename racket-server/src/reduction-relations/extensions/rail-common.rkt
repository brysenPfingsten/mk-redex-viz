#lang racket

(require redex/reduction-semantics
         "../../extensions/l4-railroad-syntax.rkt")

(check-redundancy #t)

(provide L4/K)

(define-extended-language L4/K
  L4
  ;; Base stepping context in railroad syntax:
  ;; - left branch for <-+
  ;; - right branch for +-> (rail mode)
  ;; - never descend through delay
  [K ::= hole
         (K × g c)
         (K <-+ s)
         (s +-> K)]
  ;; Core staged contexts inherited from L3/K relations.
  [Kcore ::= hole
             (Kcore × g c)]
  [Kleft ::= hole
             (Kleft <-+ s)
             (s +-> Kleft)]
  [Kcall ::= hole
             (Kcall × g c)]
  [Kinvoke ::= hole
               (Kinvoke × g c)]
  [K3 ::= K]
  [K4 ::= K])
