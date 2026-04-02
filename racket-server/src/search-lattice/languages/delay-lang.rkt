#lang racket

(require redex/reduction-semantics
         "./core-lang.rkt")

(provide delay-lang)

(check-redundancy #t)

;; Explicit delayed-goal/runtime delay layer, independent of relation calls.
(define-extended-language delay-lang core-lang
  [g ....
     (suspend g tag)]
  [search ....
          (delay runnable-search)]
  [cfg ....
       (Bounced cfg)]
  ;; First committed shell context on the delay branch.
  ;; First divergent layer: L1/delay.
  ;; Allowed extension direction: add shell constructors only.
  [QShell ::= hole
              (FreshenedShell c QShell tag)
              (Bounced QShell)])
