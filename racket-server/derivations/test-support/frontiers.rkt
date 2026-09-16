#lang racket

(provide pending?)

;; Inspect the settled Frontier shape without examining or executing the
;; source computation, closure, or resumption stored at its delayed tip.
(define (pending? frontier)
  (match frontier
    [`(,(or 'Done 'Solo) ,_ ,_ ...) #f]
    [`(Emit ,_ ,_ ,rest) (pending? rest)]
    [`(Forced ,_ ,rest) (pending? rest)]
    [`(More (Delay ,_ ,_)) #t]))
