#lang racket

(provide (struct-out premachine-config)
         (struct-out cfree-config)
         (struct-out machine))

(struct premachine-config (query root-scope term obs) #:transparent)
(struct cfree-config (query root-scope term) #:transparent)
(struct machine (query root-scope focus ctx obs) #:transparent)
