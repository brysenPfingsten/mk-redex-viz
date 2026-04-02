#lang racket

(provide canonical-flat->calls-config
         calls-config->canonical-flat)

(define (canonical-flat->calls-config cfg)
  (match cfg
    [`(,Γ ,s ,as) `(,Γ (,s ,as))]
    [_ (error 'canonical-flat->calls-config
              "expected flat canonical config '(Γ s as), got ~e"
              cfg)]))

(define (calls-config->canonical-flat cfg)
  (match cfg
    [`(,Γ (,s ,as)) `(,Γ ,s ,as)]
    [_ (error 'calls-config->canonical-flat
              "expected internal calls config '(Γ (s as)), got ~e"
              cfg)]))
