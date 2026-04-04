#lang racket

(require "../shared/configs.rkt"
         (prefix-in premachine: "../premachine/main.rkt")
         (prefix-in zipper: "../zipper/main.rkt")
         (prefix-in current: "./current.rkt"))

(provide premachine->cfree
         premachine->zipper
         zipper->cfree
         erase-c
         restore-c
         cfree->current-c-machine
         (rename-out [current:current-step current-step]
                     [current:project-observable project-observable]
                     [current:same-observable? same-observable?]
                     [current:current-c-scope-agrees? current-c-scope-agrees?]))

(define (premachine-pm->cfree term)
  (match term
    ['empty
     'empty-tree]
    [`(ans ,state)
     `(⊤ ,state)]
    [`(scope ,intro ,inner ,tag)
     `(FreshenedTree ,intro ,(premachine-pm->cfree inner) ,tag)]
    [`(delay ,inner)
     `(delay ,(premachine-pm->cfree inner))]
    [`(join ,left ,g)
     `(,(premachine-pm->cfree left) × ,g)]
    [`(merge left ,left ,right)
     `(,(premachine-pm->cfree left) <-+ ,(premachine-pm->cfree right))]
    [`(merge right ,left ,right)
     `(,(premachine-pm->cfree left) +-> ,(premachine-pm->cfree right))]
    [`(,g (state ,sub ,dis ,trail ,tag))
     `(,g (state ,sub ,dis ,trail ,tag))]
    [_ (error 'premachine-pm->cfree
              "unsupported premachine term: ~e"
              term)]))

(define (states->cfree-shell states tail)
  (match states
    ['()
     tail]
    [(cons state rest)
     `((⊤ ,state) + ,(states->cfree-shell rest tail))]))

(define (premachine->cfree cfg)
  (match cfg
    [(struct premachine-config (query-u* root-scope term obs))
     (cfree-config query-u*
                   root-scope
                   (states->cfree-shell obs (premachine-pm->cfree term)))]
    [_ (error 'premachine->cfree
              "unsupported premachine config: ~e"
              cfg)]))

(define (premachine->zipper cfg)
  (zipper:cfg->machine cfg))

(define (zipper->cfree machine)
  (premachine->cfree (zipper:machine->cfg machine)))

(define (restore-c term ambient)
  (current:restore-c term ambient))

(define (cfree->current-c-machine cfg)
  (match cfg
    [(struct cfree-config (query-u* root-scope term))
     `(config ,query-u* ,root-scope ,(restore-c term root-scope))]
    [_ (error 'cfree->current-c-machine
              "unsupported c-free config: ~e"
              cfg)]))

(define (erase-c datum)
  (match (current:erase-c datum)
    [`(config ,query-u* ,root-scope ,term)
     (cfree-config query-u* root-scope term)]
    [term term]))
