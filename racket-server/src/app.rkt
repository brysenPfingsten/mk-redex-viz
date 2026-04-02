#lang racket

(require json
         net/url-structs
         web-server/http
         web-server/servlet-env
         "canonical-json.rkt"
         "search-runtime.rkt"
         "search-strategy.rkt"
         "sexpr-read.rkt"
         "syntax-checking.rkt"
         "transpiler.rkt"
         "zipper.rkt")

(provide step!
         back!
         reset!
         init!
         init-session
         make-stepper
         (struct-out step)
         (struct-out session)
         make-empty-session
         source-convert!)

(define (request->payload req)
  (bytes->jsexpr (request-post-data/raw req)))

(define (payload->source-options payload)
  (define source-mode
    (normalize-source-mode (hash-ref payload 'sourceMode default-source-mode)))
  (define compile-profile
    (normalize-compile-profile (hash-ref payload 'compileProfile #f)
                               source-mode))
  (values source-mode compile-profile))

(define (payload->search-strategy payload)
  (normalize-search-strategy (hash-ref payload 'searchStrategy #f)))

(struct step (name prog) #:transparent)
(struct session (zipper stepper nqv search-strategy) #:transparent)

(define (make-empty-session [strategy default-search-strategy])
  (define normalized (normalize-search-strategy strategy))
  (session (make-empty-zipper)
           (make-stepper (lookup-search-step-once normalized))
           1
           normalized))

(define session-table (make-hash))

(define/match (init-session ses prog)
  [((and ses (session zip _ _ _)) prog)
   (define seeded-zipper
     (zipper-add (zipper-reset zip)
                 (step "Initialize Program" prog)))
   (struct-copy session ses
                [zipper seeded-zipper]
                [nqv (num-query-vars/canonical prog)])])

(define/match (step->response a-step a-idx nqv)
  [((step name prog) a-idx nqv)
   (response/jsexpr
    (hasheq 'stepName name
            'step a-idx
            'program (to-json/canonical prog nqv))
    #:mime-type #"application/json; charset=utf-8")])

(define/match (step->response/start a-step nqv)
  [((step name prog) nqv)
   (response/jsexpr
    (hasheq 'stepName name
            'step 0
            'program (to-json/canonical prog nqv))
    #:mime-type #"application/json; charset=utf-8"
    #:headers (list (make-header #"X-Is-Start" #"true")))])

(define/match (step/html/cookie->response a-step tagged-prog session-id nqv)
  [((step name prog) tagged-prog session-id nqv)
   (response/jsexpr
    (hasheq 'stepName name
            'step 0
            'program (to-json/canonical prog nqv)
            'htmlGuids tagged-prog)
    #:mime-type #"application/json; charset=utf-8"
    #:headers
    (list
     (make-header
      #"Set-Cookie"
      (string->bytes/utf-8
       (format "session-id=~a; Path=/; SameSite=Lax" session-id)))))])

(define (send-end-step)
  (response/jsexpr (json-null)
                   #:mime-type #"application/json; charset=utf-8"
                   #:headers (list (make-header #"X-Done" #"true"))))

(define (make-stepper step-term)
  (lambda (z nqv)
    (define-values (maybe-next z^) (zipper-forward z))
    (cond
      [(step? maybe-next)
       (values (step->response maybe-next (zipper-idx z^) nqv) z^)]
      [else
       (match-define (zipper _ curr _ _) z)
       (match (step-term (step-prog curr))
         ['()
          (values (send-end-step) z)]
         [(cons (list name new-prog) _)
          (define new-step (step name new-prog))
          (define z^^ (zipper-add z new-step))
          (values (step->response new-step (zipper-idx z^^) nqv) z^^)])])))

(define/match (step! ses)
  [((and ses (session zip stepper nqv _)))
   (define-values (response zip^) (stepper zip nqv))
   (values response (struct-copy session ses [zipper zip^]))])

(define (bind-session-search-strategy ses strategy)
  (define normalized (normalize-search-strategy strategy))
  (struct-copy session ses
               [stepper (make-stepper (lookup-search-step-once normalized))]
               [search-strategy normalized]))

(define (init! ses req ses-id)
  (define payload (request->payload req))
  (define raw-prog (hash-ref payload 'text))
  (define-values (source-mode compile-profile) (payload->source-options payload))
  (define search-strategy (payload->search-strategy payload))
  (when (equal? source-mode "mini")
    (check-syntax-capture-error raw-prog))
  (define sexpr-prog (read-all-sexprs (open-input-string raw-prog)))
  (define-values (model-prog html-prog)
    (parse-prog/canonical sexpr-prog
                          #:source-mode source-mode
                          #:compile-profile compile-profile))
  (unless (canonical-target-in-domain? model-prog canonical-parser-target-id)
    (error 'init!
           "transpiler produced a program outside canonical target ~a"
           canonical-parser-target-id))
  (check-canonical-well-formed model-prog canonical-parser-target-id)
  (check-search-config search-strategy model-prog)
  (define ses^
    (init-session (bind-session-search-strategy ses search-strategy) model-prog))
  (match-define (session init-zipper _ nqv _) ses^)
  (define init-step (zipper-curr init-zipper))
  (values (step/html/cookie->response init-step
                                      html-prog
                                      ses-id
                                      nqv)
          ses^))

(define/match (reset! ses)
  [((and ses (session (and z (zipper prev curr _ _)) _ nqv _)))
   (define init-step
     (cond
       [(step? curr) curr]
       [else
        (for/first ([entry (in-list (reverse prev))]
                    #:when (step? entry))
          entry)]))
   (unless (step? init-step)
     (error 'reset! "session has no initial program to reset to"))
   (define ses^
     (struct-copy session ses
                  [zipper (zipper-add (make-empty-zipper) init-step)]))
   (values (step->response/start init-step nqv)
           ses^)])

(define/match (back! ses)
  [((and ses (session (and z (zipper _ curr _ _)) _ nqv _)))
   (define-values (maybe-back z^) (zipper-back z))
   (define current-step
     (cond
       [(step? maybe-back) maybe-back]
       [(step? curr) curr]
       [else (error 'back! "session has no current step")]))
   (define response
     (cond
       [(zero? (zipper-idx z^))
        (step->response/start current-step nqv)]
       [else
        (step->response current-step (zipper-idx z^) nqv)]))
   (values response
           (struct-copy session ses [zipper z^]))])

(define (source-convert! req)
  (define payload (request->payload req))
  (define raw-prog (hash-ref payload 'text))
  (define target-source-mode
    (normalize-source-mode (hash-ref payload 'targetSourceMode "micro")))
  (unless (equal? target-source-mode "micro")
    (error 'source-convert!
           "unsupported target source mode: ~a"
           target-source-mode))
  (define-values (source-mode compile-profile) (payload->source-options payload))
  (when (equal? source-mode "mini")
    (check-syntax-capture-error raw-prog))
  (define sexpr-prog (read-all-sexprs (open-input-string raw-prog)))
  (response/jsexpr
   (hasheq 'source
           (render-micro-source sexpr-prog
                                #:source-mode source-mode
                                #:compile-profile compile-profile))
   #:mime-type #"application/json; charset=utf-8"
   #:code 200))

(define (cookie-field->string v)
  (cond
    [(string? v) v]
    [(bytes? v) (bytes->string/utf-8 v)]
    [(symbol? v) (symbol->string v)]
    [else (format "~a" v)]))

(define (get-or-create-session-id req)
  (or
   (for/first ([c (in-list (request-cookies req))]
               #:when (string=? (cookie-field->string (client-cookie-name c))
                                "session-id"))
     (cookie-field->string (client-cookie-value c)))
   (symbol->string (gensym 'sess-))))

(define (get-session session-id)
  (hash-ref session-table
            session-id
            (lambda ()
              (define new-session (make-empty-session))
              (hash-set! session-table session-id new-session)
              new-session)))

(define (put-session! session-id ses)
  (hash-set! session-table session-id ses))

(define (get-path req)
  (string-join (map path/param-path (url-path (request-uri req))) "/"))

(define (dispatcher req)
  (match (get-path req)
    ["post/source-convert"
     (source-convert! req)]
    [_
     (define session-id (get-or-create-session-id req))
     (define ses (get-session session-id))
     (match (get-path req)
       ["get/next"
        (define-values (response ses^) (step! ses))
        (put-session! session-id ses^)
        response]
       ["post/init"
        (define-values (response ses^) (init! ses req session-id))
        (put-session! session-id ses^)
        response]
       ["post/reset"
        (define-values (response ses^) (reset! ses))
        (put-session! session-id ses^)
        response]
       ["post/back"
        (define-values (response ses^) (back! ses))
        (put-session! session-id ses^)
        response])]))

(define (handled-dispatcher req)
  (with-handlers
      [(exn:fail?
        (lambda (e)
          (response/jsexpr (hasheq 'error (exn-message e)) #:code 400)))]
    (dispatcher req)))

(module+ main
  (serve/servlet handled-dispatcher
                 #:port 5000
                 #:servlet-regexp #rx""
                 #:listen-ip "0.0.0.0"
                 #:launch-browser? #f))
