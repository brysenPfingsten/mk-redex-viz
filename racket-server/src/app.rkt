#lang racket

(require json
         net/url-structs
         web-server/http
         web-server/servlet-env
         "program-runner.rkt"
         "sexpr-read.rkt"
         "syntax-checking.rkt"
         "transpiler.rkt")

(provide step! back! reset! init! source-convert!)

(define (request->payload req)
  (bytes->jsexpr (request-post-data/raw req)))

(define (payload->source-options payload)
  (define source-mode
    (normalize-source-mode (hash-ref payload 'sourceMode default-source-mode)))
  (values source-mode
          (normalize-compile-profile (hash-ref payload 'compileProfile #f)
                                     source-mode)))

;; HTTP and direct library use the same native configurations, history,
;; observation boundaries, and committed-answer extraction for each model.
(define (session->response ses #:html [html #f] #:cookie [session-id #f])
  (define status (model-session-status ses))
  (define-values (reductions advances) (model-session-operation-counts ses))
  (define body
    (hasheq 'stepName (model-session-current-step-name ses)
            'stepKind (symbol->string (model-session-current-step-kind ses))
            'step (model-session-step-index ses)
            'reductionCount reductions
            'advanceCount advances
            'executionStatus (symbol->string status)
            'answerCount (length (model-session-current-answer-nodes ses))
            'configuration
            (with-output-to-string
              (lambda () (pretty-write (model-session-current-config ses))))
            'program (jsexpr->string (model-session-current-picture ses))))
  (define headers
    (append
     (list (make-header #"X-Execution-Status"
                        (string->bytes/utf-8 (symbol->string status))))
     (if (model-session-done? ses) (list (make-header #"X-Done" #"true")) '())
     (if (zero? (model-session-step-index ses))
         (list (make-header #"X-Is-Start" #"true")) '())
     (if session-id
         (list (make-header #"Set-Cookie"
                            (string->bytes/utf-8
                             (format "session-id=~a; Path=/; SameSite=Lax" session-id))))
         '())))
  (response/jsexpr (if html (hash-set body 'htmlGuids html) body)
                   #:mime-type #"application/json; charset=utf-8"
                   #:headers headers))

(define (step! ses)
  (define next (model-session-step ses))
  (values (session->response next) next))

(define (back! ses)
  (define previous (model-session-back ses))
  (values (session->response previous) previous))

(define (reset! ses)
  (define initial (model-session-reset ses))
  (values (session->response initial) initial))

(define (init! _previous req session-id)
  (define payload (request->payload req))
  (define raw-prog (hash-ref payload 'text))
  (define-values (source-mode compile-profile) (payload->source-options payload))
  (define strategy (normalize-search-strategy (hash-ref payload 'searchStrategy #f)))
  (when (equal? source-mode "mini")
    (check-syntax-capture-error raw-prog))
  (define-values (configuration html query)
    (parse-prog/canonical (read-all-sexprs (open-input-string raw-prog))
                          #:source-mode source-mode
                          #:compile-profile compile-profile
                          #:search-strategy strategy))
  (define ses (open-compiled configuration query strategy))
  (values (session->response ses #:html html #:cookie session-id) ses))

(define (source-convert! req)
  (define payload (request->payload req))
  (define raw-prog (hash-ref payload 'text))
  (define target-source-mode
    (normalize-source-mode (hash-ref payload 'targetSourceMode "micro")))
  (unless (equal? target-source-mode "micro")
    (error 'source-convert! "unsupported target source mode: ~a" target-source-mode))
  (define-values (source-mode compile-profile) (payload->source-options payload))
  (when (equal? source-mode "mini")
    (check-syntax-capture-error raw-prog))
  (response/jsexpr
   (hasheq 'source
           (render-micro-source (read-all-sexprs (open-input-string raw-prog))
                                #:source-mode source-mode
                                #:compile-profile compile-profile))
   #:mime-type #"application/json; charset=utf-8"))

(define session-table (make-hash))

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
  (hash-ref session-table session-id #f))

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
  (define port (or (and (getenv "PORT") (string->number (getenv "PORT"))) 5000))
  (serve/servlet handled-dispatcher
                 #:port port
                 #:servlet-regexp #rx""
                 #:listen-ip (or (getenv "HOST") "0.0.0.0")
                 #:launch-browser? #f))
