#lang racket

(require json
         (only-in web-server/http/request-structs header-field header-value)
         web-server/http/response-structs
         "../src/app.rkt"
         (only-in "../src/program-runner.rkt"
                  model-session-current-config model-session-current-answer-nodes
                  model-session-current-host-answers model-session-status
                  model-session-step-index model-session-done?)
         "./example-compat-tests.rkt"
         "./test-http-helpers.rkt")

(define disj-relcall-program
  "(defrel (same x y) (== x y))
   (defrel (wrap x) (== x x) (same x 'cat))
   (run* (q) (conde [(wrap q)] [(== q 'dog)]))")

(define (expect condition message)
  (unless condition (error 'ui-payload-smoke message)))

(define (response->payload response)
  (define payload (string->jsexpr (response-body->string response)))
  (expect (hash? payload) "every response, including completion, must retain its JSON payload")
  payload)

(define (payload->program payload)
  (string->jsexpr (hash-ref payload 'program)))

(define (picture-names node)
  (match node
    [(hash* ['name name] ['children children] #:open)
     (cons name (append-map picture-names children))]
    [(hash* ['name name] #:open) (list name)]
    [_ '()]))

(define (example-src label)
  (or (for/first ([pr (in-list (frontend-example-programs))]
                 #:do [(match-define (cons example-label src) pr)]
                 #:when (equal? example-label label))
        src)
      (error 'example-src "missing frontend example: ~a" label)))

(define (snapshot response session)
  (define payload (response->payload response))
  (define picture (payload->program payload))
  (define status (symbol->string (model-session-status session)))
  (expect (equal? status (hash-ref payload 'executionStatus))
          "HTTP status must agree with the actual matrix session")
  (expect (= (length (model-session-current-answer-nodes session))
             (hash-ref payload 'answerCount))
          "HTTP answerCount must count committed answers")
  (expect (= (model-session-step-index session) (hash-ref payload 'step))
          "HTTP step index must agree with session history")
  (define done-header?
    (for/or ([entry (in-list (response-headers response))])
      (and (equal? (header-field entry) #"X-Done")
           (equal? (header-value entry) #"true"))))
  (expect (equal? done-header? (model-session-done? session))
          "X-Done must mean complete, not a paused More Frontier")
  (hasheq 'step (hash-ref payload 'step)
          'stepName (hash-ref payload 'stepName)
          'stepKind (hash-ref payload 'stepKind)
          'executionStatus status
          'answerCount (hash-ref payload 'answerCount)
          'root (hash-ref picture 'name)
          'visibleConstructors (remove-duplicates (picture-names picture))
          'configuration (format "~s" (model-session-current-config session))))

(define (collect-trace response session remaining [reversed '()])
  (define accumulated (cons (snapshot response session) reversed))
  (cond
    [(or (zero? remaining) (model-session-done? session))
     (values (reverse accumulated) session)]
    [else
     (define-values (next-response next-session) (step! session))
     (collect-trace next-response next-session (sub1 remaining) accumulated)]))

(define (init-session-for source [mode "mini"] [profile #f])
  (define payload
    (if profile
        (hasheq 'text source 'sourceMode mode 'compileProfile profile)
        (hasheq 'text source 'sourceMode mode)))
  (init! #f (make-post-init-request source payload) 'ui-smoke-id))

(define (run-trace source count [mode "mini"] [profile #f])
  (define-values (response session) (init-session-for source mode profile))
  (define-values (trace final-session) (collect-trace response session count))
  (values (hasheq 'source source 'sourceMode mode 'trace trace)
          final-session))

(define (trace-has? report key expected)
  (for/or ([entry (in-list (hash-ref report 'trace))])
    (equal? (hash-ref entry key) expected)))

(define (trace-shows? report constructor)
  (for/or ([entry (in-list (hash-ref report 'trace))])
    (member constructor (hash-ref entry 'visibleConstructors))))

(define (history-report)
  (define-values (initial-response initial) (init-session-for (example-src "fives/fours")))
  (define-values (_response1 first-step) (step! initial))
  (define-values (second-response second-step) (step! first-step))
  (define-values (_back-response back) (back! second-step))
  (define-values (replay-response replay) (step! back))
  (define-values (reset-response reset) (reset! replay))
  (expect (equal? (model-session-current-config first-step)
                  (model-session-current-config back)) "back must restore the exact source configuration")
  (expect (equal? (response->payload second-response) (response->payload replay-response))
          "forward replay must restore the exact payload")
  (expect (equal? (model-session-current-config initial)
                  (model-session-current-config reset)) "reset must restore the exact initial program")
  (expect (equal? (hash-ref (response->payload initial-response) 'program)
                  (hash-ref (response->payload reset-response) 'program))
          "reset must restore the initial picture")
  (hasheq 'backRestored #t 'forwardRestored #t 'resetRestored #t
          'initial (snapshot initial-response initial)
          'reset (snapshot reset-response reset)))

(define-values (failed failed-session) (run-trace "(run* (q) (== 'a 'b))" 20 "micro"))
(expect (model-session-done? failed-session) "failure must finish")
(expect (trace-shows? failed "Done") "completed failure must retain Done")
(expect (null? (model-session-current-answer-nodes failed-session)) "Done must not produce an answer")

(define-values (singleton singleton-session) (run-trace "(run* (q) (== q 'cat))" 20 "micro"))
(expect (model-session-done? singleton-session) "singleton must finish")
(expect (trace-shows? singleton "Solo") "completed singleton must be a direct Solo")
(expect (not (trace-shows? singleton "Last")) "strict singleton must not use a Last wrapper")
(expect (equal? (model-session-current-host-answers singleton-session) '(cat))
        "singleton must reify its explicit query variable")

(define-values (disj disj-session)
  (run-trace disj-relcall-program 140 "mini"
             (hasheq 'conjAssoc "right" 'disjAssoc "left" 'delayPlacement "disj")))
(expect (model-session-done? disj-session) "finite relation/disjunction program must finish")
(for ([label '("eval-call" "eval-suspend" "advance-delay")])
  (expect (trace-has? disj 'stepName label) (format "full trace must contain ~a" label)))
(expect (trace-has? disj 'stepKind "public-operation") "paused Frontier must have an explicit public advance")
(expect (trace-has? disj 'executionStatus "paused") "More must report paused before advancement")
(expect (trace-shows? disj "Forced") "public advancement must retain Forced")
(expect (equal? (model-session-current-host-answers disj-session) '(cat dog))
        "disjunction profile must preserve its committed answer order")

(define-values (internal-force internal-session)
  (run-trace "(run* (q) (disj (Zzz (== q 'left)) (Zzz (== q 'right))))" 70 "micro"))
(expect (model-session-done? internal-session) "finite delayed choice must finish")
(for ([label '("mplus-delay" "force-delay" "advance-delay")])
  (expect (trace-has? internal-force 'stepName label)
          (format "delayed choice must contain ~a" label)))
(expect (equal? (model-session-current-host-answers internal-session) '(left right))
        "internal resumption must preserve choice orientation")

(define-values (productive productive-session) (run-trace (example-src "fives/fours") 80))
(expect (not (model-session-done? productive-session)) "productive infinite example must remain resumable")
(expect (positive? (length (model-session-current-answer-nodes productive-session)))
        "bounded recursive example should have committed a finite prefix")

(define-values (unguarded unguarded-session)
  (run-trace "(defrel (loopo q) (loopo q)) (run* (q) (loopo q))" 18 "micro"))
(expect (eq? (model-session-status unguarded-session) 'running)
        "unguarded micro recursion must stay in eager evaluation")
(expect (not (trace-has? unguarded 'stepName "eval-suspend")) "calls must not insert a hidden Delay")
(expect (null? (model-session-current-answer-nodes unguarded-session))
        "unguarded recursion must not invent committed answers")

(define-values (pending-bind pending-session)
  (run-trace "(run* (q) (conj (disj (== q 'a) (== q 'b)) (== q 'c)))" 70 "micro"))
(expect (trace-shows? pending-bind "Candidate") "pending bind witness must expose Search candidates")
(expect (model-session-done? pending-session) "failed pending bind must finish")
(expect (for/and ([entry (in-list (hash-ref pending-bind 'trace))])
          (zero? (hash-ref entry 'answerCount)))
        "Search candidates must never enter the committed answer count")

(displayln
 (jsexpr->string
  (hasheq 'history (history-report)
          'completedFailure failed
          'completedSingleton singleton
          'disjunctionRelationsAndForcing disj
          'internalAndPublicForcing internal-force
          'productiveRecursion productive
          'unguardedRecursion unguarded
          'pendingBind pending-bind)))
