#lang racket
(require web-server/servlet-env
         web-server/http
         net/url-structs
         json
         web-server/http/bindings)

(require "definitions.rkt"
         "reduction-relations.rkt"
         "metafunctions.rkt"
         "transpiler.rkt")

(define-struct state (red-step prog) #:transparent)

(define init-state 'uninitialized)
(define future-cache 'uninitialized)
(define trace 'uninitialized)
(define index 'uninitialized)


;; initialize-all!: program -> void
;; Purpose: Initializes all of the state variables
(define (initialize-all! prog)
  (set! init-state (state "Initialize Program" prog))
  (set! trace (list init-state))
  (set! future-cache '())
  (set! index 1))


;; state+idx->response: state nat -> response
;; Purpose: Creates a response with the current state of the program
(define (state+idx->response a-state idx)
  (match-let ([(state red-step prog) a-state])
	(let ([response (hasheq 'stepName red-step
							'step idx
							'program (to-json prog))])
	  (response/jsexpr response #:mime-type #"application/json; charset=utf-8"))))


;; send-tree/initial!: -> response
;; Purpose: Sends the last tree in the history (init-state) with a header indicating it is the initial one
(define (send-tree/initial! a-state idx)
  (match-let ([(state red-step prog) a-state])
	(let ([response (hasheq 'stepName red-step
							'step idx
							'program (to-json prog))])
	  (response/jsexpr response
					   #:mime-type #"application/json; charset=utf-8"
					   #:headers (list (make-header #"X-Is-Last" #"true"))))))


;; send-tree-and-html: json string
;; Purpose: Send the initial tree and the html embedded program
(define (state+idx/html->response a-state idx html)
  (match-let ([(state red-step prog) a-state])
	(let ([response (hasheq 'stepName red-step
							'step idx
							'program (to-json prog)
							'htmlGuids html)])
	  (response/jsexpr response
					   #:mime-type #"application/json; charset=utf-8"))))


;; send-end-state: -> response
;; Purpose: Send a response with a header indicating the program can no longer step
(define (send-end-state)
  (response/jsexpr (json-null)
                   #:mime-type #"application/json; charset=utf-8"
                   #:headers (list (make-header #"X-Done" #"true"))))


;; [Term -> [Listof [List String Term]]] ->	Response
(define (make-stepper step-term)
  (lambda ()
	(match future-cache
	  [`(,a-state . ,future-cache^)
	   (set! future-cache future-cache^)
	   (set! trace (cons a-state trace))
	   (set! index (add1 index))
	   (state+idx->response a-state index)]
	  ['()
	   (match (step-term (state-prog (first trace)))
		 ['() (send-end-state)]
		 [(cons (list red-step new-program) _)
		  (define new-state (state red-step new-program)) ;; form the new state
		  (set! trace (cons new-state trace))             ;; add to the trace
		  (set! index (add1 index))
		  (state+idx->response new-state index)])])))   ;; send response


;; step!: -> response
;; Purpose: Applies one reduction step and sends the new JSON data of that tree
(define step! (make-stepper step-once))


;; read-all: port -> ListOf sexpression
;; Purpose: To read the string program into sexpressions
(define (read-all port)
  (let ([expr (read port)])
	(if (eof-object? expr)
		'()  ;; Stop when EOF is reached
		(cons expr (read-all port)))))


;; init-tree!: request -> response
;; Purpose: To initialize the tree
(define (init-tree! req)
  (define json-data (request-post-data/raw req))                    ;; Get the JSON data from the request
  (define raw-prog (hash-ref (bytes->jsexpr json-data) 'text))      ;; Get the program from that JSON
  (define sexpr-prog (read-all (open-input-string raw-prog)))       ;; Read the program into sexpressions
  (define-values (model-prog html-prog) (parse-prog sexpr-prog))    ;; Parse the sexpressions
  (initialize-all! model-prog)                                      ;; Initialize all state variables with the model program
  (state+idx/html->response (first trace) index html-prog))              ;; Send the initial program and HTML embedded program back to the JS side


;; reset!: -> response
;; Purpose: Resets the state of the program to the initial state
(define (reset!)
  (define-values (current-downto-second listof-initial-state) (split-at-right trace 1))
  (set! future-cache (append (reverse current-downto-second) future-cache))
  (set! trace listof-initial-state)
  (set! index 0)
  (state+idx->response (first trace) index))


;; back!: -> response
;; Purpose: Step the programs backwards one step and send that state
(define (back!)
  (match trace
	[`(,initial-state) (send-tree/initial! initial-state index)]
	[`(,current-state . ,trace^)
	 (set! future-cache (cons current-state future-cache))
     (set! trace trace^)
	 (set! index (sub1 index))
	 (state+idx->response (first trace) index)]))

;; get-path: request -> string
;; Purpose: Gets the path that was pinged as it was on the javascript side
(define (get-path req)
  (string-join (map path/param-path (url-path (request-uri req))) "/"))

;; dispatcher: request -> response
;; Purpose: Maps the input request to an output response
(define (dispatcher req)
  (match (get-path req)
	["get/next"   (step!)]
	["post/init"  (init-tree! req)]
	["post/reset" (reset!)]
	["post/back"  (back!)]))



(module+ main
  ;; Start the server on port 5000
  (serve/servlet dispatcher
                 #:port 5000
                 #:servlet-regexp #rx""
                 #:listen-ip "0.0.0.0" ; any
                 #:launch-browser? #f)
  )

(module+ test
  (require rackunit
		   redex/reduction-semantics)

  (define test-step! (make-stepper (lambda (_) (term fishsticks))))

  (define test-program
	'(prog ()
			  ((∃ (x:q)
				  (∃ ()
					 (((((sym "dog1") =? (sym "cat") "u5")
						∧ ((sym "bear1") =? x:lion "u6") "c4")
					   ∧ ((sym "dog") =? (sym "cat") "u7") "c3")
					  ∧ ((sym "bear") =? (sym "lion") "u8") "c2") "f1") "f0")
			   (state () 0 ()))))

  (test-suite
   "Check that step! correctly-advances state"
   #:before (lambda () (initialize-all! test-program))

   (test-case "stepping works"
			  (check-equal?
				(begin
				  (test-step!)
				  (state-prog (first trace)))
				'fishsticks))

   )

  (test-suite
   "Check that step! correctly-advances state"
   #:before (lambda () (initialize-all! test-program))

   (test-case "stepping works"
			  (check-equal?
				(begin
				(test-step!)
				(state-prog (first trace)))
				'fishsticks))

   )


  )
