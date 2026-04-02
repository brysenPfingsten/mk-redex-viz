#lang racket

(require "model-registry.rkt")

(provide surfaced-model-ids
         internal-smoke-model-ids
         surfaced-model-specs
         internal-smoke-model-specs)

(define surfaced-model-ids
  '("mk-l3-dfs-lazy"
    "mk-l3-flip-lazy"
    "mk-l4-rail-lazy"
    "mk-l3-dfs-eager"
    "mk-l3-flip-eager"
    "mk-l4-rail-eager"))

(define internal-smoke-model-ids
  '("mk-l0-core"
    "mk-l1-call-lazy"
    "mk-l1-call-eager"
    "mk-l2-disj-left"))

(define (ids->specs ids)
  (for/list ([mid (in-list ids)])
	(cond
	  ((lookup-model-spec mid))
	  (else (error 'ids->specs (format "unknown model id in surface policy: ~a" mid))))))

(define surfaced-model-specs (ids->specs surfaced-model-ids))
(define internal-smoke-model-specs (ids->specs internal-smoke-model-ids))
