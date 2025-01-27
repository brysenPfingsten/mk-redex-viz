#lang racket
(require spin)

(get "/api/get" (λ () "Hello"))

(run)