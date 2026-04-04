#lang racket

(require (only-in "./core-red.rkt"
                  core-red)
         (only-in "./calls-red.rkt"
                  relcall-red)
         (only-in "./delay-red.rkt"
                  delay-red)
         (only-in "./disj-fused-red.rkt"
                  disj-late-red)
         (only-in "./disj-seq-red.rkt"
                  disj-early-red)
         (only-in "./search-dfs-fused-red.rkt"
                  search-dfs-late-red)
         (only-in "./search-dfs-seq-red.rkt"
                  search-dfs-early-red)
         (only-in "./search-flip-fused-red.rkt"
                  search-flip-late-red)
         (only-in "./search-flip-seq-red.rkt"
                  search-flip-early-red)
         (only-in "./search-base-seq-red.rkt"
                  search-early-red)
         (only-in "./search-base-fused-red.rkt"
                  search-late-red)
         (only-in "./search-base-seq-calls-red.rkt"
                  search-early-relcall-red)
         (only-in "./search-base-fused-calls-red.rkt"
                  search-late-relcall-red)
         (only-in "./search-dfs-seq-calls-red.rkt"
                  search-dfs-early-relcall-red)
         (only-in "./search-dfs-fused-calls-red.rkt"
                  search-dfs-late-relcall-red)
         (only-in "./search-flip-seq-calls-red.rkt"
                  search-flip-early-relcall-red)
         (only-in "./search-flip-fused-calls-red.rkt"
                  search-flip-late-relcall-red)
         (only-in "./rail-seq-red.rkt"
                  rail-early-red)
         (only-in "./rail-fused-red.rkt"
                  rail-late-red)
         (only-in "./rail-seq-calls-red.rkt"
                  rail-early-relcall-red)
         (only-in "./rail-fused-calls-red.rkt"
                  rail-late-relcall-red))

(provide
 ;; Surfaced call-bearing reduction relations.
 relcall-red
 search-early-relcall-red
 search-late-relcall-red
 search-dfs-early-relcall-red
 search-dfs-late-relcall-red
 search-flip-early-relcall-red
 search-flip-late-relcall-red
 rail-early-relcall-red
 rail-late-relcall-red

 ;; Internal lattice reduction relations kept for staging, comparison, and tests.
 core-red
 delay-red
 disj-early-red
 disj-late-red
 search-dfs-early-red
 search-dfs-late-red
 search-flip-early-red
 search-flip-late-red
 search-early-red
 search-late-red
 rail-early-red
 rail-late-red)
