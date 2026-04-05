#lang racket

(require (only-in "./core-red.rkt"
                  core-red)
         (only-in "./relcall-red.rkt"
                  relcall-red)
         (only-in "./delay-red.rkt"
                  delay-red)
         (only-in "./disj-late-red.rkt"
                  disj-late-red)
         (only-in "./disj-early-red.rkt"
                  disj-early-red)
         (only-in "./search-dfs-late-red.rkt"
                  search-dfs-late-red)
         (only-in "./search-dfs-early-red.rkt"
                  search-dfs-early-red)
         (only-in "./search-flip-late-red.rkt"
                  search-flip-late-red)
         (only-in "./search-flip-early-red.rkt"
                  search-flip-early-red)
         (only-in "./search-early-red.rkt"
                  search-early-red)
         (only-in "./search-late-red.rkt"
                  search-late-red)
         (only-in "./search-early-relcall-red.rkt"
                  search-early-relcall-red)
         (only-in "./search-late-relcall-red.rkt"
                  search-late-relcall-red)
         (only-in "./search-dfs-early-relcall-red.rkt"
                  search-dfs-early-relcall-red)
         (only-in "./search-dfs-late-relcall-red.rkt"
                  search-dfs-late-relcall-red)
         (only-in "./search-flip-early-relcall-red.rkt"
                  search-flip-early-relcall-red)
         (only-in "./search-flip-late-relcall-red.rkt"
                  search-flip-late-relcall-red)
         (only-in "./rail-early-red.rkt"
                  rail-early-red)
         (only-in "./rail-late-red.rkt"
                  rail-late-red)
         (only-in "./rail-early-relcall-red.rkt"
                  rail-early-relcall-red)
         (only-in "./rail-late-relcall-red.rkt"
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
