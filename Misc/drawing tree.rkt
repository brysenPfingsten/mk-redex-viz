#lang racket

(require racket/gui
         racket/draw)

;; Tree data structure
(define-struct tree (value children))

;; Sample tree for testing
(define sample-tree
  (make-tree "start"
             (list (make-tree "step 1"
                              (list (make-tree "step 1.1" empty)
                                    (make-tree "step 1.2" empty)))
                   (make-tree "step 2" empty)
                   (make-tree "step 3"
                              (list (make-tree "step 3.1" empty))))))


;; Draw a tree node and its children recursively
(define (draw-tree dc tree x y x-gap y-gap)
  (define node-radius 20)
  ;; Draw the current node
  (define node-center-x x)
  (define node-center-y y)
  (send dc draw-ellipse (- node-center-x node-radius)
              (- node-center-y node-radius)
              (* 2 node-radius)
              (* 2 node-radius))
  (send dc draw-text (tree-value tree) (- x node-radius) (- y 5))
  ;; Draw children
  (let* ([children (tree-children tree)]
         [child-count (length children)]
         [child-x-start (- x (* (/ x-gap 2) (sub1 child-count)))])
    (for ([child children]
          [i (in-naturals)])
      (define child-x (+ child-x-start (* i x-gap)))
      (define child-y (+ y y-gap))
      ;; Draw line to child
      (send dc draw-line node-center-x node-center-y child-x child-y)
      ;; Recursively draw child tree
      (draw-tree dc child child-x child-y x-gap y-gap))))

;; Create a canvas for the tree
(define frame (new frame% [label "Reduction Tree"] [width 800] [height 600]))
(define canvas
  (new canvas% [parent frame]
       [paint-callback
        (lambda (canvas dc)
          ;; Initial tree drawing setup
          (define root-x (/ (send canvas get-width) 2))
          (define root-y 50)
          (define x-gap 120)
          (define y-gap 80)
          ;; Draw the sample tree
          (draw-tree dc sample-tree root-x root-y x-gap y-gap))]))

;; Show the GUI
(send frame show #t)
