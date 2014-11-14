#lang racket
(require "data-networks.rkt")
(require lang/posn
         rsound
         2htdp/image)
(provide (all-defined-out))

; this cretes an empty scene
(define BKG (empty-scene XSIZE YSIZE))



(define (positionList world)
       (list
        ;positions of the pieces
        (piece-pos (first (ws-pl world)))
        (piece-pos (second (ws-pl world)))
        (piece-pos (third (ws-pl world)))
        ; positions of the text boxes
        (make-posn (posn-x (piece-pos (first (ws-pl INITIAL_WORLD)))) (* 0.05 YSIZE))
        (make-posn (posn-x (piece-pos (second (ws-pl INITIAL_WORLD)))) (* 0.05 YSIZE))
        (make-posn (posn-x (piece-pos (third (ws-pl INITIAL_WORLD)))) (* 0.05 YSIZE))
        ; positions of the buttons
        (make-posn (posn-x (piece-pos (first (ws-pl INITIAL_WORLD)))) (* YSIZE 0.95))
        (make-posn (posn-x (piece-pos (second (ws-pl INITIAL_WORLD)))) (* YSIZE 0.95))
        (make-posn (posn-x (piece-pos (third (ws-pl INITIAL_WORLD)))) (* YSIZE 0.95))
        ; positions of the rails
        (make-posn (posn-x (piece-pos (first (ws-pl INITIAL_WORLD)))) sY-midpt)
        (make-posn (posn-x (piece-pos (second (ws-pl INITIAL_WORLD)))) sY-midpt)
        (make-posn (posn-x (piece-pos (third (ws-pl INITIAL_WORLD)))) sY-midpt)
        ))
(define shapes
  (list
   ; sliders
   (rectangle (rect-w (first (ws-pl INITIAL_WORLD)))
              (rect-h (first (ws-pl INITIAL_WORLD))) "solid" "red")
   (rectangle (rect-w (second (ws-pl INITIAL_WORLD)))
              (rect-h (second (ws-pl INITIAL_WORLD))) "solid" "red")
   (rectangle (rect-w (third (ws-pl INITIAL_WORLD)))
              (rect-h (third (ws-pl INITIAL_WORLD))) "solid" "red")
   ; descriptions of what the sliders do
   (text "playhead speed" 12 "black")
   (text "distortion-cutoff" 12 "black")
   (text "distortion-scale" 12 "black")
   ; buttons on the bottom
   (rotate 270 (triangle 20 "solid" "green"))
   (rectangle 20 20 "solid" "green")
   (rectangle 20 20 "solid" "green")
   ; rails
   (rectangle 1 (- sY-bottom sY-top) "solid" "black")
   (rectangle 1 (- sY-bottom sY-top) "solid" "black")
   (rectangle 1 (- sY-bottom sY-top) "solid" "black")
   
   ))

(define (draw-world world)
  (begin (set-box! ws-box world)
         (place-images
   shapes
   (positionList world)
   (place-image/align (text (number->string (slider-value (second (ws-pl world))))
                            12 "black")
                      200 50 "center" "center"
   BKG
   ))))
;(draw-world INITIAL_WORLD)