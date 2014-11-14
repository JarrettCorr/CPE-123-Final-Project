#lang racket
(require "data-networks.rkt")
(require lang/posn
         rsound
         2htdp/image)

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
        (make-posn (posn-x (piece-pos (first (ws-pl INITIAL_WORLD)))) (/ YSIZE 2))
        (make-posn (posn-x (piece-pos (second (ws-pl INITIAL_WORLD)))) (/ YSIZE 2))
        (make-posn (posn-x (piece-pos (third (ws-pl INITIAL_WORLD)))) (/ YSIZE 2))

        ))
(define shapes
  (list
   ; sliders
   (circle (rect-w (first (ws-pl INITIAL_WORLD))) "solid" "red")
   (circle (rect-w (second (ws-pl INITIAL_WORLD))) "solid" "red")
   (circle (rect-w (third (ws-pl INITIAL_WORLD))) "solid" "red")
   ; descriptions of what the sliders do
   (text "Volume" 12 "black")
   (text "Sine Wave Modulation" 12 "black")
   (text "Frequency" 12 "black")
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
  (place-images
   shapes
   (positionList world)
   BKG
   ))
(draw-world INITIAL_WORLD)