#lang racket
(require "data-networks.rkt")
(require lang/posn
         rsound
         2htdp/image)
(provide (all-defined-out))

; this cretes an empty scene
(define BKG (place-image (bitmap/file "backgroundImage.jpeg") (/ XSIZE 2) (/ YSIZE 2) (empty-scene XSIZE YSIZE)))
; these are strings that define the color of each element type
(define SLIDERCOLOR "SlateGray")
(define TEXTCOLOR "Azure")
(define RAILCOLOR "Light Steel Blue")
(define SLIDERIMAGE (bitmap/file "slider2.jpg"))


(define (positionList world)
       (list
        ;positions of the pieces
        (piece-pos (first (ws-pl world)))
        (piece-pos (second (ws-pl world)))
        (piece-pos (third (ws-pl world)))
        ; positions of the text boxes
        (make-posn (posn-x (piece-pos (first (ws-pl INITIAL_WORLD)))) (* 0.025 YSIZE))
        (make-posn (posn-x (piece-pos (second (ws-pl INITIAL_WORLD)))) (* 0.025 YSIZE))
        (make-posn (posn-x (piece-pos (third (ws-pl INITIAL_WORLD)))) (* 0.025 YSIZE))
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
   (overlay SLIDERIMAGE 
            (rectangle (rect-w (first (ws-pl INITIAL_WORLD)))
              (rect-h (first (ws-pl INITIAL_WORLD))) "solid" SLIDERCOLOR))
   (overlay SLIDERIMAGE 
            (rectangle (rect-w (second (ws-pl INITIAL_WORLD)))
              (rect-h (second (ws-pl INITIAL_WORLD))) "solid" SLIDERCOLOR))
   (overlay SLIDERIMAGE 
            (rectangle (rect-w (third (ws-pl INITIAL_WORLD)))
              (rect-h (third (ws-pl INITIAL_WORLD))) "solid" SLIDERCOLOR))
   ; descriptions of what the sliders do
   (text "playhead speed" 12 TEXTCOLOR)
   (text "distortion-cutoff" 12 TEXTCOLOR)
   (text "distortion-scale" 12 TEXTCOLOR)
   ; buttons on the bottom
   (rotate 270 (triangle 20 "solid" "Orange"))
   (rectangle 20 20 "solid" "Orange")
   (rectangle 20 20 "solid" "Orange")
   ; rails
   (rectangle 1 (- sY-bottom sY-top) "solid" RAILCOLOR)
   (rectangle 1 (- sY-bottom sY-top) "solid" RAILCOLOR)
   (rectangle 1 (- sY-bottom sY-top) "solid" RAILCOLOR)
   
   ))

(define (draw-world world)
  (begin (set-box! ws-box world)
         (place-images
   shapes
   (positionList world)
   (place-image/align (text (number->string (slider-value (second (ws-pl world))))
                            12 "white")
                      200 30 "center" "center"
   BKG
   ))))
;(draw-world INITIAL_WORLD)