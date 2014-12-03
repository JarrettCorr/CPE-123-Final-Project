#lang racket
(require (file "data-networks.rkt"))
(require lang/posn
         rsound
         2htdp/image
         test-engine/racket-tests)
(provide (all-defined-out))

; these are constants for certain image attributes
(define buttonColor (color 54 113 203))
(define TEXTCOLOR "Azure")
(define FONTSIZE 12)
(define RAILCOLOR "Light Steel Blue")
(define INVISIBLE_IMG (rectangle 1 1 "solid" (color 255 255 255 0)))


;; Constant that contains all non-moving, non-changing images
;; drawn onto the main background (BKG) that defines the size of the scene
(define STILLBKG  
   (local ; Creates functions to build the image for rail in which
          ; the slider will go over as well as the position it needs
     [(define (rail-image s) 
        (above 
         (overlay (text (piece-id s) FONTSIZE TEXTCOLOR)
                  (rectangle 10 (+ (* FONTSIZE 2) sH)
                             "solid" (color 255 255 255 0)))
         (rectangle 1 (- sY-bottom sY-top) "solid" RAILCOLOR)))
      (define (rail-pos s) 
        (make-posn (get-x s) (- sY-midpt (/ sH 2) FONTSIZE)))] 
     ;places all rails for any sliders in the world
     (place-images/align
      ;making image lists and posn lists for place-image function for rails
      (map (lambda (i) 
             (cond [(slider? i) (rail-image i)]
                   [else INVISIBLE_IMG]))
           (ws-pl INITIAL_WORLD))
      (map (lambda (i) 
             (cond [(slider? i) (rail-pos i)]
                   [else (make-posn XSIZE YSIZE)]))
           (ws-pl INITIAL_WORLD))
      "center" "center"
      
      ;places specific images into still background
      BKG)))

;; the draw-button function handles providing different images for buttons with different
;; id fields
;; button -> image
(define (draw-button b) 
  (local [(define H (rect-h b)) (define W (rect-w b))
          (define font (round (/ H 1.5)))
          (define id (piece-id b))]
    (cond [(string=? id "See Signals")
           (place-image/align 
            (text "Draw" font "black")
            (/ W 2) (/ H 2) "center" "center"
            (rectangle W H "solid" buttonColor))]
          [(string=? id "Sound-Menu")
           (place-image/align 
            (text "Sounds" font "black")
            (/ W 2) (/ H 2) "center" "center"
            (rectangle W H "solid" buttonColor))]
          [(and (>= (string-length id) 11) 
                (string=? (substring id 0 11) "Sound-Menu-"))         
           (place-image/align 
            (text (substring id 11 12) font "black")
            (/ W 2) (/ H 2) "center" "center"
            (rectangle W H "solid" buttonColor))]
          [(and (>= (string-length id) 5) 
                (string=? (substring id 0 6) "Reset-"))         
           (place-image/align 
            (text "R" font "black")
            (/ W 2) (/ H 2) "center" "center"
            (rectangle W H "solid" buttonColor))]
          [else (rectangle W H "solid" "red")])))

;; the draw-piece function handles how to draw all sub-types of a piece and returns the 
;; necessary image
;; piece -> image
(define (draw-piece p) 
  (cond [(slider? p) SLIDERIMAGE]
        [(button? p) (draw-button p)]
        [else INVISIBLE_IMG]))
;; the draw-world function is called by big-bang and draws all
;; moving objects/changing objects over the still background (STILBKG)
;; and also boxes the world for signal-play
;; world -> image 
;;       -> ws-box 
(define (draw-world world)
  (begin (set-box! ws-box world)
         (local [(define piece-list (ws-pl world))]
           (place-images
            (map draw-piece piece-list)
            (map piece-pos piece-list)
            STILLBKG
   ))))
;STILLBKG
;(draw-world INITIAL_WORLD)

;;(test)