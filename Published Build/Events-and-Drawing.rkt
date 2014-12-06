#lang racket
(require lang/posn
         rsound
         2htdp/image)
(require (file "Structs-Help-Setup.rkt"))
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big-bang 1: Controls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|;;;;;;;;;;;;;;;;;|#
#|Drawing Functions|#
#|;;;;;;;;;;;;;;;;;|#

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
      (place-image/align LOGO-PIC 100 100 "center" "center" BKG))))

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
            (text 
             (song-name 
              (list-ref SONGS 
                        (string->number (substring id 11 12)))) 
             font "black")
            (/ W 2) (/ H 2) "center" "center"
            (rectangle W H "solid" buttonColor))]
          
          [(and (>= (string-length id) 6) 
                (string=? (substring id 0 6) "Reset-"))         
           (place-image/align 
            (text "R" font "black")
            (/ W 2) (/ H 2) "center" "center"
            (rectangle W H "solid" buttonColor))]
          
          [(string=? id "Flang")
           (place-image/align 
            (text id font "black")
            (/ W 2) (/ H 2) "center" "center"
            (rectangle W H "solid" buttonColor))]
          
          [else (rectangle W H "solid" "red")])))

;; the draw-piece function handles how to draw all sub-types of a piece and returns the 
;; necessary image
;; piece -> image
(define (draw-piece p) 
  (cond [(slider? p) SLIDERIMAGE]
        [(button? p) (frame (draw-button p))]
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

#|;;;;;;;;;;;;;;;;;;;;;|#
#|Mouse-event Functions|#
#|;;;;;;;;;;;;;;;;;;;;;|#

;; mouse-handler handles when: 
;; -a slider is dragged
;; -a button is pressed
;; world number number mouse-event -> world
(define (mouse-handler w x y mouse)
  (local [(define fs (ws-focus w))]
    (cond 
    ;; Mouse-click determines if a piece has been clicked upon and 
    ;; updates the world's focus field to that piece
    [(string=? mouse "button-down")
     (struct-copy ws w [focus (which-piece (make-posn x y) (ws-pl w))])]
    ;; Mouse-down:
    ;; button? - run the button's function to alter the world
    ;; else    - returns the world with no focus (focus = #f)
    [(string=? mouse "button-up")
     (cond [(button? fs) ((button-function fs) w)]
           [else (struct-copy ws w [focus #f])])]
    ;; Dragging:
    ;; slider? - updates the slider's value and position in the world
    ;; else    - does nothing and returns the world
    [(and (string=? mouse "drag") (slider? fs))
     (local [(define updated-s (update-slider fs y))]
       (struct-copy ws w 
                    [pl (push-piece (ws-pl w) updated-s)]
                    [focus updated-s]))]
    [else w])))

;; determines which piece the mouse is on
;; posn list-of-pieces -> maybe-piece
(define (which-piece posn pieces)   
  (cond [(empty? pieces) #f]
        [else 
         (local [(define piece (first pieces))] 
           (cond 
             [(and (rect? piece) (in-rect? posn piece)) piece]
             [(and (circ? piece) (in-circ? posn piece)) piece]
             [else (which-piece posn (rest pieces))]))]))
         
;; update slider updates a given slider's
;; pos and value given the mouse y coord
;; slider number -> slider
(define (update-slider s mouse-y)
  (local
    [(define new-y (max sY-top (min sY-bottom mouse-y)))]     
    (struct-copy slider s 
                 [pos #:parent piece (make-posn (get-x s) new-y)]
                 [value (/ (- sY-bottom (- new-y sY-top) sY-top) (- sY-bottom sY-top))])))

