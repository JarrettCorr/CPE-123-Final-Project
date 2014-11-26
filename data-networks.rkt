#lang racket
(require lang/posn
         rsound
         2htdp/image
         2htdp/universe
         test-engine/racket-tests)
(provide struct
         struct-copy)
(provide (all-defined-out))

#|By:|#
(define Team "awesome!")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; a piece is (piece posn)
;; -pos: a posn structure 
;; -id: a unique string identifying it from other pieces
(struct piece (id pos) #:prefab)

;; a rect is (rect posn number number)
;; -w: width of the rectangle
;; -h: height of the rectangle
(struct rect piece (w h) #:prefab)

;; a circle is (circ posn number)
;; -r: radius of the circle
(struct circ piece (r) #:prefab)

;; image: a picture of some kind (ex (file "aaa.png"))
;; list-of-images is one of: 
;; -empty
;; -(cons image list-of-images)

;; an sprite is (sprite posn list-of-images number)
;; -images: list of frames to animate the sprite
;; -frame: which image to get from images
(struct sprite piece (images frame) #:prefab)

;; a slider is (slider posn number number number)
;; -value: number from 0.0 to 1.0 that represents the
;;         relative height, 1.0 = top 0.0 = bottom
;;         (remember y coord is inverted)
(struct slider rect (value) #:prefab)

;; a button is (button posn number number procedure bool)
;; - function: the function that is run on the button being clicked (released)
;;             it is a procedure (lambda (world) ...) which takes in a world struct
(struct button rect (function) #:prefab)

;; list-of-pieces is one of:
;; -empty
;; -(cons piece list-of-pieces)

;; maybe-piece is one of:
;; -false
;; -piece

;; a world is (ws list-of-pieces maybe-piece)
;; -pl: a list-of-pieces contianed in the world
;; -focus: where the piece currently being focused upon
;;         and false is interpreted as no piece being focused
(struct ws (pl focus) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world constants: 
(define BKG (bitmap/file "backgroundImage Large.jpeg"))
(define XSIZE (image-width BKG)) ;; scene's max x value
(define YSIZE (image-height BKG)) ;; scene's max y value
(define song (rs-read "Sounds\\DRR.wav")) ;song import
(define songlen (rs-frames song))         ;song length

;; slider constants: 
;; 0 > sY-top > sY-bottom > YSIZE
(define sY-top (* .1 YSIZE))    ;; slider's highest pos 
(define sY-bottom (* .9 YSIZE)) ;; slider's lowest pos
(define sY-midpt (/ (+ sY-bottom sY-top) 2)) ;;midpoint of the sliders

(define SLIDERIMAGE (bitmap/file "slider2.jpg"))

(define sH (image-height SLIDERIMAGE)) ;; sliders' height
(define sW (image-width SLIDERIMAGE)) ;; sliders' width

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; distance is the distance between two posn in pixels
;; posn posn -> number
(define (distance p1 p2)
  (sqrt (+ (expt (- (posn-x p1) (posn-x p2)) 2)
           (expt (- (posn-y p1) (posn-y p2)) 2))))

(check-expect (distance (make-posn 0 4) (make-posn 0 6)) 2)

;; Piece helper functions

(define (get-x p) (posn-x (piece-pos p))) ;shorter call to get x pos
(define (get-y p) (posn-y (piece-pos p))) ;shorter call to get y pos

;; piece-same? compares two pieces' unique string id fields
;; and returns true if they are the same and false if not
;; piece piece -> boolean
(define (piece-same? p1 p2) 
  (string=? (piece-id p1) (piece-id p2)))

;; add some pieces to a pieces-list
;; list-of-pieces list-of-pieces -> list-of-pieces
(define (add-pieces pieces pieces-to-add)
  (foldl cons pieces pieces-to-add))

;; push-piece replaces an updated version of a piece back into the list
;; list-of-pieces piece -> list-of-pieces
(define (push-piece pieces p)
  (map (lambda (i) 
         (cond [(piece-same? i p) p]
               [else i]))
       pieces))  

;; Rectangle helper functions:

;; in-rect? checks if the position (make-posn x y) is in the rectangle
;; posn rect -> boolean
(define (in-rect? posn rect) 
  (local [(define rX (get-x rect)) (define rW (/ (rect-w rect) 2))
          (define rY (get-y rect)) (define rH (/ (rect-h rect) 2))]                         
    (and (<= (- rX rW) (posn-x posn) (+ rX rW)) (<= (- rY rH) (posn-y posn) (+ rY rH)))))

(check-expect (in-rect? (make-posn 3 4) (rect "one" (make-posn 2 3) 2 3)) #t)
(check-expect (in-rect? (make-posn 10 10) (rect "one" (make-posn 2 3) 2 3)) #f)

;; Circle helper functions:

; in-circ? checks if the position (make-posn x y) is in the circle
; posn circ -> boolean
(define (in-circ? posn circle)                        
  (< (distance posn (piece-pos circle)) (circ-r circle)))

(check-expect (in-circ? (make-posn 3 4) (circ "one" (make-posn 2 2) 3)) #t)
(check-expect (in-circ? (make-posn 10 10) (circ "one" (make-posn 2 2) 3)) #f)

;; Button helper functions:

;; produces a procedure that will take in a world and reset the slider
;; in the world's piece list with matching id to the state of the 
;; given slider
;; slider -> procedure 
(define (reset-slider s) 
  (lambda (w) 
    (struct-copy ws w
                 [pl (push-piece (ws-pl w) s)])))

;; makes a button which resets a specific slider to the given slider's state
;; number number slider -> button
(define (reset-button x y s) 
  (button (string-append "Reset-" (piece-id s)) 
          (make-posn x y) 20 20 (reset-slider s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Big-bang 2: Visualization of signals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for drawing the signals played
(define saved-sig-size 1575) ; 1575 = frames in 1/28th of a sec
;; saved-sig is a vector that holds saved-sig-size signals
;; from 0 to (- to saved-sig-size 1) 
;; and the position saved-sig-size in the vector holds the 
;; most recent signal's pos in the vector
(define saved-sig (make-vector (add1 saved-sig-size) 0))

#|;;;;;;;;;;;;;;;;;|#
#|Drawing Functions|#
#|;;;;;;;;;;;;;;;;;|#

;; signal-pic calls the recursive function that draws out all the signals in the
;; saved-sig vector given the starting pos and how many frames to skip by each
;; iteration. 
;; If skip is 2 every other frame is drawn producing a half as wide image
(define (signal-pic) 
  (signal-pic-recur (vector-ref saved-sig saved-sig-size) 0 2))

;; signal-pic-recur takes in a starting position for the saved-sig vector
;; counts up by skip frames until it has almost wrapped back to its starting
;; position traversing saved-sig-size worth of frames
;; takes in: 
;; start: a number from 0 to (- saved-sig-size 1)
;; x: should always be zero
;; skip: any positive integer
;; -> image
(define (signal-pic-recur start x skip)
  (local [(define s (vector-ref saved-sig (modulo (- start x) saved-sig-size)))
          (define bar (inexact->exact (round (* (abs s) 100))))]
    (place-image 
     (rectangle 1 bar "solid" (color bar 0 0))
     (/ x skip)
     (cond [(> s 0) (- 100 bar)]
           [else 100])
     (cond [(>= (+ x 4) saved-sig-size) (empty-scene (/ saved-sig-size skip) 210)]
           [else (signal-pic-recur start (+ x skip) skip)]))))

(define (draw-signals w)
  (signal-pic))

;; add a button to the world list
;; pieces-list piece -> list-of-pieces
;(define (add-piece pieces piece)
;  (lambda (w) 
;    (struct-copy ws w
;                 [pl (add-piece (ws-pl w) piece)]))))

;; function that creates a big-bang to visualize signals
(define (signal-view) 
   (big-bang #t
            [on-tick (lambda (w) w)]            
            [to-draw draw-signals]
            [name "Signal View"]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initial Setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Default Sliders:

(define SPEED_SLIDER 
  (slider "Speedcontrol" (make-posn 250 (* .75 sY-bottom)) sW sH 0.25))
(define DISTORT_SLIDER-CUT 
  (slider "Distortion Cut-off" (make-posn 450 sY-top) sW sH 1))
(define DISTORT_SLIDER-SCALE 
  (slider "Distortion Feather" (make-posn 650 sY-bottom) sW sH 0))
(define DELAY_SLIDER 
  (slider "Delay" (make-posn 850 sY-bottom) sW sH 0))


(define INITIAL_WORLD 
  (ws (list
       ;; slider for playhead speed
       SPEED_SLIDER
       ;; slider for distortion cutoff
       DISTORT_SLIDER-CUT 
       ;; slider for the distortion form
       DISTORT_SLIDER-SCALE
       ;; slider for the delay
       DELAY_SLIDER
       ;; reset button for playhead speed
       (reset-button (get-x SPEED_SLIDER) (* .95 YSIZE) SPEED_SLIDER)
       ;; reset button for distortion cutoff
       (reset-button (get-x DISTORT_SLIDER-CUT) (* .95 YSIZE) DISTORT_SLIDER-CUT)
       ;; reset button for distortion form
       (reset-button (get-x DISTORT_SLIDER-SCALE) (* .95 YSIZE) DISTORT_SLIDER-SCALE)
       ;; reset button for the delay
       (reset-button (get-x DELAY_SLIDER) (* .95 YSIZE) DELAY_SLIDER)
       ;; button that makes the "drawing-wave" world
       (button "See Signals" (make-posn 50 50) 60 20 
               (lambda (w) (begin (thread signal-view) w)))
       ;; the drop down menu button
       #;(button "Choose your sound" (make-posn (- (image-width BKG) 50) 50) 60 20 draw-menu))
      #f))

;; world state box is used for signals
(define ws-box (box INITIAL_WORLD))