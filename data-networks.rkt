#lang racket
(require lang/posn
         rsound
         2htdp/image
         test-engine/racket-tests)
(provide struct
         struct-copy)
(provide (all-defined-out))

#|By:|#
(define Team "awesome!")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;; an sprite is (sprite posn image)
;; -image: a picture of some kind (ex (file "aaa.png"))
(struct sprite piece (image) #:prefab)

;; a slider is (slider posn number number number)
;; -value: number from 0.0 to 1.0 that represents the
;;         relative height, 1.0 = top 0.0 = bottom
;;         (remember y coord is inverted)
(struct slider rect (value) #:prefab)

;; list-of-pieces is one of:
;; -empty
;; -(cons piece list-of-pieces)

;; a world is (ws list-of-pieces maybe-piece)
;; pl: a list-of-pieces contianed in the world
;; focus: the piece currently being focused upon
;;        -> either piece? or false (interp. as no piece being focused)
(struct ws (pl focus) #:prefab)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world constants: 
(define XSIZE 400) ;; scene's max x value
(define YSIZE 600) ;; scene's max y value

;; slider constants: 
;; 0 > sY-top > sY-bottom > YSIZE
(define sY-top (* .1 YSIZE))    ;; slider's highest pos 
(define sY-bottom (* .9 YSIZE)) ;; slider's lowest pos
(define sY-midpt (/ (+ sY-bottom sY-top) 2)) ;;midpoint of the sliders

(define sH 20) ;; sliders' height
(define sW 20) ;; sliders' width

;; Initial world state setup:
(define INITIAL_WORLD 
  (ws (list
       (slider "one" (make-posn 100 (* .75 sY-bottom)) sH sW 0.25)
       (slider "two" (make-posn 200 sY-top) sH sW 1)
       (slider "three" (make-posn 300 sY-bottom) sH sW 0))
      #f))

;; world state box is used for signals
(define ws-box (box INITIAL_WORLD))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Piece helper functions

(define (get-x p) (posn-x (piece-pos p))) ;shorter call to get x pos
(define (get-y p) (posn-y (piece-pos p))) ;shorter call to get y pos

;; piece-same? compares two pieces' unique string id fields
;; and returns true if they are the same and false if not
;; piece piece -> boolean
(define (piece-same? p1 p2) 
  (string=? (piece-id p1) (piece-id p2)))

;; distance is the distance between two posn in pixels
;; posn posn -> number
(define (distance p1 p2)
  (sqrt (+ (expt (- (posn-x p1) (posn-x p2)) 2)
           (expt (- (posn-y p1) (posn-y p2)) 2))))

(check-expect (distance (make-posn 0 4) (make-posn 0 6)) 2)

;; Rectangle helper functions:

;; in-rect? checks if the position (make-posn x y) is in the rectangle
;; integer integer rect -> boolean
(define (in-rect? posn rect) 
  (local [(define rX (get-x rect)) (define rW (/ (rect-w rect) 2))
          (define rY (get-y rect)) (define rH (/ (rect-h rect) 2))]                         
    (and (<= (- rX rW) (posn-x posn) (+ rX rW)) (<= (- rY rH) (posn-y posn) (+ rY rH)))))

(check-expect (in-rect? (make-posn 3 4) (rect "one" (make-posn 2 3) 2 3)) #t)
(check-expect (in-rect? (make-posn 10 10) (rect "one" (make-posn 2 3) 2 3)) #f)

;; Circle helper functions:

; in-circ? checks if the position (make-posn x y) is in the circle
; integer integer circ -> boolean
(define (in-circ? posn circle)                        
  (< (distance posn (piece-pos circle)) (circ-r circle)))

(check-expect (in-circ? (make-posn 3 4) (circ "one" (make-posn 2 2) 3)) #t)
(check-expect (in-circ? (make-posn 10 10) (circ "one" (make-posn 2 2) 3)) #f)


;(test)


