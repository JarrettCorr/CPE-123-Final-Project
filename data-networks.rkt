#lang racket
(require lang/posn
         rsound
         2htdp/image
         test-engine/racket-tests)
(provide struct
         struct-copy)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a piece is (piece posn)
;; -pos: a posn structure 
(struct piece (pos) #:prefab)

;; a rect is (rect posn number number)
;; -w: width of the rectangle
;; -h: height of the rectangle
(struct rect piece (w h) #:prefab)

;; an sprite is (sprite posn image)
;; -image: a picture of some kind (ex (file "aaa.png"))
(struct sprite piece (image) #:prefab)

;; a slider is (slider posn number number number)
;; -value: number from 0.0 to 1.0 that represents the
;;;        relative height, 1.0 = top 0.0 = bottom
(struct slider rect (value) #:prefab)

;; list-of-pieces is one of:
;; -empty
;; -(cons piece list-of-pieces)

;; a world is (ws list-of-pieces maybe-piece)
;; pl: a list-of-pieces contianed in the world
;; focus: the piece currently being focused upon
;;        -> either piece? or false (interp. as no piece being focused)
(struct ws (pl focus))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world constants: 
(define XSIZE 400) ;; scene's max x value
(define YSIZE 300) ;; scene's max y value

;; slider constants: 
;; 0 > sY-top > sY-bottom > YSIZE
(define sY-top (* .1 YSIZE))    ;; slider's highest pos 
(define sY-bottom (* .9 YSIZE)) ;; slider's lowest pos
(define sY-midpt (/ (+ sY-bottom sY-top) 2)) ;;midpoint of the sliders

(define sH 10) ;; sliders' height
(define sW 10) ;; sliders' width

;; Initial world state setup:
(define INITIAL_WORLD 
  (ws (list
       (slider (make-posn 100 sY-midpt) sH sW 0)
       (slider (make-posn 200 sY-midpt) sH sW 0)
       (slider (make-posn 300 sY-midpt) sH sW 0))
      #f))

;; world state box is used for signals
(define ws-box (box INITIAL_WORLD))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rect-x rect) (posn-x (piece-pos rect)))
(define (rect-y rect) (posn-y (piece-pos rect)))

; checks if the position (make-posn x y) is in the rectangle
; integer integer rect -> boolean
(check-expect (in-rect? 3 4 (rect (make-posn 2 3) 2 3)) #t)

(define (in-rect? posn rect) 
  (local [(define rX (rect-x rect)) (define rW (/ (rect-w rect) 2))
          (define rY (rect-y rect)) (define rH (/ (rect-h rect) 2))]                         
    (and (<= (- rX rW) (posn-x posn) (+ rX rW)) (<= (- rY rH) (posn-y posn) (+ rY rH)))))



(test)


