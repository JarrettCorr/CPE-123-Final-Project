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
(define XSIZE 100) ;; scene's max x value
(define YSIZE 100) ;; scene's max y value

;; slider constants: 
;; 0 > sY-top > sY-bottom > YSIZE
(define sY-top 10)    ;; slider's highest pos 
(define sY-bottom 90) ;; slider's lowest pos

(define sH 0) ;; sliders' height
(define sW 0) ;; sliders' width

;; Initial world state setup:
(define INITIAL_WORLD 
  (ws (list
       (slider (make-posn 0 0) sH sW 0)
       (slider (make-posn 0 0) sH sW 0)
       (slider (make-posn 0 0) sH sW 0))
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


;; determines what slider is being selected
;; which-slider number list-of-pieces -> maybe-piece
(define (which-slider posn pieces)
    (cond [(empty? pieces) false]
          [else (cond 
                  [(in-rect? posn (first pieces)) piece-pos (first piece)]
                  [else (which-slider posn (rest pieces))])]))

;; this condition contains the things the mouse controls
;; world mouse-posn mouse-event -> world
(define (mouse-things ws posn mouse)
  (cond 
    ;; when the mouse clicks
    [(string=? mouse "button-down")
         (cond
           [(false? (which-slider posn ws)) INITIAL_WORLD]
           [else (make-ws
                  (make-updated-pieces
                   (ws-pieces ws)
                   (which-slider posn ws)
                   y)
                  (which-slider posn ws))])]
    ;; deselects the piece
    [(string=? "button-up")
     (make-ws (ws-pieces ws) #f)]
    ;; dragging updates the values of the slider
    [(string=? mouse "drag")
     (make ws
           (cons 
            (update-piece-posn posn (which-slider mouse ws))
            (rest (ws-pl ws)))
           (ws-focus ws))]))
         
;; makes a new slider to update to the world
;; slider (mouse y-posn) -> slider
(define (update-slider-posn s mouse-y)
  (local
    [(define range (- sY-top sY-bottom))]
    [(define max-min (max sY-top (min (rect-h s) mouse-y)))]
    [(define new-slider-posn (- 1 (/ (- min-max ...) range)))]
     
  (make-slider (make-posn (posn-x piece-pos s) (mouse-y))
               (rect-w s)
               (rect-h s)
               new-slider-posn)))
         
(define (update-piece-posn p mouse-y)
  (make-piece (update-slider-posn (p (make-posn (pos-x p) (- y (pos-y p)))))))

;; checks to update the focused slider to be at the mouse posn
;; slider "focused-piece" "y-mouse-posn" -> pieces
;; a "focused-piece" is the piece that the mouse has currently selected
(define (make-updated-pieces pieces focus y)
  (cond 
    [(empty? pieces) "no piece is selected"]
    [else 
     (cond
       [(equal? (piece-posn (first pieces) focus))
        (cons (make-updated-pieces (first pieces) y)
              (rest pieces))]
       [else (cons (first pieces)
                   (make-updated-pieces (rest pieces) focus y))])]))

          

(test)


