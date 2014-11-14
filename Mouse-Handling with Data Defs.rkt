#lang racket
(require lang/posn
         rsound
         2htdp/image
         test-engine/racket-tests)
(require (file "data-networks.rkt"))
(provide mouse-handler)

;; determines which slider is being selected
;; which-slider number list-of-pieces -> maybe-piece
(define (which-slider posn pieces)
  (cond [(empty? pieces) false]
        [else (cond 
                [(in-rect? posn (first pieces)) (first pieces)]
                [else (which-slider posn (rest pieces))])]))

;; mouse-handler handles when: 
;; -a slider is dragged
;; world number number mouse-event -> world
(define (mouse-handler w x y mouse)
  (cond 
    ;; when the mouse clicks
    [(string=? mouse "button-down")
     (struct-copy ws w [focus (which-slider (make-posn x y) (ws-pl w))])]
    ;; deselects the piece
    [(string=? mouse "button-up")
     (struct-copy ws w [focus #f])]
    ;; dragging updates the values of the slider
    [(and (string=? mouse "drag") (not (false? (ws-focus w))))
     (local [(define updated-s (update-slider (ws-focus w) y))]
       (struct-copy ws w 
                    [pl (push-piece (ws-pl w) updated-s)]
                    [focus updated-s]))]
    [else w]))
         
;; update slider updates a given slider's
;; pos and value given the mouse y coord
;; slider number -> slider
(define (update-slider s mouse-y)
  (local
    [(define new-y (max sY-top (min sY-bottom mouse-y)))]     
    (struct-copy slider s 
                 [pos #:parent piece (make-posn (get-x s) new-y)]
                 [value (/ (- sY-bottom (- new-y sY-top) sY-top) (- sY-bottom sY-top))])))

;; push-piece replaces an updated version of a piece back into the list
;; list-of-pieces slider -> list-of-pieces
(define (push-piece pieces p)
  (map (lambda (i) 
         (cond [(piece-same? i p) p]
               [else i]))
       pieces))         

;(test)


