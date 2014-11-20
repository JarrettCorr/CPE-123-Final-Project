#lang racket
(require lang/posn
         rsound
         2htdp/image
         test-engine/racket-tests)
(require (file "data-networks.rkt"))
(provide mouse-handler)

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

       





