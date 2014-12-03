#lang racket
(require lang/posn
         rsound
         2htdp/image
         2htdp/universe
         test-engine/racket-tests)
(require (file "data-networks.rkt")
         (file "Mouse-Handling with Data Defs.rkt")
         (file "drawing the world.rkt"))

;; distortion returns a network that scales amplitudes
;; above the cut-off by the above-scale value
;; number number -> network
(define distortion
  (network (cut-off above-scale i) 
    [out = (cond [(> i cut-off) (+ cut-off (* (- i cut-off) above-scale))]
          [(< i (* -1 cut-off)) (- cut-off (* (+ i cut-off) above-scale))]
          [else i])]))

;; (these unbox functions are temporary as the pos of the slider must be known
;; searching for them will be added later.)
;; unboxes first slider value for speeed of playhead
(define (get-speed) (max .5 (* 4 (slider-value (first (filter slider? (ws-pl (unbox ws-box))))))))
;; unboxes second and third slider values for distortion control
(define (get-cutoff) (slider-value (second (filter slider? (ws-pl (unbox ws-box))))))
(define (get-scale) (slider-value (third (filter slider? (ws-pl (unbox ws-box))))))
;; unboxes fourth slider to get a delay in frames 0 to 1575 (1/28th of a sec)
(define (get-delay) 
  (inexact->exact (round (* 1575 (slider-value (fourth (filter slider? (ws-pl (unbox ws-box)))))))))
;; save-signal takes in a signal value and its pos in the saved-sig vector
;; the pos is also saved to know where the most recent signal is when drawing
;; number number -> void?
(define (save-signal s pos)
  (begin (vector-set! saved-sig pos s) (vector-set! saved-sig saved-sig-size pos)
         s))

(signal-play 
 (network ()
          [ctr <= frame-ctr];; counter to save signals to saved-sig vector          
          [v-pos = (modulo ctr saved-sig-size)] ;; makes sure the ctr isn't out of bounds
          
          
          [skip = (get-speed)] ;;determines the speed of the song from slider
          ;; counts up in frames and resets after hitting songLength
          [f <= (loop-ctr/variable songlen) skip]          
          [frame = (inexact->exact (round f))]
          
          ;; merge left and right signals
          [r = (rs-ith/right drrr frame)]
          [l = (rs-ith/left drrr frame)]
          [s1 = (/ (+ r l) 2)]
          
          ;; merge left and right of delay signals
          [d-frame = (modulo (+ frame (get-delay)) songlen)]
          [r = (rs-ith/right drrr d-frame)]
          [l = (rs-ith/left drrr d-frame)]
          [s2 = (/ (+ r l) 2)]
          
          [s = (/ (+ s1 s2) 2)]
          ;; adds distortion given slider values
          [d <= distortion (get-cutoff) (get-scale) s]
          
          [out = (save-signal d v-pos)] ;; save signal to saved-sig vector
          ))

(begin (big-bang INITIAL_WORLD
          [name "Network Fun"]
          [to-draw draw-world]
          [on-mouse mouse-handler]
          ; quickly written for testing purposes
          [on-key (lambda (w k)
                    (if (key=? k "r") INITIAL_WORLD
                          w))]) 
       (stop))