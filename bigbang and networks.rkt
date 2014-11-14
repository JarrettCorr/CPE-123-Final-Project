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

(define (get-speed) (max .5 (* 4 (slider-value (first (ws-pl (unbox ws-box)))))))
(define (get-cutoff) (slider-value (second (ws-pl (unbox ws-box)))))
(define (get-scale) (slider-value (third (ws-pl (unbox ws-box)))))

(define song (rs-read "Sounds\\DRR.wav"))
(define songlen (rs-frames song))

(signal-play 
 (network ()
          [skip = (get-speed)]
          [f <= (loop-ctr/variable songlen) skip]
          [frame = (inexact->exact (round f))]
          [r = (rs-ith/right song frame)]
          [l = (rs-ith/left song frame)]
          [a = (/ (+ r l) 2)]
          [d <= distortion (get-cutoff) (get-scale) a]))


(begin (big-bang INITIAL_WORLD
          [to-draw draw-world]
          [on-mouse mouse-handler]
          [on-key (lambda (w k)
                    (if (key=? k "r") INITIAL_WORLD
                          w))]) 
       (stop))