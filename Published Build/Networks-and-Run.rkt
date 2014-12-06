#lang racket
(require lang/posn
         rsound
         2htdp/image
         2htdp/universe
         test-engine/racket-tests)
(require (file "Events-and-Drawing.rkt")
         (file "Structs-Help-Setup.rkt"))

;; unboxes the world's music field to get the current song and song length chosen
(define (get-song) (song-rsound (list-ref SONGS (ws-music (unbox ws-box)))))
(define (get-songlen) (song-length (list-ref SONGS (ws-music (unbox ws-box)))))
;; unboxes the flang? field of the world
(define (get-flang?) (ws-flang? (unbox ws-box)))
;; unboxes first slider value for speeed of playhead
(define (get-speed) (max .5 (* 4 (slider-value 
                                  (first (filter slider? (ws-pl (unbox ws-box))))))))
;; unboxes second and third slider values for distortion control
(define (get-cutoff) (slider-value 
                      (second (filter slider? (ws-pl (unbox ws-box))))))
(define (get-scale) (slider-value 
                     (third (filter slider? (ws-pl (unbox ws-box))))))
;; unboxes fourth slider to get a delay in frames 0 to 4410 (1/10th of a sec)
(define (get-delay) 
  (inexact->exact (round (* 4410 (get-speed) (slider-value 
                                  (fourth (filter slider? (ws-pl (unbox ws-box)))))))))
;; unboxes fifth slider to get the flang frequency ranging from 0.1 to 5
(define (get-flang-freq) 
  (max .1 (* 5 (slider-value 
                (fifth (filter slider? (ws-pl (unbox ws-box))))))))

;; playSong returns a network that grabs the signal for the current song
;; at the frame it is given. It combines the left and right channels.
(define playSong 
  (network (frame)
           ;; f is added to make sure there isn't a bad index
           [f = (modulo frame (get-songlen))]
           [r = (rs-ith/right (get-song) f)]
           [l = (rs-ith/left (get-song) f)]
           [out = (/ (+ r l) 2)]))

;; a version of loop-ctr/variable that allows the song length 
;; to change
(define loop-ctr/variable2 
  (network (songlen skip)
           [f = (+ (prev out 0) skip)]
           [out = (if (< f songlen) f 0)]))

;; distortion returns a network that scales amplitudes
;; above the cut-off by the above-scale value
;; number number -> network 
(define distortion
  (network (cut-off above-scale i) 
    [out = (cond [(> i cut-off) (+ cut-off (* (- i cut-off) above-scale))]
                 [(< i (* -1 cut-off)) (- cut-off (* (+ i cut-off) above-scale))]
                 [else i])]))

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
          ;; counts up in frames and resets after hitting current song's length
          [f <= loop-ctr/variable2 (get-songlen) skip]          
          [frame = (inexact->exact (round f))] ;; makes sure frame is an exact integer
          
          ;; create song's signal at frame
          [s1 <= playSong frame]
          
          ;; sin wave used for oscilating around playhead
          [sin <= sine-wave (get-flang-freq)]
          
          ;; determines if a delay or flang will be used
          [flang? = (if (get-flang?) sin 1)]
          
          ;; create delayed signal at d-frame
          [d-frame = (modulo (+ frame 
                                (inexact->exact (round (* flang? (get-delay))))) 
                             (get-songlen))]
          [s2 <= playSong d-frame]
          
          ;; adds signals together
          [s = (/ (+ s1 s2) 2)]
          
          ;; adds distortion given slider values
          [d <= distortion (get-cutoff) (get-scale) s] 
          
          [out = (save-signal d v-pos)] ;; save signal to saved-sig vector
          ))

;; starts the bigbang that handles all the slider and button controls
;; when closed (stop) is called to stop signal-play
(begin (big-bang INITIAL_WORLD
          [name "Network Fun"]
          [to-draw draw-world]
          [on-mouse mouse-handler]
          ) 
       (stop))

;(test)