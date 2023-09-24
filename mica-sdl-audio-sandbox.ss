(import (micascheme) (sdl) (mica-sdl))

(define $audio-buffer-size 64)
(define $audio-sample 0)
(define $audio-high? #f)

(define $tick? #f)

(define (render-audio $bytevector)
  ;(displayln (format "Sound samples: ~a" (bytevector-length $bytevector)))
  (do!
    (($index 0 (+ $index 1)))
    ((= $index (bytevector-length $bytevector)) (void))
    (bytevector-s8-set! $bytevector $index $audio-sample)
    (set! $audio-sample (+ $audio-sample (if $audio-high? 2 1)))
    (if (> $audio-sample 127) (set! $audio-sample (- $audio-sample 256)))))

(run-sdl
  SDL-INIT-AUDIO
  (
    (run-sdl-audio-device
      22050
      AUDIO-S8
      1
      $audio-buffer-size
      ($bytevector (render-audio $bytevector))
      ($audio-device
        (sdl-pause-audio-device $audio-device #f)
        (do!
          ()
          (#f (void))
          (set! $tick? (not $tick?))
          (run-sdl-locked-audio-device $audio-device
            ((set! $audio-high? $tick?)))
          (void))
        (sdl-pause-audio-device $audio-device #t)))))
