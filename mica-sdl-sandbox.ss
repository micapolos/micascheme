(import (micascheme) (sdl) (mica-sdl))

(define $flash? #f)
(define $mouse-x 0)
(define $mouse-y 0)

(define $audio-buffer-size 64)
(define $audio-sample 0)
(define $audio-flash? #f)

(define (render-audio $bytevector)
  ;(displayln (format "Sound samples: ~a" (bytevector-length $bytevector)))
  (do!
    (($index 0 (+ $index 1)))
    ((= $index (bytevector-length $bytevector)) (void))
    (bytevector-s8-set! $bytevector $index $audio-sample)
    (set! $audio-sample (+ $audio-sample (if $audio-flash? 2 1)))
    (if (> $audio-sample 127) (set! $audio-sample (- $audio-sample 256)))))

(define (render $renderer)
  ;(displayln (format "Render: ~s" (current-seconds)))
  (if $flash?
    (sdl-set-render-draw-color! $renderer 255 255 255 255)
    (sdl-set-render-draw-color! $renderer 0 0 0 255))
  (sdl-render-clear $renderer)
  (sdl-set-render-draw-color! $renderer 255 0 0 255)
  (sdl-render-fill-rect $renderer (make-sdl-rect 0 0 $mouse-x $mouse-y))
  (sdl-render-present $renderer))

(run-sdl
  SDL-INIT-VIDEO
  SDL-INIT-EVENTS
  SDL-INIT-AUDIO
  (
    (run-sdl-window
      "Test"
      SDL-WINDOWPOS-CENTERED
      SDL-WINDOWPOS-CENTERED
      640 480
      ($window
        (run-sdl-audio-device
          22050
          AUDIO-S8
          1
          $audio-buffer-size
          ($bytevector (render-audio $bytevector))
          ($audio-device
            (run-sdl-renderer
              $window
              -1
              SDL-RENDERER-ACCELERATED
              SDL-RENDERER-PRESENT-VSYNC
              ($renderer
                (sdl-pause-audio-device $audio-device #f)
                (run-sdl-event-loop
                  (cond
                    ((sdl-event-mouse-motion?)
                      (set! $mouse-x (sdl-event-mouse-motion-x))
                      (set! $mouse-y (sdl-event-mouse-motion-y)))
                    ((sdl-event-key-down? SDLK-SPACE)
                      (set! $flash? #t))
                    ((sdl-event-key-up? SDLK-SPACE)
                      (set! $flash? #f))
                    ((sdl-event-none?)
                      (run-sdl-locked-audio-device $audio-device
                        (
                          (set! $audio-flash? $flash?)))
                      (render $renderer))))
                (sdl-pause-audio-device $audio-device #t)))))))))
