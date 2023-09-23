(import (micascheme) (sdl) (mica-sdl))

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
        (define $sample 0)
        (define $flash? #f)
        (define $mouse-x 0)
        (define $mouse-y 0)
        (define $event-counter 0)
        (run-sdl-audio
          22050
          AUDIO-S8
          1
          64
          ($bytevector
            (displayln (format "Sound samples: ~a" (bytevector-length $bytevector)))
            (do!
              (($index 0 (+ $index 1)))
              ((= $index (bytevector-length $bytevector)) (void))
              (bytevector-s8-set! $bytevector $index $sample)
              (set! $sample (+ $sample (if $flash? 2 1)))
              (if (> $sample 127) (set! $sample (- $sample 256)))))
          (
            (run-sdl-renderer
              $window
              -1
              SDL-RENDERER-ACCELERATED
              SDL-RENDERER-PRESENT-VSYNC
              ($renderer
                (SDL_PauseAudio 0)
                (run-sdl-event-loop
                  (displayln (format "Event counter: ~a" $event-counter))
                  (set! $event-counter (+ $event-counter 1))
                  (cond
                    ((sdl-event-mouse-motion?)
                      (set! $mouse-x (sdl-event-mouse-motion-x))
                      (set! $mouse-y (sdl-event-mouse-motion-y)))
                    ((sdl-event-key-down? SDLK-SPACE)
                      (set! $flash? #t))
                    ((sdl-event-key-up? SDLK-SPACE)
                      (set! $flash? #f))
                    ((sdl-event-none?)
                      (displayln (format "Render: ~s" (current-seconds)))
                      (if $flash?
                        (sdl-set-render-draw-color! $renderer 255 255 255 255)
                        (sdl-set-render-draw-color! $renderer 0 0 0 255))
                      (sdl-render-clear $renderer)
                      (sdl-set-render-draw-color! $renderer 255 0 0 255)
                      (sdl-render-fill-rect $renderer (make-sdl-rect 0 0 $mouse-x $mouse-y))
                      (sdl-render-present $renderer))))
                (SDL_PauseAudio 1)))))))))
