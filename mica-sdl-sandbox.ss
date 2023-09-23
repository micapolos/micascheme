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
        (run-sdl-audio
          22050
          AUDIO-S8
          1
          64
          ($bytevector
            (writeln (format "Sound: ~s" (current-seconds)))
            (do!
              (($index 0 (+ $index 1)))
              ((= $index (bytevector-length $bytevector)) (void))
              (bytevector-s8-set! $bytevector $index $sample)
              (set! $sample (if (= $sample 127) 0 (+ $sample 1)))))
          (
            (SDL_PauseAudio 0)
            (run-sdl-renderer
              $window
              -1
              SDL-RENDERER-ACCELERATED
              SDL-RENDERER-PRESENT-VSYNC
              ($renderer
                (define $flash? #f)
                (run-sdl-event-loop
                  (cond
                    ((sdl-event-key-down? SDLK-SPACE)
                      (set! $flash? #t))
                    ((sdl-event-key-up? SDLK-SPACE)
                      (set! $flash? #f))
                    ((sdl-event-none?)
                      (writeln (format "Render: ~s" (current-seconds)))
                      (if $flash?
                        (sdl-set-render-draw-color! $renderer 255 255 255 255)
                        (sdl-set-render-draw-color! $renderer 0 0 0 255))
                      (sdl-render-clear $renderer)
                      (sdl-render-present $renderer))))))))))))
