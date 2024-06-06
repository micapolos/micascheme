(import (micascheme) (sdl) (mica-sdl))

(define $flash? #f)
(define $mouse-x 0)
(define $mouse-y 0)

(define (render $renderer)
  (let ()
    (if $flash?
      (sdl-set-render-draw-color! $renderer 255 255 255 255)
      (sdl-set-render-draw-color! $renderer 0 0 0 255))
    (sdl-render-clear $renderer)
    (sdl-set-render-draw-color! $renderer 255 0 0 255)
    (sdl-render-fill-rect $renderer (make-sdl-rect 0 0 $mouse-x $mouse-y))
    (sdl-render-present $renderer)))

(run-sdl
  SDL-INIT-VIDEO
  SDL-INIT-EVENTS
  (
    (run-sdl-window
      "Test"
      SDL-WINDOWPOS-CENTERED
      SDL-WINDOWPOS-CENTERED
      640 480
      ($window
        (run-sdl-renderer
          $window
          -1
          SDL-RENDERER-ACCELERATED
          SDL-RENDERER-PRESENT-VSYNC
          ($renderer
            (run-sdl-event-loop
              (cond
                ((sdl-event-none?)
                  (render $renderer))
                ((sdl-event-mouse-motion?)
                  (set! $mouse-x (sdl-event-mouse-motion-x))
                  (set! $mouse-y (sdl-event-mouse-motion-y)))
                ((sdl-event-key-down? SDLK-SPACE)
                  (set! $flash? #t))
                ((sdl-event-key-up? SDLK-SPACE)
                  (set! $flash? #f))))))))))
