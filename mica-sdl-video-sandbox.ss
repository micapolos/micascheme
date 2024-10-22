(import (micascheme) (sdl) (mica-sdl))

(define $flash? #f)
(define $mouse-x 0)
(define $mouse-y 0)

(define (render $renderer $texture $pixels)
  (let ()
    (if $flash?
      (sdl-set-render-draw-color! $renderer 255 255 255 255)
      (sdl-set-render-draw-color! $renderer 0 0 0 255))
    (sdl-update-texture $texture #f (object->reference-address $pixels) (* 640 4))
    (sdl-render-copy $renderer $texture #f #f)
    (sdl-render-fill-rect $renderer (make-sdl-rect 0 0 $mouse-x $mouse-y))
    (sdl-render-present $renderer)))

(run-sdl (SDL-INIT-VIDEO SDL-INIT-EVENTS)
  (run-sdl-window ($window "Test" SDL-WINDOWPOS-CENTERED SDL-WINDOWPOS-CENTERED 640 480)
    (run-sdl-renderer ($renderer $window -1 SDL-RENDERER-ACCELERATED SDL-RENDERER-PRESENT-VSYNC)
      (run-sdl-texture ($texture $renderer SDL-PIXELFORMAT-BGRA8888 SDL-TEXTUREACCESS-STREAMING 640 480)
        (define $pixels (make-immobile-bytevector (* 640 480 4)))
        (do
          (($index 0 (+ $index 1)))
          ((= $index (bytevector-length $pixels)) (void))
          (bytevector-u8-set! $pixels $index (random #xff)))
        (run-sdl-event-loop
          (cond
            ((sdl-event-none?)
              (render $renderer $texture $pixels))
            ((sdl-event-mouse-motion?)
              (set! $mouse-x (sdl-event-mouse-motion-x))
              (set! $mouse-y (sdl-event-mouse-motion-y)))
            ((sdl-event-key-down? SDLK-SPACE)
              (set! $flash? #t))
            ((sdl-event-key-up? SDLK-SPACE)
              (set! $flash? #f))))))))
