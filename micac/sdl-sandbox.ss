(import (micascheme) (micac run) (micac sdl))

(micac-run
  (sdl-init)
  (sdl-create-window window "My window" 640 480)
  (sdl-create-renderer renderer window)
  (var u8 color 0)
  (sdl-event-loop
    (add color 1)
    (sdl-set-render-draw-color renderer color 0 0 #xff)
    (sdl-render-clear renderer)
    (sdl-render-present renderer)))
