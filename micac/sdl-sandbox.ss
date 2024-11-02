(import (micascheme) (micac run) (micac sdl))

(micac-run
  (sdl-init)
  (sdl-create-window window "My window" 704 576)
  (sdl-create-renderer renderer window)
  (sdl-create-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING 352 288)
  (var int scroll 0)
  (var uint8_t (* pixels 405504))
  (sdl-event-loop
    (begin
      (var int count 405504)
      (var uint8_t (* pixel) pixels)
      (var uint8_t value scroll)
      (while count
        (set (pixel *) value)
        (sub count 1)
        (add value 1)
        (add pixel 1)))
    (add scroll 8)
    (sdl-update-texture texture 0 (&ref pixels) 1408)
    (sdl-render-copy renderer texture 0 0)
    (sdl-render-present renderer)))
