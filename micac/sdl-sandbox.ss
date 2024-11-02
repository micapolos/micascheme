(import (micascheme) (micac run) (micac sdl))

(micac-run
  (sdl-init)
  (const int width 352)
  (const int height 288)
  (const int scale 2)
  (const int bpp 4)
  (sdl-create-window window "My window" (* width scale) (* height scale))
  (sdl-create-renderer renderer window)
  (sdl-create-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)
  (var int scroll 0)
  (var uint8_t (* pixels (* width height bpp)))
  (sdl-event-loop
    (begin
      (var int count (* width height bpp))
      (var uint8_t (* pixel) pixels)
      (var uint8_t value scroll)
      (while count
        (set (pixel *) value)
        (sub count 1)
        (add value 1)
        (add pixel 1)))
    (add scroll 8)
    (sdl-update-texture texture 0 (&ref pixels) (* width bpp))
    (sdl-render-copy renderer texture 0 0)
    (sdl-render-present renderer)))
