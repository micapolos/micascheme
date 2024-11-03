(import (micascheme) (micac run) (micac std) (micac sdl))

(micac-run
  (const int width 352)
  (const int height 288)
  (const int window-scale 2)
  (const int pixel-count (* width height))
  (const int bits-per-pixel 4)
  (const int pixels-size (* pixel-count bits-per-pixel))
  (const int pixels-pitch (* width bits-per-pixel))

  (sdl-init)
  (sdl-create-window window "My window" (* width window-scale) (* height window-scale))
  (sdl-create-renderer renderer window)
  (sdl-create-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)

  (var int scroll 0)
  (allocate-array pixels uint8_t pixels-size)

  (sdl-event-loop
    (begin
      (var int count pixels-size)
      (var uint8_t (* pixel-ref) pixels)
      (var uint8_t value scroll)
      (while count
        (set (pixel-ref *) value)
        (sub count 1)
        (add value 1)
        (add pixel-ref 1)))
    (add scroll 8)
    (sdl-update-texture texture 0 pixels pixels-pitch)
    (sdl-render-copy renderer texture 0 0)
    (sdl-render-present renderer)))
