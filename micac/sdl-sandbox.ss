(import
  (micac)
  (micac std)
  (micac sdl))

(micac
  (run
    (macro width 352)
    (macro height 288)
    (macro window-scale 2)
    (macro pixel-count (* width height))
    (macro bits-per-pixel 4)
    (macro pixels-size (* pixel-count bits-per-pixel))
    (macro pixels-pitch (* width bits-per-pixel))

    (sdl-init)
    (sdl-window window "My window" (* width window-scale) (* height window-scale))
    (sdl-renderer renderer window)
    (sdl-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)

    (alloc pixels uint8_t pixels-size)
    (var int frame-counter 0)
    (sdl-event-loop
      (begin
        (var uint8_t (* pixel-ref) pixels)
        (var uint8_t value (* frame-counter 8))
        (repeat pixels-size
          (set (pixel-ref *) value)
          (set pixel-ref + 1)
          (set value + 1)))
      (set frame-counter + 1)
      (sdl-update-texture texture 0 pixels pixels-pitch)
      (sdl-render-copy renderer texture 0 0)
      (sdl-render-present renderer))))
