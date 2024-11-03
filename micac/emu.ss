(library (micac emu)
  (export
    emu video pixels pixels-size width height init update)
  (import
    (micascheme)
    (micac syntax)
    (micac c)
    (micac std)
    (micac sdl))

  (micac-externs video pixels pixels-size width height init update)

  (micac-macro
    (emu
      (video width-expr height-expr)
      (init init-body ...)
      (update update-body ...))
    (literals video init update)
    (const int width width-expr)
    (const int height height-expr)
    (const int window-scale 2)
    (const int pixel-count (* width height))
    (const int bits-per-pixel 4)
    (const int pixels-size (* pixel-count bits-per-pixel))
    (const int pixels-pitch (* width bits-per-pixel))

    (sdl-init)
    (sdl-window window "Emu" (* width window-scale) (* height window-scale))
    (sdl-renderer renderer window)
    (sdl-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)

    (alloc pixels uint8_t pixels-size)
    init-body ...
    (sdl-event-loop
      update-body ...
      (sdl-update-texture texture 0 pixels pixels-pitch)
      (sdl-render-copy renderer texture 0 0)
      (sdl-render-present renderer)))
)
