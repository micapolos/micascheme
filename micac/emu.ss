(library (micac emu)
  (export
    emu video
    width height h-counter v-counter
    mouse-x mouse-y mouse-pressed?
    red green blue
    init update
    file
    run-emu)
  (import
    (micac)
    (micac std)
    (micac sdl)
    (syntax)
    (identifier))

  (micac
    (externs
      video width height h-counter v-counter
      mouse-x mouse-y mouse-pressed?
      red green blue
      init update)

    (macro (file data data-size filename)
      (sdl-file-data data data-size filename))

    (macro
      (emu
        (video width-expr height-expr h-blank-expr v-blank-expr cycles-per-pixel-expr)
        (init init-body ...)
        (update update-body ...))
      (literals video init update)
      (const int width width-expr)
      (const int height height-expr)
      (const int h-blank h-blank-expr)
      (const int v-blank v-blank-expr)
      (const int h-size (+ width h-blank))
      (const int v-size (+ height v-blank))
      (const int cycles-per-pixel cycles-per-pixel-expr)
      (const int frame-cycles (* h-size v-size cycles-per-pixel))
      (const int window-scale 2)

      (var int h-counter 0)
      (var int v-counter 0)
      (var int pixel-cycle-counter 0)

      (var uint8_t red #x00)
      (var uint8_t green #x00)
      (var uint8_t blue #x00)

      (sdl-init)
      (sdl-window window "Emu" (* width window-scale) (* height window-scale))
      (sdl-renderer renderer window)
      (sdl-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)

      (const int pixel-count (* width height))
      (const int bits-per-pixel 4)
      (const int pixels-size (* pixel-count bits-per-pixel))
      (const int pixels-pitch (* width bits-per-pixel))
      (alloc pixels uint8_t pixels-size)
      (var uint8_t (* pixel-ref) pixels)

      (var int mouse-x 0)
      (var int mouse-y 0)
      (var bool mouse-pressed? #f)

      init-body ...
      (sdl-event-loop
        (begin
          (const uint32_t sdl-mouse-state
            (sdl-get-mouse-state mouse-x mouse-y))
          (set mouse-pressed?
            (not (zero? (bitwise-and sdl-mouse-state #b1)))))

        (repeat frame-cycles
          update-body ...

          (when (zero? pixel-cycle-counter)
            (const bool h-video? (< h-counter width))
            (const bool v-video? (< v-counter height))
            (const bool video? (and h-video? v-video?))

            (when video?
              (set (pixel-ref *) #xff) ; alpha
              (inc pixel-ref)

              (set (pixel-ref *) red)
              (inc pixel-ref)

              (set (pixel-ref *) green)
              (inc pixel-ref)

              (set (pixel-ref *) blue)
              (inc pixel-ref)))

          (inc pixel-cycle-counter)
          (when (= pixel-cycle-counter cycles-per-pixel)
            (set pixel-cycle-counter 0)

            (inc h-counter)
            (when (= h-counter h-size)
              (set h-counter 0)

              (inc v-counter)
              (when (= v-counter v-size)
                (set v-counter 0)
                (set pixel-ref pixels)))))

        (sdl-update-texture texture 0 pixels pixels-pitch)
        (sdl-render-copy renderer texture 0 0)
        (sdl-render-present renderer))))

  (define-rule-syntax (run-emu body ...)
    (micac (run (emu body ...))))
)
