(library (micac lib emu)
  (export
    emu video
    width height video-x video-y
    mouse-x mouse-y mouse-pressed?
    red green blue
    update
    file
    run-emu)
  (import
    (micac)
    (micac lib std)
    (micac lib sdl)
    (syntax)
    (identifier))

  (micac
    (externs
      video width height video-x video-y
      mouse-x mouse-y mouse-pressed?
      red green blue
      update)

    (macro (file data data-size filename)
      (sdl-file-data data data-size filename))

    (macro
      (emu
        (video width-expr height-expr h-blank-expr v-blank-expr cycles-per-pixel-expr)
        body ...
        (update update-body ...))
      (literals video init update)
      (macro width width-expr)
      (macro height height-expr)
      (macro h-blank h-blank-expr)
      (macro v-blank v-blank-expr)
      (macro h-size (+ width h-blank))
      (macro v-size (+ height v-blank))
      (macro cycles-per-pixel cycles-per-pixel-expr)
      (macro frame-cycles (* h-size v-size cycles-per-pixel))
      (macro window-scale 2)

      (var int video-x 0)
      (var int video-y 0)
      (var int pixel-cycle-counter 0)

      (var uint8_t red #x00)
      (var uint8_t green #x00)
      (var uint8_t blue #x00)

      (sdl-init)
      (sdl-window window "Emu" (* width window-scale) (* height window-scale))
      (sdl-renderer renderer window)
      (sdl-texture texture renderer SDL_PIXELFORMAT_BGRA8888 SDL_TEXTUREACCESS_STREAMING width height)

      (macro pixel-count (* width height))
      (macro bits-per-pixel 4)
      (macro pixels-size (* pixel-count bits-per-pixel))
      (macro pixels-pitch (* width bits-per-pixel))
      (alloc pixels uint8_t pixels-size)
      (var uint8_t (* pixel-ref) pixels)

      (var int mouse-x 0)
      (var int mouse-y 0)
      (var bool mouse-pressed? #f)

      body ...
      (sdl-event-loop
        (var int sdl-mouse-x)
        (var int sdl-mouse-y)
        (const uint32_t sdl-mouse-state (sdl-get-mouse-state sdl-mouse-x sdl-mouse-y))
        (set mouse-x (div sdl-mouse-x window-scale))
        (set mouse-y (div sdl-mouse-y window-scale))
        (set mouse-pressed? (not (zero? (bitwise-and sdl-mouse-state #b1))))

        (repeat frame-cycles
          update-body ...

          (when (zero? pixel-cycle-counter)
            (const bool h-video? (< video-x width))
            (const bool v-video? (< video-y height))
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

            (inc video-x)
            (when (= video-x h-size)
              (set video-x 0)

              (inc video-y)
              (when (= video-y v-size)
                (set video-y 0)
                (set pixel-ref pixels)))))

        (sdl-update-texture texture 0 pixels pixels-pitch)
        (sdl-render-copy renderer texture 0 0)
        (sdl-render-present renderer))))

  (define-rule-syntax (run-emu body ...)
    (micac (run (emu body ...))))
)